/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.XAssert.*

import parsley.internal.collection.mutable.DoublyLinkedList
import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.frontend, frontend.LetMap
import parsley.internal.deepembedding.singletons.*
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer

// Core Embedding
private [deepembedding] final class <*>[A, B](var left: StrictParsley[A => B], var right: StrictParsley[A]) extends StrictParsley[B] {
    def inlinable: Boolean = false
    // TODO: Refactor
    override def optimise(implicit lets: LetMap): StrictParsley[B] = (left, right) match {
        // Fusion laws
        case (uf, Pure(x)) if (uf.isInstanceOf[Pure[?]] || uf.isInstanceOf[_ <*> _]) => uf match {
            // first position fusion
            case Pure(f) => new Pure(f(x))
            // second position fusion
            case Pure(f: Function1[t, A => B] @unchecked) <*> (uy/*: StrictParsley[t]*/) => // scalastyle:ignore disallow.space.before.token
                left = new Pure((y: t) => f(y)(x)).asInstanceOf[StrictParsley[A => B]]
                right = uy.asInstanceOf[StrictParsley[A]]
                this
            // third position fusion
            case Pure(f: Function1[t, Function1[u, A => B] @unchecked]) <*>
                 (uy/*: StrictParsley[t]*/) <*> // scalastyle:ignore disallow.space.before.token
                 (uz/*: StrictParsley[u]*/) => // scalastyle:ignore disallow.space.before.token
                left = <*>(new Pure((y: t) => (z: u) => f(y)(z)(x)), uy.asInstanceOf[StrictParsley[t]]).asInstanceOf[StrictParsley[A => B]]
                right = uz.asInstanceOf[StrictParsley[A]]
                this
            // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
            case _ =>
                left = new Pure((f: A => B) => f(x)).asInstanceOf[StrictParsley[A => B]]
                right = uf.asInstanceOf[StrictParsley[A]]
                this
        }
        // functor law: fmap f (fmap g p) == fmap (f . g) p where fmap f p = pure f <*> p from applicative
        case (Pure(f), Pure(g: Function[t, A] @unchecked) <*> (u/*: StrictParsley[t]*/)) => // scalastyle:ignore disallow.space.before.token
            left = new Pure(f.compose(g)).asInstanceOf[StrictParsley[A => B]]
            right = u.asInstanceOf[StrictParsley[A]]
            this
        // Fusion for special combinators
        case (Pure(f), ct@CharTok(c, x)) => new CharTok(c, f(x), ct.expected)
        case (Pure(f), ct@SupplementaryCharTok(c, x)) => new SupplementaryCharTok(c, f(x), ct.expected)
        case (Pure(f), ct@StringTok(c, x)) => new StringTok(c, f(x), ct.expected)
        // TODO: functor law with lift2!
        // right absorption law: mzero <*> p = mzero
        case (z: MZero, _) => z
        /* RE-ASSOCIATION LAWS */
        // re-association law 1: (q *> left) <*> right = q *> (left <*> right)
        case (q *> uf, ux) => *>(q, <*>(uf, ux).optimise)
        case (uf, seq: Seq[A] @unchecked) => seq match {
            // re-association law 2: left <*> (right <* q) = (left <*> right) <* q
            case ux <* v => <*(<*>(uf, ux).optimise, v).optimise
            // re-association law 3: p *> pure x = pure x <* p
            // consequence of re-association law 3: left <*> (q *> pure x) = (left <*> pure x) <* q
            case v *> (ux: Pure[?]) => <*(<*>(uf, ux).optimise, v).optimise
            case _ => this
        }
        // consequence of left zero law and monadic definition of <*>, preserving error properties of left
        case (u, z: MZero) => *>(u, z)
        case _ => this
    }
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = left match {
        // pure f <*> p = f <$> p
        case Pure(f) =>
            suspend(right.codeGen[M, R](producesResults)) |> {
                if (producesResults) instrs += instructions.Lift1(f)
            }
        case _ =>
            suspend(left.codeGen[M, R](producesResults)) >>
            suspend(right.codeGen[M, R](producesResults)) |> {
                if (producesResults) instrs += instructions.Apply
            }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"(${left.pretty} <*> ${right.pretty})"
    // $COVERAGE-ON$
}

private [deepembedding] final class >>=[A, B](val p: StrictParsley[A], private [>>=] val f: A => frontend.LazyParsley[B]) extends Unary[A, B] {
    override def optimise(implicit lets: LetMap): StrictParsley[B] = p match {
        case z: MZero => z
        case _ => this
    }
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        suspend(p.codeGen[M, R](producesResults = true)) |> {
            instrs += instructions.DynCall[A] { (x, refsSz) =>
                val q = f(x)
                q.setMinReferenceAllocation(refsSz)
                if (implicitly[ContOps[M]].isStackSafe) q.overflows()
                q.instrs
            }
            // NOTE: this cannot be removed, because `q`'s instructions are cached: there is no way to tell it
            // to not produce results that doesn't stop it working later. Something like `p.flatten ~> p.flatten` will crash.
            if (!producesResults) instrs += instructions.Pop
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"$p.flatMap(?)"
    // $COVERAGE-ON$
}

private [deepembedding] final class Seq[A](private [backend] var before: DoublyLinkedList[StrictParsley[?]],
                                           private [backend] var res: StrictParsley[A],
                                           private [backend] var after: DoublyLinkedList[StrictParsley[?]]) extends StrictParsley[A] {
    def inlinable: Boolean = false

    private def mergeIntoRight(p: StrictParsley[A]): this.type = p match {
        case Seq(rs1, rr, rs2) =>
            before.stealAll(rs1)
            assume(!rr.isInstanceOf[Pure[?]] || rs2.isEmpty, "if rr is pure, then rs2 is empty, which retains normalisation")
            after = rs2
            res = rr
            this
        case _ => this
    }

    private def mergeFromRight(p: Seq[?], into: DoublyLinkedList[StrictParsley[?]]): this.type = {
        into.stealAll(p.before)
        Seq.whenNonPure(p.res, into.addOne(_))
        into.stealAll(p.after)
        this
    }

    private def chooseInto(p: StrictParsley[A]): DoublyLinkedList[StrictParsley[?]] = p match {
        case _: Pure[?] => before
        case _          => after
    }

    // TODO: Get behaves much like pure except for shifting positions
    // TODO: can this be optimised to reduce repeated matching?
    override def optimise(implicit lets: LetMap): StrictParsley[A] = this match {
        // Assume that this is eliminated first, so not other before or afters
        case (_: Pure[?] | _: Get[?] | Line | Col | Offset) **> u => u
        case (p: MZero) **> _ => p
        case u <** (_: Pure[?] | _: Get[?] | Line | Col | Offset) => u
        case (p: MZero) <** _ => p
        case u@Seq(bs1, br, bs2) **> r =>
            bs2.lastOption match {
                case Some(_: MZero) => u
                case _ =>
                    before = bs1
                    Seq.whenNonPure(br, before.addOne(_))
                    before.stealAll(bs2)
                    mergeIntoRight(r)
            }
        case _ **> q => mergeIntoRight(q)
        case u@Seq(rs1, rr, rs2) <** p =>
            rs2.lastOption match {
                case Some(_: MZero) => u
                case _ =>
                    before = rs1
                    res = rr
                    assume(!rr.isInstanceOf[Pure[?]] || rs2.isEmpty, "rs2 is empty when rr is Pure")
                    assume(after.size == 1 && (after.head eq p), "after can only contain just p, which is going to get flattened, so it can be dropped")
                    after = rs2
                    val into = chooseInto(rr)
                    p match {
                        case p: Seq[?] => mergeFromRight(p, into)
                        case p =>
                            into.addOne(p)
                            this
                    }
            }
        case r <** (p: Seq[?]) =>
            assume(after.size == 1 && (after.head eq p), "after can only contain just p, which is going to get flattened, so it can be cleared")
            after.clear()
            mergeFromRight(p, chooseInto(r))
        // shift pure to the right by swapping before and after (before is empty linked list!)
        case (_: Pure[?]) <* _ =>
            assume(before.isEmpty, "empty can reuse before instead of allocating a new list because before is empty")
            val empty = before
            before = after
            after = empty
            this
        case _ => this
    }

    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = res match {
        case Pure(x) =>
            // peephole here involves CharTokFastPerform, StringTokFastPerform, and Exchange
            assume(after.isEmpty, "The pure in question is normalised to the end: if result is pure, after is empty.")
            assume(before.nonEmpty, "before cannot be empty because after is empty")
            val last = before.last
            before.initInPlace()
            suspend(Seq.codeGenMany[M, R](before.iterator)) >> {
                before.addOne(last) // undo the initInPlace, you'll thank me later
                last match {
                    case st@Satisfy(f) if producesResults => result(instrs += new instructions.SatisfyExchange(f, x, st.expected))
                    case _ =>
                        suspend(last.codeGen[M, R](producesResults = false)) |> {
                            if (producesResults) instrs += new instructions.Push(x)
                        }
                }
            }
        case _ =>
            suspend(Seq.codeGenMany[M, R](before.iterator)) >> {
                suspend(res.codeGen[M, R](producesResults)) >> {
                    suspend(Seq.codeGenMany(after.iterator))
                }
            }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = (before.map(_.pretty) ++ (res.pretty :: after.map(_.pretty).toList)).mkString("seq(", ", ", ")")
    // $COVERAGE-ON$
}

private [backend] object Seq {
    def unapply[A](self: Seq[A]): Some[(DoublyLinkedList[StrictParsley[?]], StrictParsley[A], DoublyLinkedList[StrictParsley[?]])] = {
        Some((self.before, self.res, self.after))
    }

    private [Seq] def codeGenMany[M[_, +_]: ContOps, R](it: Iterator[StrictParsley[?]])
                                                       (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        if (it.hasNext) {
            suspend(it.next().codeGen[M, R](producesResults = false)) >> suspend(codeGenMany(it))
        } else result(())
    }

    private [Seq] def whenNonPure(p: StrictParsley[?], f: StrictParsley[?] => Unit): Unit = p match {
        case _: Pure[?] =>
        case p          => f(p)
    }
}

private [backend] object <*> {
    def apply[A, B](left: StrictParsley[A=>B], right: StrictParsley[A]): <*>[A, B] = new <*>[A, B](left, right)
    def unapply[A, B](self: <*>[A, B]): Some[(StrictParsley[A=>B], StrictParsley[A])] = Some((self.left, self.right))
}
private [deepembedding] object *> {
    def apply[A](left: StrictParsley[?], right: StrictParsley[A]): Seq[A] = {
        val before = DoublyLinkedList.empty[StrictParsley[?]]
        before.addOne(left)
        *>(before, right)
    }
    private [backend] def apply[A](before: DoublyLinkedList[StrictParsley[?]], res: StrictParsley[A]): Seq[A] = new Seq(before, res, DoublyLinkedList.empty)
    private [backend] def unapply[A](self: Seq[A]): Option[(DoublyLinkedList[StrictParsley[?]], StrictParsley[A])] = {
        if (self.after.isEmpty) Some((self.before, self.res))
        else None
    }
}
private [backend] object **> {
    private [backend] def unapply[A](self: Seq[A]): Option[(StrictParsley[?], StrictParsley[A])] = *>.unapply(self).collect {
        case (before, res) if self.before.size == 1 => (before.head, res)
    }
}

private [deepembedding]  object <* {
    def apply[A](left: StrictParsley[A], right: StrictParsley[?]): Seq[A] = {
        val after = DoublyLinkedList.empty[StrictParsley[?]]
        after.addOne(right)
        <*(left, after)
    }
    private [backend] def apply[A](res: StrictParsley[A], after: DoublyLinkedList[StrictParsley[?]]): Seq[A] = new Seq(DoublyLinkedList.empty, res, after)
    private [backend] def unapply[A](self: Seq[A]): Option[(StrictParsley[A], DoublyLinkedList[StrictParsley[?]])] = {
        if (self.before.isEmpty) Some((self.res, self.after))
        else None
    }
}

private [backend] object <** {
    private [backend] def unapply[A](self: Seq[A]): Option[(StrictParsley[A], StrictParsley[?])] = <*.unapply(self).collect {
        case (res, after) if after.size == 1 => (res, after.head)
    }
}
