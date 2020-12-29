package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}
import parsley.{Var, Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A](s"pure($x)", new instructions.Push(x))

private [parsley] final class <*>[A, B](_pf: =>Parsley[A => B], _px: =>Parsley[A]) extends Binary[A => B, A, B](_pf, _px)((l, r) => s"($l <*> $r)", <*>.empty) {
    override val numInstrs = 1
    override def optimise: Parsley[B] = (left, right) match {
        // Fusion laws
        case (uf, Pure(x)) if uf.isInstanceOf[Pure[_]] || uf.isInstanceOf[_ <*> _] && uf.safe => uf match {
            // first position fusion
            case Pure(f) => new Pure(f(x))
            // second position fusion
            case Pure(f: (T => A => B) @unchecked) <*> (uy: Parsley[T]) =>
                left = new Pure((y: T) => f(y)(x))
                right = uy.asInstanceOf[Parsley[A]]
                this
            // third position fusion
            case Pure(f: (T => U => A => B) @unchecked) <*> (uy: Parsley[T]) <*> (uz: Parsley[U]) =>
                left = <*>(new Pure((y: T) => (z: U) => f(y)(z)(x)), uy)
                right = uz.asInstanceOf[Parsley[A]]
                this
            // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
            case _ =>
                left = new Pure((f: A => B) => f(x)).asInstanceOf[Parsley[A => B]]
                right = uf.asInstanceOf[Parsley[A]]
                this
        }
        // functor law: fmap f (fmap g p) == fmap (f . g) p where fmap f p = pure f <*> p from applicative
        case (Pure(f), Pure(g: (T => A) @unchecked) <*> (u: Parsley[T])) => <*>(new Pure(f.compose(g)), u)
        // TODO: functor law with lift2!
        // right absorption law: mzero <*> p = mzero
        case (z: MZero, _) => z
        /* RE-ASSOCIATION LAWS */
        // re-association law 1: (q *> left) <*> right = q *> (left <*> right)
        case (q *> uf, ux) => *>(q, <*>(uf, ux).optimise)
        case (uf, seq: Seq[_, _]) => seq match {
            // re-association law 2: left <*> (right <* q) = (left <*> right) <* q
            case ux <* v => <*(<*>(uf, ux).optimise, v).optimise
            // re-association law 3: p *> pure x = pure x <* p
            // consequence of re-association law 3: left <*> (q *> pure x) = (left <*> pure x) <* q
            case v *> (ux: Pure[_]) => <*(<*>(uf, ux).optimise, v).optimise
            case _ => this
        }
        // consequence of left zero law and monadic definition of <*>, preserving error properties of left
        case (u, z: MZero) => *>(u, z)
        // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
        case (uf, Pure(x)) =>
            left = new Pure((f: A => B) => f(x)).asInstanceOf[Parsley[A => B]]
            right = uf.asInstanceOf[Parsley[A]]
            this
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = left match {
        // pure f <*> p = f <$> p
        case Pure(f) => right match {
            case ct@CharTok(c) => result(instrs += instructions.CharTokFastPerform[Char, B](c, f.asInstanceOf[Char => B], ct.expected))
            case st@StringTok(s) => result(instrs += new instructions.StringTokFastPerform(s, f.asInstanceOf[String => B], st.expected))
            case _ =>
                right.codeGen |>
                (instrs += new instructions.Perform(f))
        }
        case _ =>
            left.codeGen >>
            right.codeGen |>
            (instrs += instructions.Apply)
    }
}
private [parsley] final class <|>[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Binary[A, B, B](_p, _q)((l, r) => s"($l <|> $r)", <|>.empty) {
    override val numInstrs = 3

    override def optimise: Parsley[B] = (left, right) match {
        // left catch law: pure x <|> p = pure x
        case (u: Pure[B @unchecked], _) => u
        // alternative law: empty <|> p = p
        case (e: Empty, v) if e.expected == null => v
        // alternative law: p <|> empty = p
        case (u: Parsley[B @unchecked], e: Empty) if e.expected == null => u
        // associative law: (u <|> v) <|> w = u <|> (v <|> w)
        case ((u: Parsley[T]) <|> (v: Parsley[A]), w) =>
            left = u.asInstanceOf[Parsley[A]]
            right = <|>[A, B](v, w).optimise
            this
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = tablify(this, Nil) match {
        // If the tablified list is single element, that implies that this should be generated as normal!
        case _::Nil => left match {
            case Attempt(u) => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    instrs += new instructions.PushHandler(handler)
                    u.codeGen |> {
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.AlwaysRecoverWith[B](x)
                    }
                case v =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.PushHandler(handler)
                    u.codeGen >> {
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.JumpGoodAttempt(skip)
                        v.codeGen |>
                        (instrs += new instructions.Label(skip))
                    }
            }
            case u => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.InputCheck(handler)
                    u.codeGen |> {
                        instrs += new instructions.JumpGood(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.RecoverWith[B](x)
                        instrs += new instructions.Label(skip)
                    }
                case v =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.InputCheck(handler)
                    u.codeGen >> {
                        instrs += new instructions.JumpGood(skip)
                        instrs += new instructions.Label(handler)
                        instrs += instructions.Catch
                        v.codeGen |>
                        (instrs += new instructions.Label(skip))
                    }
            }
        }
        // In case of None'd list, the codeGen cont continues by codeGenning that p, else we are done for this tree, call cont!
        case tablified =>
            // This list is backwards :)
            val needsDefault = tablified.head._2.isDefined
            val end = state.freshLabel()
            val default = state.freshLabel()
            val (roots, leads, ls, expecteds) = foldTablified(tablified, state, mutable.Map.empty, Nil, Nil, Nil)
            instrs += new instructions.JumpTable(leads, ls, default, expecteds)
            codeGenRoots(roots, ls, end) >> {
                instrs += instructions.Catch //This instruction is reachable as default - 1
                instrs += new instructions.Label(default)
                if (needsDefault) {
                    instrs += new instructions.Empty(null)
                    result(instrs += new instructions.Label(end))
                }
                else {
                    tablified.head._1.codeGen |>
                    (instrs += new instructions.Label(end))
                }
            }
    }
    def codeGenRoots[Cont[_, +_]](roots: List[List[Parsley[_]]], ls: List[Int], end: Int)
                                 (implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = roots match {
        case root::roots_ =>
            instrs += new instructions.Label(ls.head)
            codeGenAlternatives(root) >> {
                instrs += new instructions.JumpGood(end)
                codeGenRoots(roots_, ls.tail, end)
            }
        case Nil => result(())
    }
    def codeGenAlternatives[Cont[_, +_]](alts: List[Parsley[_]])
                                        (implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen
        case Attempt(alt)::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            alt.codeGen >> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.JumpGoodAttempt(skip)
                ops.|>(codeGenAlternatives(alts_), instrs += new instructions.Label(skip))
            }
        case alt::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            alt.codeGen >> {
                instrs += new instructions.JumpGood(skip)
                instrs += new instructions.Label(handler)
                instrs += instructions.Catch
                ops.|>(codeGenAlternatives(alts_), instrs += new instructions.Label(skip))
            }
    }
    @tailrec def foldTablified(tablified: List[(Parsley[_], Option[Parsley[_]])], labelGen: CodeGenState,
                               roots: mutable.Map[Char, List[Parsley[_]]],
                               leads: List[Char],
                               labels: List[Int],
                               expecteds: List[UnsafeOption[String]]):
        (List[List[Parsley[_]]], List[Char], List[Int], List[UnsafeOption[String]]) = tablified match {
        case (_, None)::tablified_ => foldTablified(tablified_, labelGen, roots, leads, labels, expecteds)
        case (root, Some(lead))::tablified_ =>
            val (c, expected) = lead match {
                case ct@CharTok(d) => (d, ct.expected)
                case st@StringTok(s) => (s.head, if (st.expected == null) "\"" + s + "\"" else st.expected)
                case kw@Keyword(k) => (k.head, if (kw.expected == null) k else kw.expected)
                case op@Operator(o) => (o.head, if (op.expected == null) o else op.expected)
                case op@MaxOp(o) => (o.head, if (op.expected == null) o else op.expected)
                case sl: StringLiteral => ('"', if (sl.expected == null) "string" else sl.expected)
                case rs: RawStringLiteral => ('"', if (rs.expected == null) "string" else rs.expected)
            }
            if (roots.contains(c)) {
                roots.update(c, root::roots(c))
                foldTablified(tablified_, labelGen, roots, leads, labelGen.freshLabel() :: labels, expected :: expecteds)
            }
            else {
                roots.update(c, root::Nil)
                foldTablified(tablified_, labelGen, roots, c::leads, labelGen.freshLabel() :: labels, expected :: expecteds)
            }
        case Nil => (leads.map(roots(_)), leads, labels, expecteds)
    }
    @tailrec private def tablable(p: Parsley[_]): Option[Parsley[_]] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case t@(_: CharTok | _: StringTok | _: Keyword | _: StringLiteral | _: RawStringLiteral | _: Operator | _: MaxOp) => Some(t)
        case Attempt(t) => tablable(t)
        case (_: Pure[_]) <*> t => tablable(t)
        case Lift2(_, t, _) => tablable(t)
        case Lift3(_, t, _, _) => tablable(t)
        case t <*> _ => tablable(t)
        case t *> _ => tablable(t)
        case t <* _ => tablable(t)
        case _ => None
    }
    @tailrec private [deepembedding] def tablify(p: Parsley[_], acc: List[(Parsley[_], Option[Parsley[_]])]): List[(Parsley[_], Option[Parsley[_]])] = p match {
        case u <|> v =>
            val leading = tablable(u)
            if (leading.isDefined) tablify(v, (u, leading)::acc)
            else (p, None)::acc
        case _ => (p, tablable(p))::acc
    }
}
private [parsley] final class >>=[A, B](_p: =>Parsley[A], private [>>=] var f: A => Parsley[B], val expected: UnsafeOption[String] = null)
    extends Unary[A, B](_p)(l => s"($l >>= ?)", >>=.empty(f, _)) {
    override val numInstrs = 1
    override def optimise: Parsley[B] = p match {
        // CODO: We need to try and identify the Recs in the optimised binds, so we can remove the call instructions
        // monad law 1: pure x >>= f = f x
        //case Pure(x) if safe => new Rec(() => f(x), expected)
        // char/string x = char/string x *> pure x and monad law 1
        //case p@CharTok(c) => *>(p, new Rec(() => f(c.asInstanceOf[A]), expected))
        //case p@StringTok(s) => *>(p, new Rec(() => f(s.asInstanceOf[A]), expected))
        // (q *> p) >>= f = q *> (p >>= f)
        case u *> v => *>(u, >>=(v, f, expected).optimise)
        // monad law 3: (m >>= g) >>= f = m >>= (\x -> g x >>= f) Note: this *could* help if g x ended with a pure, since this would be optimised out!
        //case (m: Parsley[T] @unchecked) >>= (g: (T => A) @unchecked) =>
        //    p = m.asInstanceOf[Parsley[A]]
        //    f = (x: T) => >>=(g(x), f, expected).optimise
        //    this
        // monadplus law (left zero)
        case z: MZero => z
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.DynCall[A](x => f(x).instrs, expected))
    }
}
private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Char]("satisfy(f)", new Satisfy(f, _), new instructions.Satisfies(f, expected))

private [deepembedding] abstract class Seq[A, B](_discard: =>Parsley[A], _result: =>Parsley[B], pretty: String, empty: =>Seq[A, B])
    extends Binary[A, B, B](_discard, _result)((l, r) => s"($l $pretty $r)", empty) {
    final def result: Parsley[B] = right
    final def discard: Parsley[A] = left
    final def result_=(p: Parsley[B]): Unit = right = p
    final def discard_=(p: Parsley[A]): Unit = left = p
    final override val numInstrs = 1
    def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): Seq[A, B_]
    private def unionUnsafe[A >: Null](x: UnsafeOption[A], y: UnsafeOption[A]): UnsafeOption[A] = if (x != null) x else y
    final protected def optimiseSeq(combine: (String, String) => String,
                                    make: (StringTok, Pure[B]) => Parsley[B]): PartialFunction[Parsley[A], Parsley[B]] = {
        // pure _ *> p = p = p <* pure _
        case _: Pure[_] => result
        // p *> pure _ *> q = p *> q, p <* (q *> pure _) = p <* q
        case u *> (_: Pure[_]) =>
            discard = u.asInstanceOf[Parsley[A]]
            optimise
        case ct1@CharTok(c) if result.isInstanceOf[CharTok] || result.isInstanceOf[StringTok] => result match {
            // char(c) *> char(d) = string(cd) *> pure(d)
            // char(c) <* char(d) = string(cd) *> pure(c)
            case ct2@CharTok(d) => make(new StringTok(combine(c.toString, d.toString), unionUnsafe(ct1.expected, ct2.expected)), new Pure(d.asInstanceOf[B]))
            // char(c) *> string(s) = string(cs) *> pure(s)
            // string(s) <* char(d) = string(sd) *> pure(s)
            case st2@StringTok(s) => make(new StringTok(combine(c.toString, s), unionUnsafe(ct1.expected, st2.expected)), new Pure(s.asInstanceOf[B]))
        }
        case st1@StringTok(s) if result.isInstanceOf[CharTok] || result.isInstanceOf[StringTok] => result match {
            // string(s) *> char(c) = string(sc) *> pure(c)
            // char(c) <* string(s) = string(cs) *> pure(c)
            case ct2@CharTok(c) => make(new StringTok(combine(s, c.toString), unionUnsafe(st1.expected, ct2.expected)), new Pure(c.asInstanceOf[B]))
            // string(s) *> string(t) = string(st) *> pure(t)
            // string(s) <* string(t) = string(st) *> pure(s)
            case st2@StringTok(t) => make(new StringTok(combine(s, t), unionUnsafe(st1.expected, st2.expected)), new Pure(t.asInstanceOf[B]))
        }
    }
    final protected def codeGenSeq[Cont[_, +_]](default: =>Cont[Unit, Unit])(implicit instrs: InstrBuffer,
                                                                                      state: CodeGenState,
                                                                                      ops: ContOps[Cont]): Cont[Unit, Unit] = (result, discard) match {
        case (Pure(x), ct@CharTok(c)) => ops.wrap(instrs += instructions.CharTokFastPerform[Char, B](c, _ => x, ct.expected))
        case (Pure(x), st@StringTok(s)) => ops.wrap(instrs += new instructions.StringTokFastPerform(s, _ => x, st.expected))
        case (Pure(x), st@Satisfy(f)) => ops.wrap(instrs += new instructions.SatisfyExchange(f, x, st.expected))
        case (Pure(x), v) =>
            v.codeGen |>
            (instrs += new instructions.Exchange(x))
        case _ => default
    }
}
private [parsley] final class *>[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Seq[A, B](_p, _q, "*>", *>.empty) {
    val optimiseSeq: PartialFunction[Parsley[A], Parsley[B]] = optimiseSeq(_ + _, (str, res) => {
        discard = str.asInstanceOf[Parsley[A]]
        result = res
        optimise
    })
    @tailrec override def optimise: Parsley[B] = discard match {
        case optimiseSeq(p) => p
        // mzero *> p = mzero (left zero and definition of *> in terms of >>=)
        case z: MZero => z
        case u => result match {
            // re-association - normal form of Then chain is to have result at the top of tree
            case v *> w =>
                discard = *>(u, v).asInstanceOf[Parsley[A]].optimiseDefinitelyNotTailRec
                result = w
                optimise
            case _ => this
        }
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = codeGenSeq {
        discard.codeGen >> {
            instrs += instructions.Pop
            result.codeGen
        }
    }
    override def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): A *> B_ = *>(prev, next)
}
private [parsley] final class <*[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Seq[B, A](_q, _p, "<*", <*.empty) {
    val optimiseSeq: PartialFunction[Parsley[B], Parsley[A]] = optimiseSeq((t, s) => s + t, *>.apply)
    @tailrec override def optimise: Parsley[A] = discard match {
        case optimiseSeq(p) => p
        // p <* mzero = p *> mzero (by preservation of error messages and failure properties) - This moves the pop instruction after the failure
        case z: MZero => *>(result, z)
        case w => result match {
            // re-association law 3: pure x <* p = p *> pure x
            case u: Pure[_] => *>(w, u).optimise
            // mzero <* p = mzero (left zero law and definition of <* in terms of >>=)
            case z: MZero => z
            // re-association - normal form of Prev chain is to have result at the top of tree
            case u <* v =>
                result = u
                discard = <*(v, w).asInstanceOf[Parsley[B]].optimiseDefinitelyNotTailRec
                optimise
            case _ => this
        }
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = codeGenSeq {
        result.codeGen >>
        discard.codeGen |>
        (instrs += instructions.Pop)
    }
    override def copy[A_ >: A](prev: Parsley[B], next: Parsley[A_]): <*[A_, B] = <*(next, prev)
}
private [parsley] final class Attempt[A](_p: =>Parsley[A]) extends Unary[A, A](_p)(c => s"attempt($c)", _ => Attempt.empty) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.PushHandler(handler)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instructions.Attempt
        }
    }
}
private [parsley] final class Look[A](_p: =>Parsley[A]) extends Unary[A, A](_p)(c => s"lookAhead($c)", _ => Look.empty) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.PushHandler(handler)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instructions.Look
        }
    }
}
private [parsley] sealed trait MZero extends Parsley[Nothing]
private [parsley] class Empty(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Nothing]("empty", new Empty(_), new instructions.Empty(expected)) with MZero

private [parsley] final class Fail(private [Fail] val msg: String, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Nothing](s"fail($msg)", new Fail(msg, _), new instructions.Fail(msg, expected)) with MZero

private [parsley] final class Unexpected(private [Unexpected] val msg: String, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Nothing](s"unexpected($msg)", new Unexpected(msg, _), new instructions.Unexpected(msg, expected)) with MZero

private [parsley] final class Rec[A](val p: Parsley[A], val expected: UnsafeOption[String] = null)
    extends SingletonExpect[A](s"rec $p", new Rec(p, _), new instructions.Call(p.instrs, expected))

private [parsley] final class Subroutine[A](_p: =>Parsley[A], val expected: UnsafeOption[String] = null)
    extends Unary[A, A](_p)(c => s"+$c", Subroutine.empty) {
    override val numInstrs = 1
    override val childRepeats = 0

    override def preprocess[Cont[_, +_], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] = {
        val self = if (label == null) this else Subroutine(p, label)
        if (!processed) for (p <- this.p.optimised(seen, sub, null, ops)) yield self.ready(p)
        else result(self)
    }
    override def optimise: Parsley[A] = if (p.size <= 1) p else this // This threshold might need tuning?
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val label = state.getSubLabel(p)
        result(instrs += new instructions.GoSub(label, expected))
    }
}
// Intrinsic Embedding
private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Char](s"char($c)", new CharTok(c, _), instructions.CharTok(c, expected))

private [parsley] final class StringTok(private [StringTok] val s: String, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String](s"string($s)", new StringTok(s, _), new instructions.StringTok(s, expected)) {
    override def optimise: Parsley[String] = s match {
        case "" => new Pure("")
        case _ => this
    }
}
// TODO: Perform applicative fusion optimisations
private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, _p: =>Parsley[A], _q: =>Parsley[B])
    extends Binary[A, B, C](_p, _q)((l, r) => s"lift2(f, $l, $r)", Lift2.empty(f))  {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        left.codeGen >>
        right.codeGen |>
        (instrs += new instructions.Lift2(f))
    }
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, _p: =>Parsley[A], _q: =>Parsley[B], _r: =>Parsley[C])
    extends Ternary[A, B, C, D](_p, _q, _r)((f, s, t) => s"lift3(f, $f, $s, $t)", Lift3.empty(f)) {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        first.codeGen >>
        second.codeGen >>
        third.codeGen |>
        (instrs += new instructions.Lift3(f))
    }
}
private [parsley] final class FastFail[A](_p: =>Parsley[A], private [FastFail] val msggen: A => String, val expected: UnsafeOption[String] = null)
    extends Unary[A, Nothing](_p)(c => s"fastfail($c)", FastFail.empty(msggen, _)) with MZero {
    override val numInstrs = 1
    override def optimise: Parsley[Nothing] = p match {
        case Pure(x) => new Fail(msggen(x))
        case z: MZero => z
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.FastFail(msggen, expected))
    }
}
private [parsley] final class FastUnexpected[A](_p: =>Parsley[A], private [FastUnexpected] val msggen: A => String, val expected: UnsafeOption[String] = null)
    extends Unary[A, Nothing](_p)(c => s"fastunxpected($c)", FastFail.empty(msggen, _)) with  MZero {
    override val numInstrs = 1
    override def optimise: Parsley[Nothing] = p match {
        case Pure(x) => new Unexpected(msggen(x))
        case z: MZero => z
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.FastUnexpected(msggen, expected))
    }
}
private [parsley] final class Ensure[A](_p: =>Parsley[A], private [Ensure] val pred: A => Boolean, val expected: UnsafeOption[String] = null)
    extends Unary[A, A](_p)(c => s"ensure($c, ?)", Ensure.empty(pred, _)) {
    override val numInstrs = 1
    override def optimise: Parsley[A] = p match {
        case px@Pure(x) => if (pred(x)) px else new Empty
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.Ensure(pred, expected))
    }
}
private [parsley] final class Guard[A](_p: =>Parsley[A], private [Guard] val pred: A => Boolean,
                                       private [Guard] val msg: String, val expected: UnsafeOption[String] = null)
    extends Unary[A, A](_p)(c => s"guard($c, $msg)", Guard.empty(pred, msg, _)) {
    override val numInstrs = 1
    override def optimise: Parsley[A] = p match {
        case px@Pure(x) => if (pred(x)) px else new Fail(msg)
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.Guard(pred, msg, expected))
    }
}
private [parsley] final class FastGuard[A](_p: =>Parsley[A], private [FastGuard] val pred: A => Boolean,
                                           private [FastGuard] val msggen: A => String, val expected: UnsafeOption[String] = null)
    extends Unary[A, A](_p)(c => s"guard($c)", FastGuard.empty(pred, msggen, _)) {
    override val numInstrs = 1
    override def optimise: Parsley[A] = p match {
        case px@Pure(x) => if (pred(x)) px else new Fail(msggen(x))
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.FastGuard(pred, msggen, expected))
    }
}
private [parsley] final class Many[A](_p: =>Parsley[A]) extends Unary[A, List[A]](_p)(c => s"many($c)", _ => Many.empty) {
    override val numInstrs = 2
    override def optimise: Parsley[List[A]] = p match {
        case _: Pure[A @unchecked] => throw new Exception("many given parser which consumes no input")
        case _: MZero => new Pure(Nil)
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.Many(body)
        }
    }
}
private [parsley] final class SkipMany[A](_p: =>Parsley[A]) extends Unary[A, Unit](_p)(c => s"skipMany($c)", _ => SkipMany.empty) {
    override val numInstrs = 2
    override def optimise: Parsley[Unit] = p match {
        case _: Pure[A @unchecked] => throw new Exception("skipMany given parser which consumes no input")
        case _: MZero => new Pure(()).asInstanceOf[Parsley[Nothing]]
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.SkipMany(body)
        }
    }
}
private [parsley] final class ChainPost[A](_p: =>Parsley[A], _op: =>Parsley[A => A])
    extends Binary[A, A => A, A](_p, _op)((l, r) => s"chainPost($l, $r)", ChainPost.empty) {
    override val numInstrs = 2
    override def optimise: Parsley[A] = right match {
        case _: Pure[(A => A) @unchecked] => throw new Exception("left chain given parser which consumes no input")
        case _: MZero => left
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPost(body)
            }
        }
    }
}
private [parsley] final class ChainPre[A](_p: =>Parsley[A], _op: =>Parsley[A => A])
    extends Binary[A, A => A, A](_p, _op)((l, r) => s"chainPre($r, $l)", ChainPre.empty) {
    override val numInstrs = 3
    override def optimise: Parsley[A] = right match {
        case _: Pure[(A => A) @unchecked] => throw new Exception("right chain given parser which consumes no input")
        case _: MZero => left
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        right.codeGen >> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ChainPre(body)
            left.codeGen |>
            (instrs += instructions.Apply)
        }
    }
}
private [parsley] final class Chainl[A, B](_p: =>Parsley[A], _op: =>Parsley[(B, A) => B], private [Chainl] val wrap: A => B)
    extends Binary[A, (B, A) => B, B](_p, _op)((l, r) => s"chainl1($l, $r)", Chainl.empty(wrap)) {
    override val numInstrs = 2
    override val leftRepeats = 2
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            right.codeGen >>
            left.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainl(body, wrap)
            }
        }
    }
}
private [parsley] final class Chainr[A, B](_p: =>Parsley[A], _op: =>Parsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](_p, _op)((l, r) => s"chainr1($l, $r)", Chainr.empty(wrap)) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit]= {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainr(body, wrap)
            }
        }
    }
}
private [parsley] final class SepEndBy1[A, B](_p: =>Parsley[A], _sep: =>Parsley[B])
    extends Binary[A, B, List[A]](_p, _sep)((l, r) => s"sepEndBy1($r, $l)", SepEndBy1.empty) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SepEndBy1(body)
            }
        }
    }
}
private [parsley] final class ManyUntil[A](_body: Parsley[Any]) extends Unary[Any, List[A]](_body)(c => s"manyUntil($c)", _ => ManyUntil.empty) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val start = state.freshLabel()
        val loop = state.freshLabel()
        instrs += new instructions.PushFallthrough(loop)
        instrs += new instructions.Label(start)
        p.codeGen |> {
            instrs += new instructions.Label(loop)
            instrs += new instructions.ManyUntil(start)
        }
    }
}
private [parsley] final class If[A](_b: =>Parsley[Boolean], _p: =>Parsley[A], _q: =>Parsley[A])
    extends Ternary[Boolean, A, A, A](_b, _p, _q)((f, s, t) => s"($f ? $s : $t)", If.empty) {
    override val numInstrs = 2
    override def optimise: Parsley[A] = first match {
        case Pure(true) => second
        case Pure(false) => third
        case _ => this
    }
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val success = state.freshLabel()
        val end = state.freshLabel()
        first.codeGen >> {
            instrs += new instructions.If(success)
            third.codeGen >> {
                instrs += new instructions.Jump(end)
                instrs += new instructions.Label(success)
                second.codeGen |>
                (instrs += new instructions.Label(end))
            }
        }
    }
}
private [parsley] final class NotFollowedBy[A](_p: =>Parsley[A], val expected: UnsafeOption[String] = null)
    extends Unary[A, Unit](_p)(c => s"notFollowedBy($c)", NotFollowedBy.empty) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.PushHandler(handler)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.NotFollowedBy(expected)
        }
    }
}
private [parsley] final class Eof(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Unit]("eof", new Eof(_), new instructions.Eof(expected))

private [parsley] object Line extends Singleton[Int]("line", instructions.Line)
private [parsley] object Col extends Singleton[Int]("col", instructions.Col)
private [parsley] final class Get[S](v: Var) extends Singleton[S](s"get($v)", new instructions.Get(v.v))
private [parsley] final class Put[S](private [Put] val v: Var, _p: =>Parsley[S]) extends Unary[S, Unit](_p)(c => s"put($v, $c)", _ => Put.empty(v)) {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.Put(v.v))
    }
}
private [parsley] final class Modify[S](v: Var, f: S => S) extends Singleton[Unit](s"modify($v, ?)", new instructions.Modify(v.v, f))
private [parsley] final class Local[S, A](private [Local] val v: Var, _p: =>Parsley[S], _q: =>Parsley[A])
    extends Binary[S, A, A](_p, _q)((l, r) => s"local($v, $l, $r)", Local.empty(v)) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        left.codeGen >> {
            instrs += new instructions.LocalEntry(v.v)
            right.codeGen |>
            (instrs += new instructions.LocalExit(v.v))
        }
    }
}
private [parsley] final class ErrorRelabel[+A](_p: =>Parsley[A], msg: String) extends Parsley[A] {
    lazy val p = _p
    override def preprocess[Cont[_, +_], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] = {
        if (label == null) p.optimised(seen, sub, msg, ops)
        else p.optimised
    }
    override def findLetsAux[Cont[_, +_]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = p.findLets
    override def optimise: Parsley[A] = throw new Exception("Error relabelling should not be in optimisation!")
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        throw new Exception("Error relabelling should not be in code gen!")
    }
    override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"($c ? $msg)"
}
private [parsley] final class Debug[A](_p: =>Parsley[A], name: String, break: Breakpoint)
    extends Unary[A, A](_p)(identity[String], _ => Debug.empty(name, break)) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.LogBegin(handler, name, (break eq EntryBreak) || (break eq FullBreak))
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.LogEnd(name, (break eq ExitBreak) || (break eq FullBreak))
        }
    }
}

private [deepembedding] object Pure {
    def unapply[A](self: Pure[A]): Option[A] = Some(self.x)
}
private [deepembedding] object <*> {
    def empty[A, B]: A <*> B = new <*>(null, null)
    def apply[A, B](left: Parsley[A=>B], right: Parsley[A]): <*>[A, B] = empty.ready(left, right)
    def unapply[A, B](self: <*>[A, B]): Option[(Parsley[A=>B], Parsley[A])] = Some((self.left, self.right))
}
private [deepembedding] object <|> {
    def empty[A, B]: A <|> B = new <|>(null, null)
    def apply[A, B](left: Parsley[A], right: Parsley[B]): A <|> B = empty.ready(left, right)
    def unapply[A, B](self: A <|> B): Option[(Parsley[A], Parsley[B])] = Some((self.left, self.right))
}
private [deepembedding] object >>= {
    def empty[A, B](f: A => Parsley[B], expected: UnsafeOption[String]): >>=[A, B] = new >>=(null, f, expected)
    def apply[A, B](p: Parsley[A], f: A => Parsley[B], expected: UnsafeOption[String]): >>=[A, B] = empty(f, expected).ready(p)
    def unapply[A, B](self: >>=[A, B]): Option[(Parsley[A], A => Parsley[B])] = Some((self.p, self.f))
}
private [deepembedding] object Seq {
    def unapply[A, B](self: Seq[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.discard, self.result))
}
private [deepembedding] object *> {
    def empty[A, B]: A *> B = new *>(null, null)
    def apply[A, B](left: Parsley[A], right: Parsley[B]): A *> B = empty.ready(left, right)
    def unapply[A, B](self: A *> B): Option[(Parsley[A], Parsley[B])] = Some((self.left, self.right))
}
private [deepembedding] object <* {
    def empty[A, B]: A <* B = new <*(null, null)
    def apply[A, B](left: Parsley[A], right: Parsley[B]): A <* B = empty.ready(right, left)
    def unapply[A, B](self: A <* B): Option[(Parsley[A], Parsley[B])] = Some((self.result, self.discard))
}
private [deepembedding] object Attempt {
    def empty[A]: Attempt[A] = new Attempt(null)
    def apply[A](p: Parsley[A]): Attempt[A] = empty.ready(p)
    def unapply[A](self: Attempt[A]): Option[Parsley[A]] = Some(self.p)
}
private [deepembedding] object Look {
    def empty[A]: Look[A] = new Look(null)
    def apply[A](p: Parsley[A]): Look[A] = empty.ready(p)
}
private [parsley] object Subroutine {
    def empty[A](expected: UnsafeOption[String]): Subroutine[A] = new Subroutine(null, expected)
    def apply[A](p: Parsley[A], expected: UnsafeOption[String]): Subroutine[A] = empty(expected).ready(p)
    def unapply[A](self: Subroutine[A]): Option[Parsley[A]] = Some(self.p)
}
private [deepembedding] object CharTok {
    def unapply(self: CharTok): Option[Char] = Some(self.c)
}
private [deepembedding] object StringTok {
    def unapply(self: StringTok): Option[String] = Some(self.s)
}
private [deepembedding] object Satisfy {
    def unapply(self: Satisfy): Option[Char => Boolean] = Some(self.f)
}
private [deepembedding] object Lift2 {
    def empty[A, B, C](f: (A, B) => C): Lift2[A, B, C] = new Lift2(f, null, null)
    def apply[A, B, C](f: (A, B) => C, left: Parsley[A], right: Parsley[B]): Lift2[A, B, C] = empty(f).ready(left, right)
    def unapply[A, B, C](self: Lift2[A, B, C]): Option[((A, B) => C, Parsley[A], Parsley[B])] = Some((self.f, self.left, self.right))
}
private [deepembedding] object Lift3 {
    def empty[A, B, C, D](f: (A, B, C) => D): Lift3[A, B, C, D] = new Lift3(f, null, null, null)
    def apply[A, B, C, D](f: (A, B, C) => D, p: Parsley[A], q: Parsley[B], r: Parsley[C]): Lift3[A, B, C, D] = empty(f).ready(p, q, r)
    def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Option[((A, B, C) => D, Parsley[A], Parsley[B], Parsley[C])] = {
        Some((self.f, self.first, self.second, self.third))
    }
}
private [deepembedding] object FastFail {
    def empty[A](msggen: A => String, expected: UnsafeOption[String]): FastFail[A] = new FastFail(null, msggen, expected)
    def apply[A](p: Parsley[A], msggen: A => String, expected: UnsafeOption[String]): FastFail[A] = empty(msggen, expected).ready(p)
}
private [deepembedding] object FastUnexpected {
    def empty[A](msggen: A => String, expected: UnsafeOption[String]): FastUnexpected[A] = new FastUnexpected(null, msggen, expected)
    def apply[A](p: Parsley[A], msggen: A => String, expected: UnsafeOption[String]): FastUnexpected[A] = empty(msggen, expected).ready(p)
}
private [deepembedding] object Ensure {
    def empty[A](pred: A => Boolean, expected: UnsafeOption[String]): Ensure[A] = new Ensure(null, pred, expected)
    def apply[A](p: Parsley[A], pred: A => Boolean, expected: UnsafeOption[String]): Ensure[A] = empty(pred, expected).ready(p)
}
private [deepembedding] object Guard {
    def empty[A](pred: A => Boolean, msg: String, expected: UnsafeOption[String]): Guard[A] = new Guard(null, pred, msg, expected)
    def apply[A](p: Parsley[A], pred: A => Boolean, msg: String, expected: UnsafeOption[String]): Guard[A] = empty(pred, msg, expected).ready(p)
}
private [deepembedding] object FastGuard {
    def empty[A](pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String]): FastGuard[A] = new FastGuard(null, pred, msggen, expected)
    def apply[A](p: Parsley[A], pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String]): FastGuard[A] = empty(pred, msggen, expected).ready(p)
}
private [deepembedding] object Many {
    def empty[A]: Many[A] = new Many(null)
    def apply[A](p: Parsley[A]): Many[A] = empty.ready(p)
}
private [deepembedding] object SkipMany {
    def empty[A]: SkipMany[A] = new SkipMany(null)
    def apply[A](p: Parsley[A]): SkipMany[A] = empty.ready(p)
}
private [deepembedding] object ChainPost {
    def empty[A]: ChainPost[A] = new ChainPost(null, null)
    def apply[A](left: Parsley[A], right: Parsley[A => A]): ChainPost[A] = empty.ready(left, right)
}
private [deepembedding] object ChainPre {
    def empty[A]: ChainPre[A] = new ChainPre(null, null)
    def apply[A](left: Parsley[A], right: Parsley[A => A]): ChainPre[A] = empty.ready(left, right)
}
private [deepembedding] object Chainl {
    def empty[A, B](wrap: A => B): Chainl[A, B] = new Chainl(null, null, wrap)
    def apply[A, B](left: Parsley[A], right: Parsley[(B, A) => B], wrap: A => B): Chainl[A, B] = empty(wrap).ready(left, right)
}
private [deepembedding] object Chainr {
    def empty[A, B](wrap: A => B): Chainr[A, B] = new Chainr(null, null, wrap)
    def apply[A, B](left: Parsley[A], right: Parsley[(A, B) => B], wrap: A => B): Chainr[A, B] = empty(wrap).ready(left, right)
}
private [deepembedding] object SepEndBy1 {
    def empty[A, B]: SepEndBy1[A, B] = new SepEndBy1(null, null)
    def apply[A, B](left: Parsley[A], right: Parsley[B]): SepEndBy1[A, B] = empty.ready(left, right)
}
private [parsley] object ManyUntil {
    object Stop
    def empty[A]: ManyUntil[A] = new ManyUntil(null)
    def apply[A](p: Parsley[Any]): ManyUntil[A] = empty.ready(p)
}
private [deepembedding] object If {
    def empty[A]: If[A] = new If(null, null, null)
    def apply[A](b: Parsley[Boolean], p: Parsley[A], q: Parsley[A]): If[A] = empty.ready(b, p, q)
}
private [deepembedding] object NotFollowedBy {
    def empty[A](expected: UnsafeOption[String]): NotFollowedBy[A] = new NotFollowedBy(null, expected)
    def apply[A](p: Parsley[A], expected: UnsafeOption[String]): NotFollowedBy[A] = empty(expected).ready(p)
}
private [deepembedding] object Put {
    def empty[S](v: Var): Put[S] = new Put(v, null)
    def apply[S](v: Var, p: Parsley[S]): Put[S] = empty(v).ready(p)
}
private [deepembedding] object Local {
    def empty[S, A](v: Var): Local[S, A] = new Local(v, null, null)
    def apply[S, A](v: Var, left: Parsley[S], right: Parsley[A]): Local[S, A] = empty(v).ready(left, right)
}
private [deepembedding] object Debug {
    def empty[A](name: String, break: Breakpoint): Debug[A] = new Debug(null, name, break)
    def apply[A](p: Parsley[A], name: String, break: Breakpoint): Debug[A] = empty(name, break).ready(p)
}