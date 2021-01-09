package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A](s"pure($x)", new instructions.Push(x))

private [parsley] final class <*>[A, B](_pf: =>Parsley[A => B], _px: =>Parsley[A]) extends Binary[A => B, A, B](_pf, _px)((l, r) => s"($l <*> $r)", <*>.empty) {
    override val numInstrs = 1
    // TODO: Refactor
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
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = left match {
        // pure f <*> p = f <$> p
        case Pure(f) => right match {
            case ct@CharTok(c) => result(instrs += instructions.CharTokFastPerform[Char, B](c, f.asInstanceOf[Char => B], ct.expected))
            case st@StringTok(s) => result(instrs += instructions.StringTokFastPerform(s, f.asInstanceOf[String => B], st.expected))
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

private [parsley] final class >>=[A, B](_p: =>Parsley[A], private [>>=] val f: A => Parsley[B], val expected: UnsafeOption[String] = null)
    extends Unary[A, B](_p)(l => s"($l >>= ?)", >>=.empty(f, _)) {
    override val numInstrs = 1
    override def optimise: Parsley[B] = p match {
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
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        p.codeGen |>
        (instrs += new instructions.DynCall[A](x => f(x).demandCalleeSave().instrs, expected))
    }
}

private [deepembedding] sealed abstract class Seq[A, B](_discard: =>Parsley[A], _result: =>Parsley[B], pretty: String, empty: =>Seq[A, B])
    extends Binary[A, B, B](_discard, _result)((l, r) => s"($l $pretty $r)", empty) {
    final def result: Parsley[B] = right
    final def discard: Parsley[A] = left
    final def result_=(p: Parsley[B]): Unit = right = p
    final def discard_=(p: Parsley[A]): Unit = left = p
    final override val numInstrs = 1
    def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): Seq[A, B_]
    private def buildResult[R](make: (StringTok, Pure[B]) => Parsley[B])(s: String, r: R, ex1: UnsafeOption[String], ex2: UnsafeOption[String]) = {
        make(new StringTok(s, if (ex1 != null) ex1 else ex2), new Pure(r.asInstanceOf[B]))
    }
    private def optimiseStringResult(combine: (String, String) => String, make: (StringTok, Pure[B]) => Parsley[B])
                                    (s: String, ex: UnsafeOption[String]): Parsley[B] = result match {
        case ct@CharTok(c) => buildResult(make)(combine(s, c.toString), c, ex, ct.expected)
        case st@StringTok(t) => buildResult(make)(combine(s, t), t, ex, st.expected)
    }
    final protected def optimiseSeq(combine: (String, String) => String,
                                    make: (StringTok, Pure[B]) => Parsley[B]): PartialFunction[Parsley[A], Parsley[B]] = {
        // pure _ *> p = p = p <* pure _
        case _: Pure[_] => result
        // p *> pure _ *> q = p *> q, p <* (q *> pure _) = p <* q
        case u *> (_: Pure[_]) =>
            discard = u.asInstanceOf[Parsley[A]]
            optimise
        case ct@CharTok(c) if result.isInstanceOf[CharTok] || result.isInstanceOf[StringTok] => optimiseStringResult(combine, make)(c.toString, ct.expected)
        case st@StringTok(s) if result.isInstanceOf[CharTok] || result.isInstanceOf[StringTok] => optimiseStringResult(combine, make)(s, st.expected)
    }
    final protected def codeGenSeq[Cont[_, +_]: ContOps](default: =>Cont[Unit, Unit])(implicit instrs: InstrBuffer,
                                                                                      state: CodeGenState): Cont[Unit, Unit] = (result, discard) match {
        case (Pure(x), ct@CharTok(c)) => ContOps.result(instrs += instructions.CharTokFastPerform[Char, B](c, _ => x, ct.expected))
        case (Pure(x), st@StringTok(s)) => ContOps.result(instrs += instructions.StringTokFastPerform(s, _ => x, st.expected))
        case (Pure(x), st@Satisfy(f)) => ContOps.result(instrs += new instructions.SatisfyExchange(f, x, st.expected))
        case (Pure(x), v) =>
            v.codeGen |>
            (instrs += new instructions.Exchange(x))
        case _ => default
    }
}
private [parsley] final class *>[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Seq[A, B](_p, _q, "*>", *>.empty) {
    def optimiseSeq: Option[Parsley[B]] = optimiseSeq(_ + _, (str, res) => {
        discard = str.asInstanceOf[Parsley[A]]
        result = res
        optimise
    }).lift(discard)
    @tailrec override def optimise: Parsley[B] = optimiseSeq match {
        case Some(p) => p
        case None => discard match {
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
    }
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = codeGenSeq {
        discard.codeGen >> {
            instrs += instructions.Pop
            result.codeGen
        }
    }
    override def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): A *> B_ = *>(prev, next)
}
private [parsley] final class <*[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Seq[B, A](_q, _p, "<*", <*.empty) {
    def optimiseSeq: Option[Parsley[A]] = optimiseSeq((t, s) => s + t, *>.apply).lift(discard)
    @tailrec override def optimise: Parsley[A] =  optimiseSeq match {
        case Some(p) => p
        case None => discard match {
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
    }
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = codeGenSeq {
        result.codeGen >>
        discard.codeGen |>
        (instrs += instructions.Pop)
    }
    override def copy[A_ >: A](prev: Parsley[B], next: Parsley[A_]): <*[A_, B] = <*(next, prev)
}

private [deepembedding] object Pure {
    def unapply[A](self: Pure[A]): Option[A] = Some(self.x)
}
private [deepembedding] object <*> {
    def empty[A, B]: A <*> B = new <*>(null, null)
    def apply[A, B](left: Parsley[A=>B], right: Parsley[A]): <*>[A, B] = empty.ready(left, right)
    def unapply[A, B](self: <*>[A, B]): Option[(Parsley[A=>B], Parsley[A])] = Some((self.left, self.right))
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