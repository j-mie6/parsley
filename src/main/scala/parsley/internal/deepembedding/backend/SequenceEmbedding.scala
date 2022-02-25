package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import StrictParsley.InstrBuffer

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A](new instructions.Push(x))

private [parsley] final class <*>[A, B](var left: StrictParsley[A => B], var right: StrictParsley[A]) extends Binary[A => B, A, B] {
    override val numInstrs = 1
    // TODO: Refactor
    override def optimise: StrictParsley[B] = (left, right) match {
        // Fusion laws
        case (uf, Pure(x)) if uf.isInstanceOf[Pure[_]] || uf.isInstanceOf[_ <*> _] && uf.safe => uf match {
            // first position fusion
            case Pure(f) => new Pure(f(x))
            // second position fusion
            case Pure(f: (T => A => B) @unchecked) <*> (uy: StrictParsley[T]) =>
                left = new Pure((y: T) => f(y)(x))
                right = uy.asInstanceOf[StrictParsley[A]]
                this
            // third position fusion
            case Pure(f: (T => U => A => B) @unchecked) <*> (uy: StrictParsley[T]) <*> (uz: StrictParsley[U]) =>
                left = <*>(new Pure((y: T) => (z: U) => f(y)(z)(x)), uy)
                right = uz.asInstanceOf[StrictParsley[A]]
                this
            // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
            case _ =>
                left = new Pure((f: A => B) => f(x)).asInstanceOf[StrictParsley[A => B]]
                right = uf.asInstanceOf[StrictParsley[A]]
                this
        }
        // functor law: fmap f (fmap g p) == fmap (f . g) p where fmap f p = pure f <*> p from applicative
        case (Pure(f), Pure(g: (T => A) @unchecked) <*> (u: StrictParsley[T])) => <*>(new Pure(f.compose(g)), u)
        // TODO: functor law with lift2!
        // right absorption law: mzero <*> p = mzero
        case (z: MZero, _) => z
        /* RE-ASSOCIATION LAWS */
        // re-association law 1: (q *> left) <*> right = q *> (left <*> right)
        case (q *> uf, ux) => *>(q, <*>(uf, ux).optimise)
        case (uf, seq: Seq[_, _, A @unchecked]) => seq match {
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
            left = new Pure((f: A => B) => f(x)).asInstanceOf[StrictParsley[A => B]]
            right = uf.asInstanceOf[StrictParsley[A]]
            this
        case _ => this
    }
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = left match {
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

private [parsley] final class >>=[A, B](var p: StrictParsley[A], private [>>=] val f: A => parsley.internal.deepembedding.Parsley[B]) extends Unary[A, B] {
    override val numInstrs = 1
    override def optimise: StrictParsley[B] = p match {
        // monad law 1: pure x >>= f = f x
        //case Pure(x) if safe => new Rec(() => f(x), expected)
        // char/string x = char/string x *> pure x and monad law 1
        //case p@CharTok(c) => *>(p, new Rec(() => f(c.asInstanceOf[A]), expected))
        //case p@StringTok(s) => *>(p, new Rec(() => f(s.asInstanceOf[A]), expected))
        // (q *> p) >>= f = q *> (p >>= f)
        //case u *> v => *>(u, >>=(v, f).optimise)
        // monad law 3: (m >>= g) >>= f = m >>= (\x -> g x >>= f) Note: this *could* help if g x ended with a pure, since this would be optimised out!
        //case (m: Parsley[T] @unchecked) >>= (g: (T => A) @unchecked) =>
        //    p = m.asInstanceOf[Parsley[A]]
        //    f = (x: T) => >>=(g(x), f, expected).optimise
        //    this
        // monadplus law (left zero)
        case z: MZero => z
        case _ => this
    }
    // TODO: Make bind generate with expected != None a ErrorLabel instruction
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen |>
        (instrs += new instructions.DynCall[A](x => f(x).demandCalleeSave().instrs))
    }
}

private [deepembedding] sealed abstract class Seq[A, B, Res] extends Binary[A, B, Res] {
    def result: StrictParsley[Res]
    def discard: StrictParsley[_]
    def result_=(p: StrictParsley[Res]): Unit
    def discard_=(p: StrictParsley[_]): Unit
    final override val numInstrs = 1
    // The type parameter R here is for /some/ reason necessary because Scala can't infer that Res =:= Char/String in `optimiseStringResult`...
    private def buildResult[R](s: String, r: R, ex1: Option[String], ex2: Option[String]) = {
        makeSeq(new StringTok(s, if (ex1.nonEmpty) ex1 else ex2), new Pure(r.asInstanceOf[Res]))
    }
    private def optimiseStringResult(s: String, ex: Option[String]): StrictParsley[Res] = result match {
        case ct@CharTok(c) => buildResult(combineSeq(s, c.toString), c, ex, ct.expected)
        case st@StringTok(t) => buildResult(combineSeq(s, t), t, ex, st.expected)
    }
    protected def makeSeq(str: StrictParsley[String], res: Pure[Res]): StrictParsley[Res]
    protected def combineSeq(sdis: String, sres: String): String
    final protected def optimiseSeq: Option[StrictParsley[Res]] = discard match {
        // pure _ *> p = p = p <* pure _
        case _: Pure[_] => Some(result)
        // p *> pure _ *> q = p *> q, p <* (q *> pure _) = p <* q
        case u *> (_: Pure[_]) =>
            discard = u
            Some(this.optimise.asInstanceOf[StrictParsley[Res]])
        case ct@CharTok(c) if result.isInstanceOf[CharTok] || result.isInstanceOf[StringTok] => Some(this.optimiseStringResult(c.toString, ct.expected))
        case st@StringTok(s) if result.isInstanceOf[CharTok] || result.isInstanceOf[StringTok] => Some(this.optimiseStringResult(s, st.expected))
        case _ => None
    }
    final protected def codeGenSeq[Cont[_, +_], R](default: =>Cont[R, Unit])(implicit ops: ContOps[Cont, R], instrs: InstrBuffer,
                                                                                      state: CodeGenState): Cont[R, Unit] = (result, discard) match {
        case (Pure(x), ct@CharTok(c)) => ContOps.result(instrs += instructions.CharTokFastPerform[Char, Res](c, _ => x, ct.expected))
        case (Pure(x), st@StringTok(s)) => ContOps.result(instrs += instructions.StringTokFastPerform(s, _ => x, st.expected))
        case (Pure(x), st@Satisfy(f)) => ContOps.result(instrs += new instructions.SatisfyExchange(f, x, st.expected))
        case (Pure(x), v) =>
            v.codeGen |>
            (instrs += new instructions.Exchange(x))
        case _ => default
    }
}
private [parsley] final class *>[A](var left: StrictParsley[Any], var right: StrictParsley[A]) extends Seq[Any, A, A] {
    override def result: StrictParsley[A] = right
    override def discard: StrictParsley[_] = left
    override def result_=(p: StrictParsley[A]): Unit = right = p
    override def discard_=(p: StrictParsley[_]): Unit = left = p
    def makeSeq(str: StrictParsley[String], res: Pure[A]): StrictParsley[A] = {
        discard = str
        result = res
        optimise
    }
    def combineSeq(sdis: String, sres: String): String = sdis + sres
    @tailrec override def optimise: StrictParsley[A] = optimiseSeq match {
        case Some(p) => p
        case None => discard match {
            // mzero *> p = mzero (left zero and definition of *> in terms of >>=)
            case z: MZero => z
            case u => result match {
                // re-association - normal form of Then chain is to have result at the top of tree
                case v *> w =>
                    discard = *>(u, v).optimiseDefinitelyNotTailRec
                    result = w
                    optimise
                case _ => this
            }
        }
    }
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = codeGenSeq {
        discard.codeGen >> {
            instrs += instructions.Pop
            result.codeGen
        }
    }
}
private [parsley] final class <*[A](var left: StrictParsley[A], var right: StrictParsley[Any]) extends Seq[A, Any, A] {
    override def result: StrictParsley[A] = left
    override def discard: StrictParsley[_] = right
    override def result_=(p: StrictParsley[A]): Unit = left = p
    override def discard_=(p: StrictParsley[_]): Unit = right = p

    def makeSeq(str: StrictParsley[String], res: Pure[A]): StrictParsley[A] = *>(str, res)
    def combineSeq(sdis: String, sres: String): String = sres + sdis
    @tailrec override def optimise: StrictParsley[A] =  optimiseSeq match {
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
                    discard = <*(v, w).optimiseDefinitelyNotTailRec
                    optimise
                case _ => this
            }
        }
    }
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = codeGenSeq {
        result.codeGen >>
        discard.codeGen |>
        (instrs += instructions.Pop)
    }
}

private [deepembedding] object Pure {
    def unapply[A](self: Pure[A]): Some[A] = Some(self.x)
}
private [deepembedding] object <*> {
    def apply[A, B](left: StrictParsley[A=>B], right: StrictParsley[A]): <*>[A, B] = new <*>[A, B](left, right)
    def unapply[A, B](self: <*>[A, B]): Some[(StrictParsley[A=>B], StrictParsley[A])] = Some((self.left, self.right))
}
private [deepembedding] object >>= {
    def apply[A, B](p: StrictParsley[A], f: A => parsley.internal.deepembedding.Parsley[B]): >>=[A, B] = new >>=(p, f)
    def unapply[A, B](self: >>=[A, B]): Some[(StrictParsley[A], A => StrictParsley[B])] = Some((self.p, self.f))
}
private [deepembedding] object Seq {
    def unapply[A, B, Res](self: Seq[A, B, Res]): Some[(StrictParsley[_], StrictParsley[Res])] = Some((self.discard, self.result))
}
private [deepembedding] object *> {
    def apply[A](left: StrictParsley[_], right: StrictParsley[A]): *>[A]= new *>(left, right)
    def unapply[A](self: *>[A]): Some[(StrictParsley[_], StrictParsley[A])] = Some((self.discard, self.result))
}
private [deepembedding] object <* {
    def apply[A](left: StrictParsley[A], right: StrictParsley[_]): <*[A] = new <*(left, right)
    def unapply[A](self: <*[A]): Some[(StrictParsley[A], StrictParsley[_])] = Some((self.result, self.discard))
}