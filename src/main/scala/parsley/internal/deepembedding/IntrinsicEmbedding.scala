package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}
import parsley.Reg

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Char](s"char($c)", new CharTok(c, _), instructions.CharTok(c, expected))

private [parsley] final class StringTok(private [StringTok] val s: String, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String](s"string($s)", new StringTok(s, _), instructions.StringTok(s, expected)) {
    override def optimise: Parsley[String] = s match {
        case "" => new Pure("")
        case _ => this
    }
}
// TODO: Perform applicative fusion optimisations
private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, _p: =>Parsley[A], _q: =>Parsley[B])
    extends Binary[A, B, C](_p, _q)((l, r) => s"lift2(f, $l, $r)", Lift2.empty(f))  {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        left.codeGen >>
        right.codeGen |>
        (instrs += new instructions.Lift2(f))
    }
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, _p: =>Parsley[A], _q: =>Parsley[B], _r: =>Parsley[C])
    extends Ternary[A, B, C, D](_p, _q, _r)((f, s, t) => s"lift3(f, $f, $s, $t)", Lift3.empty(f)) {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        first.codeGen >>
        second.codeGen >>
        third.codeGen |>
        (instrs += new instructions.Lift3(f))
    }
}

private [deepembedding] sealed abstract class FilterLike[A, B](_p: =>Parsley[A], pretty: String => String, empty: UnsafeOption[String] => FilterLike[A, B],
                                                               pure: A => Parsley[B], instr: instructions.Instr, pred: A => Boolean)
    extends Unary[A, B](_p)(pretty, empty) {
    final override val numInstrs = 1
    final override def optimise: Parsley[B] = p match {
        case px@Pure(x) => if (pred(x)) px.asInstanceOf[Parsley[B]] else pure(x)
        case z: MZero => z
        case _ => this
    }
    final override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = p.codeGen |> (instrs += instr)
}
private [parsley] final class FastFail[A](_p: =>Parsley[A], msggen: A => String, expected: UnsafeOption[String] = null)
    extends FilterLike[A, Nothing](_p, c => s"$c ! ?", FastFail.empty(msggen, _),
                                   x => new Fail(msggen(x), expected), new instructions.FastFail(msggen, expected), _ => false) with MZero
private [parsley] final class FastUnexpected[A](_p: =>Parsley[A], msggen: A => String, expected: UnsafeOption[String] = null)
    extends FilterLike[A, Nothing](_p, c => s"$c.unexpected(?)", FastFail.empty(msggen, _),
                                   x => new Unexpected(msggen(x), expected), new instructions.FastUnexpected(msggen, expected), _ => false) with MZero
private [parsley] final class Filter[A](_p: =>Parsley[A], pred: A => Boolean, expected: UnsafeOption[String] = null)
    extends FilterLike[A, A](_p, c => s"$c.filter(?)", Filter.empty(pred, _),
                             _ => new Empty(expected), new instructions.Filter(pred, expected), pred)
private [parsley] final class Guard[A](_p: =>Parsley[A], pred: A => Boolean, msg: String, expected: UnsafeOption[String] = null)
    extends FilterLike[A, A](_p, c => s"$c.guard(?, $msg)", Guard.empty(pred, msg, _),
                             _ => new Fail(msg, expected), new instructions.Guard(pred, msg, expected), pred)
private [parsley] final class FastGuard[A](_p: =>Parsley[A], pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String] = null)
    extends FilterLike[A, A](_p, c => s"$c.guard(?, ?)", FastGuard.empty(pred, msggen, _),
                             x => new Fail(msggen(x), expected), new instructions.FastGuard(pred, msggen, expected), pred)


private [parsley] final class If[A](_b: =>Parsley[Boolean], _p: =>Parsley[A], _q: =>Parsley[A])
    extends Ternary[Boolean, A, A, A](_b, _p, _q)((f, s, t) => s"($f ? $s : $t)", If.empty) {
    override val numInstrs = 2
    override def optimise: Parsley[A] = first match {
        case Pure(true) => second
        case Pure(false) => third
        case _ => this
    }
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
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
private [parsley] final class Eof(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Unit]("eof", new Eof(_), new instructions.Eof(expected))

private [parsley] final class Modify[S](val reg: Reg[S], f: S => S)
    extends Singleton[Unit](s"modify($reg, ?)", new instructions.Modify(reg.addr, f)) with UsesRegister
private [parsley] final class Local[S, A](val reg: Reg[S], _p: =>Parsley[S], _q: =>Parsley[A])
    extends Binary[S, A, A](_p, _q)((l, r) => s"local($reg, $l, $r)", Local.empty(reg)) with UsesRegister {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        left.codeGen >> {
            val local = state.freshLabel()
            val body = state.freshLabel()
            instrs += new instructions.Jump(local)
            instrs += new instructions.Label(body)
            right.codeGen |> {
                instrs += new instructions.Label(local)
                instrs += new instructions.Local(body, reg.addr)
            }
        }
    }
}

private [deepembedding] object CharTok {
    def unapply(self: CharTok): Option[Char] = Some(self.c)
}
private [deepembedding] object StringTok {
    def unapply(self: StringTok): Option[String] = Some(self.s)
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
private [deepembedding] object Filter {
    def empty[A](pred: A => Boolean, expected: UnsafeOption[String]): Filter[A] = new Filter(null, pred, expected)
    def apply[A](p: Parsley[A], pred: A => Boolean, expected: UnsafeOption[String]): Filter[A] = empty(pred, expected).ready(p)
}
private [deepembedding] object Guard {
    def empty[A](pred: A => Boolean, msg: String, expected: UnsafeOption[String]): Guard[A] = new Guard(null, pred, msg, expected)
    def apply[A](p: Parsley[A], pred: A => Boolean, msg: String, expected: UnsafeOption[String]): Guard[A] = empty(pred, msg, expected).ready(p)
}
private [deepembedding] object FastGuard {
    def empty[A](pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String]): FastGuard[A] = new FastGuard(null, pred, msggen, expected)
    def apply[A](p: Parsley[A], pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String]): FastGuard[A] = empty(pred, msggen, expected).ready(p)
}
private [deepembedding] object If {
    def empty[A]: If[A] = new If(null, null, null)
    def apply[A](b: Parsley[Boolean], p: Parsley[A], q: Parsley[A]): If[A] = empty.ready(b, p, q)
}
private [deepembedding] object Local {
    def empty[S, A](r: Reg[S]): Local[S, A] = new Local(r, null, null)
    def apply[S, A](r: Reg[S], left: Parsley[S], right: Parsley[A]): Local[S, A] = empty(r).ready(left, right)
}