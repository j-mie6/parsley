package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg

import scala.language.higherKinds

private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: Option[String])
    extends Singleton[Char](s"char($c)", instructions.CharTok(c, expected))

private [parsley] final class StringTok(private [StringTok] val s: String, val expected: Option[String])
    extends Singleton[String](s"string($s)", instructions.StringTok(s, expected)) {
    override def optimise: Parsley[String] = s match {
        case "" => new Pure("")
        case _ => this
    }
}
// TODO: Perform applicative fusion optimisations
private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, _p: =>Parsley[A], _q: =>Parsley[B])
    extends Binary[A, B, C](_p, _q, (l, r) => s"lift2(f, $l, $r)", new Lift2(f, ???, ???))  {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R],  instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        left.codeGen >>
        right.codeGen |>
        (instrs += new instructions.Lift2(f))
    }
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, _p: =>Parsley[A], _q: =>Parsley[B], _r: =>Parsley[C])
    extends Ternary[A, B, C, D](_p, _q, _r, (f, s, t) => s"lift3(f, $f, $s, $t)", new Lift3(f, _, ???, ???)) {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        first.codeGen >>
        second.codeGen >>
        third.codeGen |>
        (instrs += new instructions.Lift3(f))
    }
}

private [parsley] object Eof extends Singleton[Unit]("eof", instructions.Eof)

private [parsley] final class Modify[S](val reg: Reg[S], f: S => S)
    extends Singleton[Unit](s"modify($reg, ?)", new instructions.Modify(reg.addr, f)) with UsesRegister
private [parsley] final class Local[S, A](val reg: Reg[S], _p: =>Parsley[S], _q: =>Parsley[A])
    extends Binary[S, A, A](_p, _q, (l, r) => s"local($reg, $l, $r)", new Local(reg, ???, ???)) with UsesRegister {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
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
    def unapply[A, B, C](self: Lift2[A, B, C]): Option[((A, B) => C, Parsley[A], Parsley[B])] = Some((self.f, self.left, self.right))
}
private [deepembedding] object Lift3 {
    def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Option[((A, B, C) => D, Parsley[A], Parsley[B], Parsley[C])] = {
        Some((self.f, self.first, self.second, self.third))
    }
}