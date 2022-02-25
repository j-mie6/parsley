package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import StrictParsley.InstrBuffer

import scala.language.higherKinds

private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: Option[String])
    extends Singleton[Char](instructions.CharTok(c, expected))

private [parsley] final class StringTok(private [StringTok] val s: String, val expected: Option[String])
    extends Singleton[String](instructions.StringTok(s, expected)) {
    override def optimise: StrictParsley[String] = s match {
        case "" => new Pure("")
        case _ => this
    }
}
// TODO: Perform applicative fusion optimisations
private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, var left: StrictParsley[A], var right: StrictParsley[B]) extends Binary[A, B, C] {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R],  instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        left.codeGen >>
        right.codeGen |>
        (instrs += new instructions.Lift2(f))
    }
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, var p: StrictParsley[A], var q: StrictParsley[B], var r: StrictParsley[C])
    extends StrictParsley[D] {
    val size = p.size + q.size + r.size + 1
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen >>
        q.codeGen >>
        r.codeGen |>
        (instrs += new instructions.Lift3(f))
    }
}

private [parsley] object Eof extends Singleton[Unit](instructions.Eof)

private [parsley] final class Modify[S](val reg: Reg[S], f: S => S)
    extends Singleton[Unit](new instructions.Modify(reg.addr, f)) with UsesRegister
private [parsley] final class Local[S, A](val reg: Reg[S], var left: StrictParsley[S], var right: StrictParsley[A]) extends Binary[S, A, A] with UsesRegister {
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
    def unapply[A, B, C](self: Lift2[A, B, C]): Option[((A, B) => C, StrictParsley[A], StrictParsley[B])] = Some((self.f, self.left, self.right))
}
private [deepembedding] object Lift3 {
    def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Option[((A, B, C) => D, StrictParsley[A], StrictParsley[B], StrictParsley[C])] = {
        Some((self.f, self.p, self.q, self.r))
    }
}