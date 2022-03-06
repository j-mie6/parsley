package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import StrictParsley.InstrBuffer

import scala.language.higherKinds

// TODO: Perform applicative fusion optimisations
private [deepembedding] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, val left: StrictParsley[A], val right: StrictParsley[B])
    extends StrictParsley[C] {
    def inlinable = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont],  instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        left.codeGen[Cont, R] >>
        right.codeGen |>
        (instrs += new instructions.Lift2(f))
    }
}
private [deepembedding] final class Lift3[A, B, C, D](val f: (A, B, C) => D, val p: StrictParsley[A], val q: StrictParsley[B], val r: StrictParsley[C])
    extends StrictParsley[D] {
    def inlinable = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen[Cont, R] >>
        q.codeGen >>
        r.codeGen |>
        (instrs += new instructions.Lift3(f))
    }
}

private [deepembedding] final class Local[S, A](reg: Reg[S], left: StrictParsley[S], right: StrictParsley[A]) extends StrictParsley[A] {
    def inlinable = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
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

private [backend] object Lift2 {
    def unapply[A, B, C](self: Lift2[A, B, C]): Option[((A, B) => C, StrictParsley[A], StrictParsley[B])] = Some((self.f, self.left, self.right))
}
private [backend] object Lift3 {
    def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Option[((A, B, C) => D, StrictParsley[A], StrictParsley[B], StrictParsley[C])] = {
        Some((self.f, self.p, self.q, self.r))
    }
}