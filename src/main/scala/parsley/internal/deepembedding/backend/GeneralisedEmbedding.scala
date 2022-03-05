package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds
import StrictParsley.InstrBuffer

// Core Embedding
private [parsley] abstract class Singleton[A](instr: =>instructions.Instr) extends StrictParsley[A] {
    val inlinable = true
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += instr)
    }
}

private [deepembedding] abstract class Unary[A, B] extends StrictParsley[B] {
    private [deepembedding] var p: StrictParsley[A]
    val inlinable = false
}

private [deepembedding] abstract class ScopedUnary[A, B](setup: Int => instructions.Instr, instr: instructions.Instr) extends Unary[A, B] {
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += setup(handler)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instr
        }
    }
}

private [deepembedding] abstract class ScopedUnaryWithState[A, B](doesNotProduceHints: Boolean, instr: instructions.Instr)
    extends ScopedUnary[A, B](new instructions.PushHandlerAndState(_, doesNotProduceHints, doesNotProduceHints), instr)