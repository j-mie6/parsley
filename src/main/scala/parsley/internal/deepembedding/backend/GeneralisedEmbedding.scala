package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds
import StrictParsley.InstrBuffer

// Core Embedding
private [backend] abstract class Unary[A, B] extends StrictParsley[B] {
    protected val p: StrictParsley[A]
    def inlinable: Boolean = false
}

private [backend] abstract class ScopedUnary[A, B] extends Unary[A, B] {
    def instr: instructions.Instr
    def setup(label: Int): instructions.Instr
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += setup(handler)
        suspend[Cont, R, Unit](p.codeGen) |> {
            instrs += new instructions.Label(handler)
            instrs += instr
        }
    }
}

private [backend] abstract class ScopedUnaryWithState[A, B](doesNotProduceHints: Boolean) extends ScopedUnary[A, B] {
    override def setup(label: Int): instructions.Instr = new instructions.PushHandlerAndState(label, doesNotProduceHints, doesNotProduceHints)
}