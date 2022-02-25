package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds
import StrictParsley.InstrBuffer

// Core Embedding
private [parsley] abstract class Singleton[A](instr: =>instructions.Instr) extends StrictParsley[A] {
    val size = 1
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += instr)
    }
}

private [deepembedding] abstract class Unary[A, B] extends StrictParsley[B] {
    private [deepembedding] var p: StrictParsley[A]
    protected val childRepeats: Int = 1
    protected val numInstrs: Int
    val size = p.size * childRepeats + numInstrs
}

private [deepembedding] abstract class ScopedUnary[A, B](setup: Int => instructions.Instr, instr: instructions.Instr) extends Unary[A, B] {
    final override val numInstrs = 2
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

private [deepembedding] abstract class Binary[A, B, C] extends StrictParsley[C] {
    var left: StrictParsley[A]
    var right: StrictParsley[B]
    protected val numInstrs: Int
    protected val leftRepeats: Int = 1
    protected val rightRepeats: Int = 1
    val size = left.size * leftRepeats + right.size * rightRepeats + numInstrs
}