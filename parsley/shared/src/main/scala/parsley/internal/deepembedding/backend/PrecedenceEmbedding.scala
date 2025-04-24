package parsley.internal.deepembedding.backend

import parsley.expr.Fixity
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{result}
import parsley.internal.machine.instructions

private [deepembedding] final class Precedence[A](table: StrictPrec[A]) extends StrictParsley[A] {
  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = {
    val handler = state.freshLabel()
    result { instrs += new instructions.Label(handler) }
  }

  override private[deepembedding] def inlinable: Boolean = false

  override private[deepembedding] def pretty: String = "precedence" // TODO: implement pretty printing
}