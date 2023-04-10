package parsley.internal.deepembedding.backend.debugger

import parsley.debugger.objects.DebugContext

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.backend.{CodeGenState, StrictParsley, Unary}
import parsley.internal.deepembedding.backend.StrictParsley.InstrBuffer
import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.machine.instructions.Label
import parsley.internal.machine.instructions.debugger.{AddAttempt, EnterParser, LeaveParser}

private [deepembedding] final class Debugged[A]
  (origin: LazyParsley[A], val p: StrictParsley[A])
  (implicit dbgCtx: DebugContext) extends Unary[A, A] {
  override protected[backend] def codeGen[Cont[_, +_] : ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
    val handler = state.freshLabel()

    result(instrs += new EnterParser(handler, origin)) |>
    suspend(p.codeGen[Cont, R]) |>
    (instrs += new Label(handler)) |>
    (instrs += new AddAttempt) |>
    (instrs += new LeaveParser)
  }

  override protected def pretty(p: String): String = s"debugged($p)"
}
