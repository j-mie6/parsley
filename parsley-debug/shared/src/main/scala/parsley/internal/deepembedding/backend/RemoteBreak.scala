package parsley.internal.deepembedding.backend

import parsley.debug.*
import parsley.debug.internal.DebugContext
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.frontend.debug.StrictParsleyDebugged
import parsley.internal.machine.instructions.debug.TriggerBreakpoint

private [deepembedding] final class RemoteBreak[A](p: StrictParsley[A], break: Breakpoint) extends StrictParsleyDebugged[A] {
  private var dbgCtx: Option[DebugContext] = None;

  override private [deepembedding] def injectDebugContext(dbgCtx: DebugContext) = this.dbgCtx = Some(dbgCtx)

  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = {
    (dbgCtx, break) match {
      case (Some(dbgCtx), EntryBreak | FullBreak) => instrs += new TriggerBreakpoint(dbgCtx)
      case _ =>
    }
    suspend[M, R, Unit](p.codeGen[M, R](producesResults)) |> {
        (dbgCtx, break) match {
          case (Some(dbgCtx), ExitBreak | FullBreak) => instrs += new TriggerBreakpoint(dbgCtx)
          case _ =>
        }
    }
  }

  override private [deepembedding] def inlinable: Boolean = p.inlinable

  override private [deepembedding] def pretty: String = f"remoteBreak${prettySuffix}(${p.pretty})"

  private def prettySuffix: String = dbgCtx match {
    case Some(_) => ""
    case None => "Inactive"
  }
}
