package parsley.internal.deepembedding.backend

import parsley.internal.machine.instructions.*
import parsley.internal.machine.instructions.debug.DormantBreakpoint
import parsley.debug.Breakpoint

private [deepembedding] final class RemoteBreak[A](val p: StrictParsley[A], break: Breakpoint)
    extends ScopedUnary[A, A] {

  override protected def pretty(p: String): String = s"$p.break($break)"

  override def instr: Instr = new DormantBreakpoint(break)

  override def setup(label: Int): Instr = new PushHandler(label) // ????

  override def handlerLabel(state: CodeGenState): Int = state.getLabel(instr)

  override def instrNeedsLabel: Boolean = false // ????
}
