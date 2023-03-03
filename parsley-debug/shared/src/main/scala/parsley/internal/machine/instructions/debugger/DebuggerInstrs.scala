package parsley.internal.machine.instructions.debugger

import parsley.debugger.objects.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.Instr

// Instructions used by the debugger itself.
private [internal] sealed abstract class DebuggerInstr(implicit dbgCtx: DebugContext) extends Instr

private [internal] class EnterParser
  (origin: LazyParsley[_])
  (implicit dbgCtx: DebugContext) extends DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    // I think we can get away with executing this unconditionally.
    dbgCtx.push(origin)
    ctx.pushCheck() // Save our location for inputs.
    ctx.inc()
  }
}

private [internal] class LeaveParser(implicit dbgCtx: DebugContext) extends DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    // See above.
    dbgCtx.pop()
    ctx.checkStack = ctx.checkStack.tail // Manually pop off our debug checkpoint.
    ctx.inc()
  }
}

private [internal] class AddAttempt(implicit dbgCtx: DebugContext) extends DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    val prevCheck = ctx.checkStack.offset
    val currentOff = ctx.offset

    // Slice based on current offset to see what a parser has attempted to parse,
    // and the 'good' member should indicate whether the previous parser has succeeded or not.
    val input = ctx.input.slice(prevCheck, currentOff)
    val success = ctx.good

    dbgCtx.addParseAttempt(input, success)
    ctx.inc()
  }
}