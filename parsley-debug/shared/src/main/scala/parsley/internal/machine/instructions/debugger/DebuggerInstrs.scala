package parsley.internal.machine.instructions.debugger

import parsley.debugger.objects.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.{Instr, InstrWithLabel}
import parsley.internal.machine.stacks.Stack

// Instructions used by the debugger itself.
private [internal] sealed trait DebuggerInstr extends Instr

private [internal] class EnterParser
  (var label: Int, origin: LazyParsley[_])
  (implicit dbgCtx: DebugContext) extends InstrWithLabel with DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    // I think we can get away with executing this unconditionally.
    dbgCtx.push(origin)
    ctx.pushCheck() // Save our location for inputs.
    ctx.pushHandler(label) // Mark the AddAttempt instruction as an exit handler.
    ctx.inc()
  }
}

private [internal] class LeaveParser(implicit dbgCtx: DebugContext) extends DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    // See above.
    dbgCtx.pop()
    ctx.checkStack = ctx.checkStack.tail // Manually pop off our debug checkpoint.

    if (ctx.good) ctx.inc()
    else ctx.fail()
  }
}

// Ideally we want to make this instruction extend InstrWithLabel, but I can't do that without
// the compiler complaining.
private [internal] class AddAttempt(implicit dbgCtx: DebugContext) extends DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    assert(dbgCtx.size > 0, "AddAttempt can only be called if we are within the scope of a parser being debugged")
    assert(ctx.checkStack != Stack.empty, "check stack must not be empty when AddAttempt executes")

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