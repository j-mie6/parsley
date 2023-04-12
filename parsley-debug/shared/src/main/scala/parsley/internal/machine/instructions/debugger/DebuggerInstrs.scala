/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.debugger

import parsley.debugger.objects.DebugContext
import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.machine.Context
import parsley.internal.machine.instructions.{Instr, InstrWithLabel}

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

// Ideally we want to make this instruction extend InstrWithLabel, but I can't do that without
// the compiler complaining.
private [internal] class AddAttemptAndLeave(implicit dbgCtx: DebugContext) extends DebuggerInstr {
  override def apply(ctx: Context): Unit = {
    val prevCheck = ctx.checkStack.offset
    val currentOff = ctx.offset

    // Slice based on current offset to see what a parser has attempted to parse,
    // and the 'good' member should indicate whether the previous parser has succeeded or not.
    // We add 1 to currentOff to see what character caused the parse failure.
    val input = ctx.input.slice(prevCheck, currentOff + 1)
    val success = ctx.good

    dbgCtx.addParseAttempt(input, success)

    // See above.
    dbgCtx.pop()
    ctx.checkStack = ctx.checkStack.tail // Manually pop off our debug checkpoint.

    // Fail if the current context is not good.
    if (ctx.good) {
      ctx.handlers = ctx.handlers.tail
      ctx.inc()
    } else {
      ctx.fail()
    }
  }
}