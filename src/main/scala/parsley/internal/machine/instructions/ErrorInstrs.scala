/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.errors.UnexpectDesc
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.{ClassicUnexpectedError, ClassicFancyError}

private [internal] final class RelabelHints(label: String) extends Instr {
    val isHide: Boolean = label.isEmpty
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // if this was a hide, pop the hints if possible
        if (isHide) ctx.popHints
        // EOK
        // replace the head of the hints with the singleton for our label
        else if (ctx.offset == ctx.checkStack.offset) ctx.replaceHint(label)
        // COK
        // do nothing
        ctx.mergeHints()
        ctx.handlers = ctx.handlers.tail
        ctx.checkStack = ctx.checkStack.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RelabelHints($label)"
    // $COVERAGE-ON$
}

private [internal] final class RelabelErrorAndFail(label: String) extends Instr {
    val isHide: Boolean = label.isEmpty
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        ctx.errs.error = ctx.useHints {
            // only use the label if the error message is generated at the same offset
            // as the check stack saved for the start of the `label` combinator.
            ctx.errs.error.label(label, ctx.checkStack.offset)
        }
        ctx.checkStack = ctx.checkStack.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ApplyError($label)"
    // $COVERAGE-ON$
}

private [internal] object ErrorToHints extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.addErrorToHintsAndPop()
        ctx.inc()
    }

    // $COVERAGE-OFF$
    override def toString: String = "ErrorToHints"
    // $COVERAGE-ON$
}

private [internal] object MergeErrorsAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        val err2 = ctx.errs.error
        ctx.errs = ctx.errs.tail
        ctx.errs.error = ctx.errs.error.merge(err2)
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "MergeErrorsAndFail"
    // $COVERAGE-ON$
}

private [internal] class ApplyReasonAndFail(reason: String) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        if (ctx.errs.error.offset == ctx.checkStack.offset) ctx.errs.error = ctx.errs.error.withReason(reason)
        ctx.checkStack = ctx.checkStack.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ApplyReasonAndFail($reason)"
    // $COVERAGE-ON$
}

private [internal] object AmendAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.amend(ctx.states.offset, ctx.states.line, ctx.states.col)
        ctx.states = ctx.states.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "AmendAndFail"
    // $COVERAGE-ON$
}

private [internal] object EntrenchAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.entrench
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "EntrenchAndFail"
    // $COVERAGE-ON$
}

private [internal] object SetLexicalAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.markAsLexical(ctx.checkStack.offset)
        ctx.checkStack = ctx.checkStack.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "SetLexicalAndFail"
    // $COVERAGE-ON$
}

private [internal] final class Fail(width: Int, msgs: String*) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.failWithMessage(width, msgs: _*)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$
}

private [internal] final class Unexpected(msg: String, width: Int) extends Instr {
    private [this] val unexpected = UnexpectDesc(msg, width)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.unexpectedFail(None, unexpected)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Unexpected($msg)"
    // $COVERAGE-ON$
}

private [internal] final class FastFail(msggen: Any => String) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        ctx.handlers = ctx.handlers.tail
        val state = ctx.states
        ctx.states = ctx.states.tail
        ctx.fail(new ClassicFancyError(ctx.offset, state.line, state.col, ctx.offset - state.offset, msggen(x)))
    }
    // $COVERAGE-OFF$
    override def toString: String = "FastFail(?)"
    // $COVERAGE-ON$
}
private [internal] object FastFail {
    def apply[A](msggen: A => String): FastFail = new FastFail(msggen.asInstanceOf[Any => String])
}

private [internal] final class FastUnexpected[A](_namegen: A=>String) extends Instr {
    private [this] def namegen(x: Any, width: Int) = new UnexpectDesc(_namegen(x.asInstanceOf[A]), width)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        ctx.handlers = ctx.handlers.tail
        val state = ctx.states
        ctx.states = ctx.states.tail
        ctx.fail(new ClassicUnexpectedError(ctx.offset, state.line, state.col, None, namegen(x, ctx.offset - state.offset)))
    }
    // $COVERAGE-OFF$
    override def toString: String = "FastUnexpected(?)"
    // $COVERAGE-ON$
}
