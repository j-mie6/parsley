/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.errors.{CaretWidth, RigidCaret, UnexpectDesc}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.{ClassicExpectedError, ClassicExpectedErrorWithReason, ClassicFancyError, EmptyError}

private [internal] final class RelabelHints(label: String) extends Instr {
    private [this] val isHide: Boolean = label.isEmpty
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // if this was a hide, pop the hints if possible
        if (isHide) ctx.popHints()
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
        ctx.errs.error = ctx.errs.error.withReason(reason, ctx.checkStack.offset)
        ctx.checkStack = ctx.checkStack.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ApplyReasonAndFail($reason)"
    // $COVERAGE-ON$
}

private [internal] class AmendAndFail private (partial: Boolean) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.amend(partial, ctx.states.offset, ctx.states.line, ctx.states.col)
        ctx.states = ctx.states.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "AmendAndFail"
    // $COVERAGE-ON$
}
private [internal] object AmendAndFail {
    private [this] val partial = new AmendAndFail(partial = true)
    private [this] val full = new AmendAndFail(partial = false)
    def apply(partial: Boolean): AmendAndFail = if (partial) this.partial else this.full
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

private [internal] class DislodgeAndFail(n: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.dislodge(n)
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"DislodgeAndFail($n)"
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

private [internal] final class Fail(width: CaretWidth, msgs: String*) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.failWithMessage(width, msgs: _*)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$
}

private [internal] final class Unexpected(msg: String, width: CaretWidth) extends Instr {
    private [this] val unexpected = new UnexpectDesc(msg, width)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.unexpectedFail(None, unexpected)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Unexpected($msg)"
    // $COVERAGE-ON$
}

private [internal] class MakeVerifiedError private (msggen: Either[Any => Seq[String], Option[Any => String]]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val state = ctx.states
        ctx.states = ctx.states.tail
        ctx.restoreHints()
        // A previous success is a failure
        ctx.handlers = ctx.handlers.tail
        val caretWidth = ctx.offset - state.offset
        val x = ctx.stack.upeek
        val err = msggen match {
            case Left(f) => new ClassicFancyError(state.offset, state.line, state.col, new RigidCaret(caretWidth), f(x): _*)
            case Right(Some(f)) => new ClassicExpectedErrorWithReason(state.offset, state.line, state.col, Nil, f(x), caretWidth)
            case Right(None) => new ClassicExpectedError(state.offset, state.line, state.col, Nil, caretWidth)
        }
        ctx.fail(err)
    }
    // $COVERAGE-OFF$
    override def toString: String = "MakeVerifiedError"
    // $COVERAGE-ON$
}
private [internal] object MakeVerifiedError {
    def apply[A](msggen: Either[A => Seq[String], Option[A => String]]): MakeVerifiedError = {
        new MakeVerifiedError(msggen.asInstanceOf[Either[Any => Seq[String], Option[Any => String]]])
    }
}

private [internal] object NoVerifiedError extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        // If a verified error goes wrong, then it should appear like nothing happened
        ctx.restoreState()
        ctx.restoreHints()
        ctx.errs.error = new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = 0)
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "VerifiedErrorHandler"
    // $COVERAGE-ON$
}
