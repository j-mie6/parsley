package parsley.internal.machine.instructions

import parsley.internal.machine.{Context, Good}

import parsley.internal.errors.{ErrorItem, Desc}
import parsley.internal.machine.errors.{WithReason, MergedErrors, WithLabel, Entrenched, Amended}

private [internal] final class ApplyError(label: String) extends Instr {
    val isHide: Boolean = label.isEmpty
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            // if this was a hide, pop the hints if possible
            if (isHide) ctx.popHints
            // EOK
            // replace the head of the hints with the singleton for our label
            else if (ctx.offset == ctx.checkStack.offset) ctx.replaceHint(label)
            // COK
            // do nothing
            ctx.mergeHints()
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.restoreHints()
            ctx.errs.error = ctx.useHints {
                // EERR
                // the top of the error stack is adjusted:
                if (ctx.errs.error.offset == ctx.checkStack.offset) WithLabel(ctx.errs.error, label)
                // CERR
                // do nothing
                else ctx.errs.error
            }
            ctx.fail()
        }
        ctx.checkStack = ctx.checkStack.tail
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ApplyError($label)"
    // $COVERAGE-ON$
}

private [internal] object MergeErrors extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.addErrorToHintsAndPop()
            ctx.inc()
        }
        else {
            val err2 = ctx.errs.error
            ctx.errs = ctx.errs.tail
            ctx.errs.error = MergedErrors(ctx.errs.error, err2)
            ctx.fail()
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "MergeErrors"
    // $COVERAGE-ON$
}

private [internal] class ApplyReason(reason: String) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            if (ctx.errs.error.offset == ctx.checkStack.offset) ctx.errs.error = WithReason(ctx.errs.error, reason)
            ctx.fail()
        }
        ctx.checkStack = ctx.checkStack.tail
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ApplyReason($reason)"
    // $COVERAGE-ON$
}

private [internal] object Amend extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.errs.error = Amended(ctx.states.offset, ctx.states.line, ctx.states.col, ctx.errs.error)
            ctx.fail()
        }
        ctx.states = ctx.states.tail
    }

    // $COVERAGE-OFF$
    override def toString: String = "Amend"
    // $COVERAGE-ON$
}

private [internal] object Entrench extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.errs.error = Entrenched(ctx.errs.error)
            ctx.fail()
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "Entrench"
    // $COVERAGE-ON$
}

private [internal] final class Fail(msgs: String*) extends Instr {
    override def apply(ctx: Context): Unit = ctx.failWithMessage(msgs: _*)
    // $COVERAGE-OFF$
    override def toString: String = s"Fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$
}

private [internal] final class Unexpected(msg: String) extends Instr {
    private [this] val unexpected = Desc(msg)
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(None, unexpected)
    // $COVERAGE-OFF$
    override def toString: String = s"Unexpected($msg)"
    // $COVERAGE-ON$
}

private [internal] final class FastFail[A](msggen: A=>String) extends Instr {
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit = ctx.failWithMessage(msggen_(ctx.stack.upop()))
    // $COVERAGE-OFF$
    override def toString: String = "FastFail(?)"
    // $COVERAGE-ON$
}

private [internal] final class FastUnexpected[A](_msggen: A=>String) extends Instr {
    private [this] def msggen(x: Any) = new Desc(_msggen(x.asInstanceOf[A]))
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(expected = None, unexpected = msggen(ctx.stack.upop()))
    // $COVERAGE-OFF$
    override def toString: String = "FastUnexpected(?)"
    // $COVERAGE-ON$
}