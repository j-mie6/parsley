package parsley.internal.instructions

import parsley.internal.ResizableArray
import parsley.internal.UnsafeOption

private [internal] final class ApplyError(label: String) extends Instr {
    val isHide: Boolean = label.isEmpty
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            // if this was a hide, pop the hints if possible
            if (isHide) ctx.popHints
            // EOK
            // replace the head of the hints with the singleton for our label
            else if (ctx.offset == ctx.checkStack.head) ctx.replaceHint(label)
            // COK
            // do nothing
            ctx.mergeHints()
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.restoreHints()
            // EERR
            // the top of the error stack is adjusted:
            if (ctx.offset == ctx.checkStack.head) ctx.errs.head = ctx.errs.head match {
                // - if it is a fail, it is left alone
                case err: FailError              => err
                //  - otherwise if this is a hide, the expected set is discarded
                case err: TrivialError if isHide => err.copy(expecteds = Set.empty)
                //  - otherwise expected set is replaced by singleton containing this label
                case err: TrivialError           => err.copy(expecteds = Set(Desc(label)))
            }
            // CERR
            // do nothing
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
            val err2 = ctx.errs.head
            ctx.errs = ctx.errs.tail
            ctx.errs.head = ctx.errs.head.merge(err2)
            ctx.fail()
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"MergeErrors"
    // $COVERAGE-ON$
}

private [internal] final class Fail(msg: String) extends Instr {
    override def apply(ctx: Context): Unit = ctx.failWithMessage(msg)
    // $COVERAGE-OFF$
    override def toString: String = s"Fail($msg)"
    // $COVERAGE-ON$
}

private [internal] final class Unexpected(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(expected = expected, unexpected = msg)
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

private [internal] final class FastUnexpected[A](msggen: A=>String, expected: UnsafeOption[String]) extends Instr {
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(expected = expected, unexpected = msggen_(ctx.stack.upop()))
    // $COVERAGE-OFF$
    override def toString: String = "FastUnexpected(?)"
    // $COVERAGE-ON$
}