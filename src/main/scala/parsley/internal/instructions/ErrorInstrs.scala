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
            ctx.inc()
        }
        else {
            // EERR
            // the top of the error stack is adjusted:
            if (ctx.offset == ctx.checkStack.head) ctx.errs.head = ctx.errs.head match {
                // - if it is a fail, it is left alone
                case err: FailMessage            => err
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

private [internal] final class Fail(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.failWithMessage(expected, msg)
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

private [internal] final class FastFail[A](msggen: A=>String, expected: UnsafeOption[String]) extends Instr {
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit = ctx.failWithMessage(expected, msggen_(ctx.stack.upop()))
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