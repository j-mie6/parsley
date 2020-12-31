package parsley.internal.instructions

import Stack.{isEmpty, push}
import parsley.internal.ResizableArray
import parsley.internal.UnsafeOption

private [internal] final class Satisfies(f: Char => Boolean, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) ctx.pushAndContinue(ctx.consumeChar())
        else ctx.fail(expected)
    }
    override def toString: String = "Sat(?)"
}

private [internal] final class Fail(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.failWithMessage(expected, msg)
    override def toString: String = s"Fail($msg)"
}

private [internal] final class Unexpected(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(expected = expected, unexpected = msg)
    override def toString: String = s"Unexpected($msg)"
}

private [internal] object Attempt extends Instr {
    override def apply(ctx: Context): Unit = {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        // Pop input off head then fail to next handler
        else {
            ctx.restoreState()
            ctx.fail()
        }
    }
    override def toString: String = "Attempt"
}

private [internal] object Look extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.restoreState()
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.fail()
        }
    }
    override def toString: String = "Look"
}

// Position Extractors
private [internal] object Line extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.line)
    override def toString: String = "Line"
}

private [internal] object Col extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.col)
    override def toString: String = "Col"
}

// Register-Manipulators
private [internal] final class Get(v: Int) extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.regs(v))
    override def toString: String = s"Get($v)"
}

private [internal] final class Put(v: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.copyOnWrite(v, ctx.stack.peekAndExchange(()))
        ctx.inc()
    }
    override def toString: String = s"Put($v)"
}