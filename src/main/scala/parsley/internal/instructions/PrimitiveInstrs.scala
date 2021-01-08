package parsley.internal.instructions

import Stack.{isEmpty, push}
import parsley.internal.ResizableArray
import parsley.internal.UnsafeOption

private [internal] final class Satisfies(f: Char => Boolean, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) ctx.pushAndContinue(ctx.consumeChar())
        else ctx.fail(expected)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Sat(?)"
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
    // $COVERAGE-OFF$
    override def toString: String = "Attempt"
    // $COVERAGE-ON$
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
    // $COVERAGE-OFF$
    override def toString: String = "Look"
    // $COVERAGE-ON$
}

// Position Extractors
private [internal] object Line extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.line)
    // $COVERAGE-OFF$
    override def toString: String = "Line"
    // $COVERAGE-ON$
}

private [internal] object Col extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.col)
    // $COVERAGE-OFF$
    override def toString: String = "Col"
    // $COVERAGE-ON$
}

// Register-Manipulators
private [internal] final class Get(reg: Int) extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.regs(reg))
    // $COVERAGE-OFF$
    override def toString: String = s"Get($reg)"
    // $COVERAGE-ON$
}

private [internal] final class Put(reg: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.writeReg(reg, ctx.stack.peekAndExchange(()))
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Put($reg)"
    // $COVERAGE-ON$
}

private [parsley] final class CalleeSave(var label: Int, _slots: List[Int]) extends JumpInstr with Stateful {
    private val saveArray = new Array[AnyRef](_slots.length)
    private val slots = _slots.zipWithIndex
    private var inUse = false

    private def save(ctx: Context): Unit = {
        for ((slot, idx) <- slots) {
            saveArray(idx) = ctx.regs(slot)
        }
    }

    private def restore(ctx: Context): Unit = {
        for ((slot, idx) <- slots) {
            ctx.regs(slot) = saveArray(idx)
            saveArray(idx) = null
        }
    }

    private def continue(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.pc = label
        }
        else ctx.fail()
    }

    override def apply(ctx: Context): Unit = {
        // Second-entry, callee-restore and either jump or fail
        if (inUse) {
            restore(ctx)
            inUse = false
            continue(ctx)
        }
        // Entry for the first time, register as a handle, callee-save and inc
        else {
            save(ctx)
            inUse = true
            ctx.pushHandler(ctx.pc)
            ctx.inc()
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"CalleeSave($label)"
    // $COVERAGE-ON$
    override def copy: CalleeSave = new CalleeSave(label, _slots)
}
