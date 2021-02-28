package parsley.internal.machine.instructions

import parsley.internal.machine.{Context, Good, SavePoint}
import parsley.internal.ResizableArray
import parsley.internal.errors.{ErrorItem, Desc}

private [internal] final class Satisfies(f: Char => Boolean, _expected: Option[String]) extends Instr {
    private [this] final val expected = _expected.map(Desc)
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) ctx.pushAndContinue(ctx.consumeChar())
        else ctx.expectedFail(expected)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Sat(?)"
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

private [internal] object Tell extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(new SavePoint(ctx.offset, ctx.line, ctx.col))
    // $COVERAGE-OFF$
    override def toString: String = "Tell"
    // $COVERAGE-ON$
}
private [internal] object Seek extends Instr {
    override def apply(ctx: Context): Unit = {
        val save = ctx.stack.rotAndPop[SavePoint]()
        ctx.restoreState(save)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Seek"
    // $COVERAGE-ON$
}


private [internal] object Look extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.restoreHints()
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

private [parsley] final class CalleeSave(var label: Int, _slots: List[Int]) extends InstrWithLabel with Stateful {
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
