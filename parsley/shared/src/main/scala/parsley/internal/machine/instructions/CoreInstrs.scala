/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.EmptyError

// Stack Manipulators
private [internal] final class Push[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Push($x)"
    // $COVERAGE-ON$
}

private [internal] final class Fresh[A](x: =>A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Fresh($x)"
    // $COVERAGE-ON$
}

private [internal] object Pop extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.stack.pop_()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Pop"
    // $COVERAGE-ON$
}

private [internal] object Swap extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val y = ctx.stack.upop()
        val x = ctx.stack.peekAndExchange(y)
        ctx.unsafePushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Swap"
    // $COVERAGE-ON$
}

// Applicative Functors
private [internal] object Apply extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upop()
        val f = ctx.stack.peek[Any => Any]
        ctx.exchangeAndContinue(f(x))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Apply"
    // $COVERAGE-ON$
}

// Monadic
private [internal] final class DynCall(f: Any => Array[Instr]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.call(f(ctx.stack.upop()))
    }
    // $COVERAGE-OFF$
    override def toString: String = "DynCall(?)"
    // $COVERAGE-ON$
}
private [internal] object DynCall {
    def apply[A](f: A => Array[Instr]): DynCall = new DynCall(f.asInstanceOf[Any => Array[Instr]])
}

// Control Flow
private [internal] object Halt extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.running = false
    }
    // $COVERAGE-OFF$
    override def toString: String = "Halt"
    // $COVERAGE-ON$
}

private [internal] final class Call(var label: Int) extends InstrWithLabel {
    private [this] var isSet: Boolean = false
    override def relabel(labels: Array[Int]): this.type = {
        if (!isSet) {
            label = labels(label)
            isSet = true
        }
        this
    }

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.call(label)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Call($label)"
    // $COVERAGE-ON$
}

private [internal] object Return extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.ret()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Return"
    // $COVERAGE-ON$
}

private [internal] final class Empty(width: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.fail(new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = width))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Empty"
    // $COVERAGE-ON$
}
private [internal] object Empty {
    val zero = new Empty(0)
}

private [internal] final class PushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandler($label)"
    // $COVERAGE-ON$
}

private [internal] object PopHandler extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandler"
    // $COVERAGE-ON$
}

private [internal] final class PushHandlerAndState(var label: Int, saveHints: Boolean, hideHints: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        ctx.saveState()
        // FIXME: shadow = !hide so why is it like this?
        if (saveHints) {
            if (!hideHints) ctx.hints = parsley.internal.machine.errors.EmptyHints
        }
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndState($label)"
    // $COVERAGE-ON$
}

private [internal] object PopHandlerAndState extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.states = ctx.states.tail
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandlerAndState"
    // $COVERAGE-ON$
}

private [internal] final class PushHandlerAndCheck(var label: Int, saveHints: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushHandler(label)
        if (saveHints) {
            ctx.hints = parsley.internal.machine.errors.EmptyHints
        }
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndCheck($label)"
    // $COVERAGE-ON$
}

private [internal] final class Jump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Jump($label)"
    // $COVERAGE-ON$
}

private [internal] final class JumpAndPopCheck(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // TODO: should this be mergeHints?
        ctx.handlers = ctx.handlers.tail
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopCheck($label)"
    // $COVERAGE-ON$
}

private [internal] final class JumpAndPopState(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.states = ctx.states.tail
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopState($label)"
    // $COVERAGE-ON$
}

private [internal] final class Catch(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints()
        val check = ctx.handlers.offset
        ctx.handlers = ctx.handlers.tail
        ctx.catchNoConsumed(check) {
            ctx.pushHandler(label)
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Catch($label)"
    // $COVERAGE-ON$
}

private [internal] final class RestoreAndPushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreState()
        ctx.restoreHints()
        ctx.good = true
        ctx.handlers = ctx.handlers.tail
        ctx.pushHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RestoreAndPushHandler($label)"
    // $COVERAGE-ON$
}

private [internal] object Refail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "Refail"
    // $COVERAGE-ON$
}
