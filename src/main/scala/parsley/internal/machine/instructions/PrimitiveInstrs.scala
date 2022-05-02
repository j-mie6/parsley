/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.errors.{Desc, ErrorItem}
import parsley.internal.machine.{Context, Good}

private [internal] final class Satisfies(f: Char => Boolean, _expected: Option[String]) extends Instr {
    private [this] final val expected = _expected.map(Desc(_))
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) ctx.pushAndContinue(ctx.consumeChar())
        else ctx.expectedFail(expected)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Sat(?)"
    // $COVERAGE-ON$
}

private [internal] object RestoreAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        // Pop input off head then fail to next handler
        ctx.restoreState()
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "RestoreAndFail"
    // $COVERAGE-ON$
}

private [internal] object RestoreHintsAndState extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.restoreHints()
        ctx.restoreState()
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "RestoreHintsAndState"
    // $COVERAGE-ON$
}

private [internal] object PopStateAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.restoreHints()
        ctx.states = ctx.states.tail
        ctx.fail()
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

private [parsley] final class CalleeSave(var label: Int, slots: List[(Int, Int)], saveArray: Array[AnyRef]) extends InstrWithLabel with Stateful {
    def this(label: Int, slots: List[Int]) = this(label, slots.zipWithIndex, new Array[AnyRef](slots.length))
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
    override def copy: CalleeSave = new CalleeSave(label, slots, new Array[AnyRef](slots.length))
}
