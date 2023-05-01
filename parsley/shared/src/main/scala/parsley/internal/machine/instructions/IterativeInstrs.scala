/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._

// TODO: Now PushHandlerAndCheck(label, false), so could be removed again!
private [internal] final class PushHandlerIterative(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // This is used for iterative parsers, which must ensure that invalidated hints are invalided _now_
        //ctx.invalidateHints() // FIXME: This has been removed because hint setting in updateCheckOffsetAndHints has been disabled, pending deep thought
        ctx.pushCheck()
        ctx.pushHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerIterative($label)"
    // $COVERAGE-ON$
}

private [internal] final class Many(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            val x = ctx.stack.upop()
            ctx.stack.peek[mutable.ListBuffer[Any]] += x
            ctx.updateCheckOffset()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.exchangeAndContinue(ctx.stack.peek[mutable.ListBuffer[Any]].toList)
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Many($label)"
    // $COVERAGE-ON$
}

private [internal] final class SkipMany(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            ctx.stack.pop_()
            ctx.updateCheckOffset()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.pushAndContinue(())
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SkipMany($label)"
    // $COVERAGE-ON$
}

private [internal] final class ChainPost(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            val op = ctx.stack.pop[Any => Any]()
            ctx.stack.exchange(op(ctx.stack.upeek))
            ctx.updateCheckOffset()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ChainPost($label)"
    // $COVERAGE-ON$
}

private [internal] final class ChainPre(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            val f = ctx.stack.pop[Any => Any]()
            ctx.stack.exchange(f.andThen(ctx.stack.peek[Any => Any]))
            ctx.updateCheckOffset()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ChainPre($label)"
    // $COVERAGE-ON$
}
private [internal] final class Chainl(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            val y = ctx.stack.upop()
            val op = ctx.stack.pop[(Any, Any) => Any]()
            ctx.stack.exchange(op(ctx.stack.peek[Any], y))
            ctx.updateCheckOffset()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Chainl($label)"
    // $COVERAGE-ON$
}

private [instructions] object DualHandler {
    def popSecondHandlerAndJump(ctx: Context, label: Int): Unit = {
        ctx.handlers = ctx.handlers.tail
        ctx.checkStack = ctx.checkStack.tail
        ctx.updateCheckOffset()
        ctx.pc = label
    }
}

private [internal] final class ChainrJump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val f = ctx.stack.pop[(Any, Any) => Any]()
        val x = ctx.stack.upop()
        val acc = ctx.stack.peek[Any => Any]
        // We perform the acc after the tos function; the tos function is "closer" to the final p
        ctx.stack.exchange((y: Any) => acc(f(x, y)))
        DualHandler.popSecondHandlerAndJump(ctx, label)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ChainrJump($label)"
    // $COVERAGE-ON$
}

private [internal] final class ChainrOpHandler(wrap: Any => Any) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            val y = ctx.stack.upop()
            ctx.exchangeAndContinue(ctx.stack.peek[Any => Any](wrap(y)))
        }
        ctx.checkStack = ctx.checkStack.tail
    }

    // $COVERAGE-OFF$
    override def toString: String = "ChainrOpHandler"
    // $COVERAGE-ON$
}
private [internal] object ChainrOpHandler  {
    def apply[A, B](wrap: A => B): ChainrOpHandler = new ChainrOpHandler(wrap.asInstanceOf[Any => Any])
}

private [internal] object ChainrWholeHandler extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.checkStack = ctx.checkStack.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "ChainrWholeHandler"
    // $COVERAGE-ON$
}

private [internal] final class SepEndBy1Jump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.stack.pop_()
        val x = ctx.stack.upop()
        ctx.stack.peek[mutable.ListBuffer[Any]] += x
        DualHandler.popSecondHandlerAndJump(ctx, label)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SepEndBy1Jump($label)"
    // $COVERAGE-ON$
}

private [instructions] object SepEndBy1Handlers {
    def pushAccWhenCheckValidAndContinue(ctx: Context, acc: mutable.ListBuffer[Any]): Unit = {
        if (ctx.offset != ctx.checkStack.offset || acc.isEmpty) ctx.fail()
        else {
            ctx.addErrorToHintsAndPop()
            ctx.good = true
            ctx.exchangeAndContinue(acc.toList)
        }
    }
}

private [internal] object SepEndBy1SepHandler extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        // p succeeded and sep didn't, so push p and fall-through to the whole handler
        val x = ctx.stack.upop()
        val acc = ctx.stack.peek[mutable.ListBuffer[Any]]
        acc += x
        // discard the other handler and increment so that we are sat on the other handler
        assert(ctx.instrs(ctx.pc + 1) eq SepEndBy1WholeHandler, "the next instruction from the sep handler must be the whole handler")
        assert(ctx.handlers.pc == ctx.pc + 1, "the top-most handler must be the whole handler in the sep handler")
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
        SepEndBy1Handlers.pushAccWhenCheckValidAndContinue(ctx, acc)
        ctx.checkStack = ctx.checkStack.tail.tail
    }

    // $COVERAGE-OFF$
    override def toString: String = "SepEndBy1SepHandler"
    // $COVERAGE-ON$
}

private [internal] object SepEndBy1WholeHandler extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        SepEndBy1Handlers.pushAccWhenCheckValidAndContinue(ctx, ctx.stack.peek[mutable.ListBuffer[Any]])
        ctx.checkStack = ctx.checkStack.tail
    }

    // $COVERAGE-OFF$
    override def toString: String = "SepEndBy1WholeHandler"
    // $COVERAGE-ON$
}

private [internal] final class ManyUntil(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            ctx.stack.upop() match {
                case parsley.combinator.ManyUntil.Stop =>
                    ctx.exchangeAndContinue(ctx.stack.peek[mutable.ListBuffer[Any]].toList)
                    ctx.handlers = ctx.handlers.tail
                case x =>
                    ctx.stack.peek[mutable.ListBuffer[Any]] += x
                    ctx.pc = label
            }
        }
        // ManyUntil is a fallthrough handler, it must be visited during failure, but does nothing to the external state
        else ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ManyUntil($label)"
    // $COVERAGE-ON$
}

private [internal] final class SkipManyUntil(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            ctx.stack.upeek match {
                case parsley.combinator.ManyUntil.Stop =>
                    ctx.handlers = ctx.handlers.tail
                    ctx.exchangeAndContinue(())
                case _ =>
                    ctx.pc = label
                    ctx.stack.pop_()
            }
        }
        // ManyUntil is a fallthrough handler, it must be visited during failure, but does nothing to the external state
        else ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ManyUntil($label)"
    // $COVERAGE-ON$
}
