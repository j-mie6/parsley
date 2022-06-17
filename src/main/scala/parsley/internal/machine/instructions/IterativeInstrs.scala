/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.internal.deepembedding
import parsley.internal.machine.{Context, Good}
import parsley.internal.machine.stacks.{HandlerStack, Stack}, Stack.StackExt

private [internal] final class Many(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            val x = ctx.stack.upop()
            ctx.stack.peek[mutable.ListBuffer[Any]] += x
            ctx.updateCheckOffsetAndHints()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else {
            ctx.catchNoConsumed {
                ctx.addErrorToHintsAndPop()
                ctx.exchangeAndContinue(ctx.stack.peek[mutable.ListBuffer[Any]].toList)
            }
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Many($label)"
    // $COVERAGE-ON$
}

private [internal] final class SkipMany(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.stack.pop_()
            ctx.updateCheckOffsetAndHints()
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
        if (ctx.status eq Good) {
            val op = ctx.stack.pop[Any => Any]()
            ctx.stack.exchange(op(ctx.stack.upeek))
            ctx.updateCheckOffsetAndHints()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else {
            ctx.catchNoConsumed {
                ctx.addErrorToHintsAndPop()
                ctx.inc()
            }
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ChainPost($label)"
    // $COVERAGE-ON$
}

private [internal] final class ChainPre(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            val f = ctx.stack.pop[Any => Any]()
            ctx.stack.exchange(f.andThen(ctx.stack.peek[Any => Any]))
            ctx.updateCheckOffsetAndHints()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else {
            ctx.catchNoConsumed {
                ctx.addErrorToHintsAndPop()
                ctx.inc()
            }
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ChainPre($label)"
    // $COVERAGE-ON$
}
private [internal] final class Chainl(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            val y = ctx.stack.upop()
            val op = ctx.stack.pop[(Any, Any) => Any]()
            ctx.stack.exchange(op(ctx.stack.peek, y))
            ctx.updateCheckOffsetAndHints()
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else {
            ctx.catchNoConsumed {
                ctx.addErrorToHintsAndPop()
                ctx.inc()
            }
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Chainl($label)"
    // $COVERAGE-ON$
}

private [instructions] object DualHandler {
    def checkForFirstHandlerAndPop(ctx: Context, otherwise: =>Unit)(action: =>Unit): Unit = {
        if (!ctx.handlers.isEmpty && ctx.handlers.pc == ctx.pc) {
            ctx.handlers = ctx.handlers.tail
            action
        }
        else otherwise
        ctx.checkStack = ctx.checkStack.tail
    }
    def popSecondHandlerAndJump(ctx: Context, label: Int): Unit = {
        ctx.handlers = ctx.handlers.tail
        ctx.checkStack = ctx.checkStack.tail
        ctx.updateCheckOffsetAndHints()
        ctx.pc = label
    }
}
private [internal] final class Chainr[A, B](var label: Int, wrap: Any => Any) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            val f = ctx.stack.pop[(Any, Any) => Any]()
            val x = ctx.stack.upop()
            val acc = ctx.stack.peek[Any => Any]
            // We perform the acc after the tos function; the tos function is "closer" to the final p
            ctx.stack.exchange((y: Any) => acc(f(x, y)))
            DualHandler.popSecondHandlerAndJump(ctx, label)
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else {
            // presence of first handler indicates p succeeded and op didn't
            DualHandler.checkForFirstHandlerAndPop(ctx, ctx.fail()) {
                ctx.catchNoConsumed {
                    ctx.addErrorToHintsAndPop()
                    val y = ctx.stack.upop()
                    ctx.exchangeAndContinue(ctx.stack.peek[Any => Any](wrap(y)))
                }
            }
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Chainr($label)"
    // $COVERAGE-ON$
}
private [internal] object Chainr {
    def apply[A, B](label: Int, wrap: A => B) = new Chainr(label, wrap.asInstanceOf[Any => Any])
}

private [internal] final class SepEndBy1(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.stack.pop_()
            val x = ctx.stack.upop()
            ctx.stack.peek[mutable.ListBuffer[Any]] += x
            DualHandler.popSecondHandlerAndJump(ctx, label)
        }
        else {
            val check = ctx.checkStack.offset
            // this needs to be lazy, because `x` might need to be popped first in the branch below
            lazy val acc = ctx.stack.peek[mutable.ListBuffer[Any]]
            // presence of first handler indicates p succeeded and sep didn't
            DualHandler.checkForFirstHandlerAndPop(ctx, ()) {
                val x = ctx.stack.upop()
                acc += x
                ctx.checkStack = ctx.checkStack.tail
            }
            if (ctx.offset != check || acc.isEmpty) ctx.fail()
            else {
                ctx.addErrorToHintsAndPop()
                ctx.status = Good
                ctx.exchangeAndContinue(acc.toList)
            }
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SepEndBy1($label)"
    // $COVERAGE-ON$
}

private [internal] final class ManyUntil(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
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
