/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.XAssert.*
import parsley.internal.machine.Context
import parsley.expr.Fixity.{PrefixTag, PostfixTag, InfixLTag, InfixRTag, InfixNTag}
import scala.annotation.switch
import parsley.internal.machine.stacks.ArrayStack
import scala.annotation.tailrec

private [internal] sealed abstract class ShuntToken {
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit
}

private [internal] final class Atom(val v: Any, val lvl: Int) extends ShuntToken {
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        state.atoms.push(this)
        shunt.gotoPostInfix(ctx, state)
        ctx.refreshState()
    }
}
private [internal] abstract class Operator extends ShuntToken {
    val f: Any
    val fix: Int
    val prec: Int
    def isPostfix: Boolean
    def isInfixNonAssoc: Boolean
}
private [internal] final class PrefixOp(val f: Any => Any, val fix: Int, val prec: Int) extends Operator {
    def isPostfix: Boolean = false
    def isInfixNonAssoc: Boolean = false
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit =  {
        if (state.operators.nonEmpty && prec.compare(state.operators.peek.prec) < 0) {
            // This is a malformed expression
            ctx.handlers = ctx.handlers.tail
            val width = shunt.restoreStateGetWidth(ctx)
            ctx.expectedFail(Nil, width)
        } else {
            state.operators.push(this)
            shunt.gotoPreAtom(ctx, state)
            ctx.refreshState()
        }
    }
}

private [internal] final class PostfixOp(val f: Any => Any, val fix: Int, val prec: Int) extends Operator {
    def isPostfix: Boolean = true
    def isInfixNonAssoc: Boolean = false
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.isPostfix && prec.compare(state.operators.peek.prec) > 0) {
            // This was an unexpected postfix operator
            ctx.handlers = ctx.handlers.tail
            ctx.restoreState()
            shunt.produceResult(ctx)
        } else {
            shunt.reduceWhilePrecGreaterOrEqual(state, prec)
            state.operators.push(this)
            shunt.gotoPostInfix(ctx, state)
            ctx.refreshState()
        }
    }
}

private [internal] final class InfixLOp(val f: (Any, Any) => Any, val fix: Int, val prec: Int) extends Operator {
    def isPostfix: Boolean = false
    def isInfixNonAssoc: Boolean = false
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        shunt.reduceWhilePrecGreaterOrEqual(state, prec)
        state.operators.push(this)
        shunt.gotoPreAtom(ctx, state)
        ctx.refreshState()
    }
}

private [internal] final class InfixROp(val f: (Any, Any) => Any, val fix: Int, val prec: Int) extends Operator {
    def isPostfix: Boolean = false
    def isInfixNonAssoc: Boolean = false
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        shunt.reduceWhilePrecGreater(state, prec)
        state.operators.push(this)
        shunt.gotoPreAtom(ctx, state)
        ctx.refreshState()
    }
}

private [internal] final class InfixNOp(val f: (Any, Any) => Any, val fix: Int, val prec: Int) extends Operator {
    def isPostfix: Boolean = false
    def isInfixNonAssoc: Boolean = true
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        shunt.reduceWhilePrecGreater(state, prec)
        if (state.operators.nonEmpty && state.operators.peek.isInfixNonAssoc && state.operators.peek.prec == prec) {
            // This is a special case in which non-associative operators are chained
            ctx.handlers = ctx.handlers.tail
            val width = shunt.restoreStateGetWidth(ctx)
            ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", width)
        } else {
            state.operators.push(this)
            shunt.gotoPreAtom(ctx, state)
            ctx.refreshState()
        }
    }
}

private [instructions] final class ShuntingYardState(
    val atoms: ArrayStack[Atom],
    val operators: ArrayStack[Operator],
    var failOnNoConsumed: Boolean
)

private [internal] object ShuntingYardState {
  def empty = new ShuntingYardState(new ArrayStack(), new ArrayStack(), true)
}

private [internal] final class Shunt(var prefixAtomLabel: Int, var postfixInfixLabel: Int, wraps: Array[Array[Any => Any]]) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.good) {
            val token = ctx.stack.pop[ShuntToken]()
            val state = ctx.stack.peek[ShuntingYardState]
            ctx.updateCheckOffset()

            token.handle(ctx, state, this)
        }
        else handleBadContext(ctx)
    }

    private final def handleBadContext(ctx: Context): Unit = {
        val handler = ctx.handlers
        ctx.handlers = ctx.handlers.tail
        ctx.states = ctx.states.tail
        if (ctx.offset != handler.check || ctx.stack.peek[ShuntingYardState].failOnNoConsumed) {
            // consumed input and/or prefix/atom choice did not match, hard failure
            ctx.fail()
        } else {
            // The end of the expression has been reached
            ctx.good = true
            ctx.addErrorToHintsAndPop()
            produceResult(ctx)
        }
    }

    final def gotoPreAtom(ctx: Context, state: ShuntingYardState): Unit = {
        state.failOnNoConsumed = true
        ctx.pc = prefixAtomLabel
    }

    final def gotoPostInfix(ctx: Context, state: ShuntingYardState): Unit = {
        state.failOnNoConsumed = false
        ctx.pc = postfixInfixLabel
    }

    final def produceResult(ctx: Context): Unit = {
        val state = ctx.stack.peek[ShuntingYardState]

        reduceAll(state)

        assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")

        val atom = state.atoms.peek[Atom]
        ctx.stack.exchange(wrap(atom.lvl, 0, atom.v))
        ctx.inc()
    }

    final def reduce(state: ShuntingYardState): Unit = {
        val op = state.operators.pop[Operator]()
        val result = (op.fix: @switch) match {
            case InfixLTag =>
                val right = state.atoms.pop[Atom]()
                val left = state.atoms.pop[Atom]()
                op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec, left.v), wrap(right.lvl, op.prec + 1, right.v))
            case InfixRTag =>
                val right = state.atoms.pop[Atom]()
                val left = state.atoms.pop[Atom]()
                op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1, left.v), wrap(right.lvl, op.prec, right.v))
            case InfixNTag =>
                val right = state.atoms.pop[Atom]()
                val left = state.atoms.pop[Atom]()
                op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1, left.v), wrap(right.lvl, op.prec + 1, right.v))
            case PostfixTag | PrefixTag =>
                val input = state.atoms.pop[Atom]()
                op.f.asInstanceOf[Any => Any](wrap(input.lvl, op.prec, input.v))
        }
        state.atoms.upush(new Atom(result, op.prec))
    }

    @tailrec
    final def reduceAll(state: ShuntingYardState): Unit = if (state.operators.nonEmpty) {
        reduce(state)
        reduceAll(state)
    }

    @tailrec
    final def reduceWhilePrecGreater(state: ShuntingYardState, prec: Int): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) > 0) {
            reduce(state)
            reduceWhilePrecGreater(state, prec)
        }
    }

    @tailrec
    final def reduceWhilePrecGreaterOrEqual(state: ShuntingYardState, prec: Int): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) >= 0) {
            reduce(state)
            reduceWhilePrecGreaterOrEqual(state, prec)
        }
    }

    final def wrap(from: Int, to: Int, input: Any): Any = {
        assume(to <= from, "Target level must be less than or equal to current level")
        wraps(from)(to) match {
            // case _: =:=[_, _] => input // Would be faster for 2.12 (which would wrap currently). Slower for other versions.
            case _: <:<[_, _] => input // TODO: not tested?
            case wrap => wrap(input)
        }
    }

    final def restoreStateGetWidth(ctx: Context): Int = {
        val currentOffset = ctx.offset
        ctx.restoreState()
        currentOffset - ctx.offset
    }

    override def relabel(labels: Array[Int]): this.type = {
        prefixAtomLabel = labels(prefixAtomLabel)
        postfixInfixLabel = labels(postfixInfixLabel)
        this
    }

    override def toString: String = s"Shunt(Prefix/Atom: $prefixAtomLabel, Postfix/Infix: $postfixInfixLabel)"
}
