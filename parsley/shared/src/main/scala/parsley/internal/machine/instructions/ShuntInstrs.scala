/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.XAssert.*
import parsley.internal.machine.Context
import parsley.internal.machine.stacks.ArrayStack
import scala.annotation.tailrec

private [internal] sealed abstract class ShuntToken {
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit
}

private [internal] final class Atom(val v: Any, val lvl: Int) extends ShuntToken {
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        state.atoms.push(this)
        shunt.gotoPostInfix(ctx, state)
    }
}
private [internal] abstract class Operator extends ShuntToken {
    private [instructions] val prec: Int
    private [instructions] def isPostfix: Boolean
    private [instructions] def isInfixNonAssoc: Boolean
    private [instructions] def reduce(state: ShuntingYardState, shunt: Shunt): Unit

    @tailrec
    private [instructions] final def reduceWhilePrecGreater(state: ShuntingYardState, shunt: Shunt): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) > 0) {
            state.operators.pop[Operator]().reduce(state, shunt)
            reduceWhilePrecGreater(state, shunt)
        }
    }

    @tailrec
    private [instructions] final def reduceWhilePrecGreaterOrEqual(state: ShuntingYardState, shunt: Shunt): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) >= 0) {
            state.operators.pop[Operator]().reduce(state, shunt)
            reduceWhilePrecGreaterOrEqual(state, shunt)
        }
    }

}

private [internal] final class PrefixOp(f: Any => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit =  {
        if (state.operators.nonEmpty && prec.compare(state.operators.peek.prec) < 0) {
            // This is a malformed expression
            ctx.handlers = ctx.handlers.tail
            val width = shunt.restoreStateGetWidth(ctx)
            ctx.expectedFail(Nil, width)
        } else {
            state.operators.push(this)
            shunt.gotoPreAtom(ctx, state)
        }
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: Shunt): Unit = {
        val input = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(input.lvl, prec, input.v)), prec))
    }
}

private [internal] final class PostfixOp(f: Any => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = true
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.isPostfix && prec.compare(state.operators.peek.prec) > 0) {
            // This was an unexpected postfix operator
            ctx.handlers = ctx.handlers.tail
            ctx.restoreState()
            shunt.produceResult(ctx)
        } else {
            reduceWhilePrecGreaterOrEqual(state, shunt)
            state.operators.push(this)
            shunt.gotoPostInfix(ctx, state)
        }
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: Shunt): Unit = {
        val input = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(input.lvl, prec, input.v)), prec))
    }
}

private [internal] final class InfixLOp(f: (Any, Any) => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        reduceWhilePrecGreaterOrEqual(state, shunt)
        state.operators.push(this)
        shunt.gotoPreAtom(ctx, state)
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: Shunt): Unit = {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(left.lvl, prec, left.v), shunt.wrap(right.lvl, prec + 1, right.v)), prec))
    }
}

private [internal] final class InfixROp(f: (Any, Any) => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = false
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        reduceWhilePrecGreater(state, shunt)
        state.operators.push(this)
        shunt.gotoPreAtom(ctx, state)
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: Shunt): Unit = {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(left.lvl, prec + 1, left.v), shunt.wrap(right.lvl, prec, right.v)), prec))
    }
}

private [internal] final class InfixNOp(f: (Any, Any) => Any, val prec: Int) extends Operator {
    private [instructions] def isPostfix: Boolean = false
    private [instructions] def isInfixNonAssoc: Boolean = true
    private [instructions] def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        reduceWhilePrecGreater(state, shunt)
        if (state.operators.nonEmpty && state.operators.peek.isInfixNonAssoc && state.operators.peek.prec == prec) {
            // This is a special case in which non-associative operators are chained
            ctx.handlers = ctx.handlers.tail
            val width = shunt.restoreStateGetWidth(ctx)
            ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", width)
        } else {
            state.operators.push(this)
            shunt.gotoPreAtom(ctx, state)
        }
    }
    private [instructions] def reduce(state: ShuntingYardState, shunt: Shunt): Unit = {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.peek[Atom]
        state.atoms.exchange(new Atom(f(shunt.wrap(left.lvl, prec + 1, left.v), shunt.wrap(right.lvl, prec + 1, right.v)), prec))
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

    private [instructions] final def gotoPreAtom(ctx: Context, state: ShuntingYardState): Unit = {
        state.failOnNoConsumed = true
        ctx.pc = prefixAtomLabel
        ctx.refreshState()
    }

    private [instructions] final def gotoPostInfix(ctx: Context, state: ShuntingYardState): Unit = {
        state.failOnNoConsumed = false
        ctx.pc = postfixInfixLabel
        ctx.refreshState()
    }

    private [instructions] final def produceResult(ctx: Context): Unit = {
        val state = ctx.stack.peek[ShuntingYardState]

        reduceAll(state)

        assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")

        val atom = state.atoms.peek[Atom]
        ctx.stack.exchange(wrap(atom.lvl, 0, atom.v))
        ctx.inc()
    }

    @tailrec
    private final def reduceAll(state: ShuntingYardState): Unit = if (state.operators.nonEmpty) {
        state.operators.pop[Operator]().reduce(state, this)
        reduceAll(state)
    }

    private [instructions] final def wrap(from: Int, to: Int, input: Any): Any = {
        assume(to <= from, "Target level must be less than or equal to current level")
        wraps(from)(to) match {
            // case _: =:=[_, _] => input // Would be faster for 2.12 (which would wrap currently). Slower for other versions.
            case _: <:<[_, _] => input // TODO: not tested?
            case wrap => wrap(input)
        }
    }

    private [instructions] final def restoreStateGetWidth(ctx: Context): Int = {
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
