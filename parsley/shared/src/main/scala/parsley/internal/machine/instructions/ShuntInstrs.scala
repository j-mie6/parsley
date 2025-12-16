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

private [internal] sealed abstract class ShuntInput {
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit
}

private [internal] final class Atom(val v: Any, val lvl: Int) extends ShuntInput {
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = {
        state.atoms.push(this)
        shunt.gotoPostInfix(ctx, state)
        ctx.refreshState()
    }
}
private [internal] final class Operator(val f: Any, val fix: Int, val prec: Int) extends ShuntInput {
    def handle(ctx: Context, state: ShuntingYardState, shunt: Shunt): Unit = (fix: @switch) match {
        case PrefixTag => shunt.handlePrefixOperator(ctx, this, state)
        case PostfixTag => shunt.handlePostfixOperator(ctx, this, state)
        case InfixLTag => shunt.handleInfixLOperator(ctx, this, state)
        case InfixRTag => shunt.handleInfixROperator(ctx, this, state)
        case InfixNTag => shunt.handleInfixNOperator(ctx, this, state)
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
            val input = ctx.stack.pop[ShuntInput]()
            val state = ctx.stack.peek[ShuntingYardState]
            ctx.updateCheckOffset()

            input.handle(ctx, state, this)
        }
        else handleBadContext(ctx)
    }

    final def handlePrefixOperator(ctx: Context, op: Operator, state: ShuntingYardState): Unit = {
        if (state.operators.nonEmpty && op.prec.compare(state.operators.peek.prec) < 0) {
            // This is a malformed expression
            ctx.handlers = ctx.handlers.tail
            val width = restoreStateGetWidth(ctx)
            ctx.expectedFail(Nil, width)
        } else {
            state.operators.push(op)
            gotoPreAtom(ctx, state)
            ctx.refreshState()
        }
    }

   final def handlePostfixOperator(ctx: Context, op: Operator, state: ShuntingYardState): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.fix == PostfixTag && op.prec.compare(state.operators.peek.prec) > 0) {
            // This was an unexpected postfix operator
            ctx.handlers = ctx.handlers.tail
            ctx.restoreState()
            produceResult(ctx)
        } else {
            reduceWhilePrecGreaterOrEqual(state, op.prec)
            state.operators.push(op)
            gotoPostInfix(ctx, state)
            ctx.refreshState()
        }
    }

    final def handleInfixLOperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
        reduceWhilePrecGreaterOrEqual(state, o.prec)
        state.operators.push(o)
        gotoPreAtom(ctx, state)
        ctx.refreshState()
    }

    final def handleInfixROperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
        reduceWhilePrecGreater(state, o.prec)
        state.operators.push(o)
        gotoPreAtom(ctx, state)
        ctx.refreshState()
    }

    final def handleInfixNOperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
        reduceWhilePrecGreater(state, o.prec)
        if (state.operators.nonEmpty && state.operators.peek.fix == InfixNTag && state.operators.peek.prec == o.prec) {
            // This is a special case in which non-associative operators are chained
            ctx.handlers = ctx.handlers.tail
            val width = restoreStateGetWidth(ctx)
            ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", width)
        } else {
            state.operators.push(o)
            gotoPreAtom(ctx, state)
            ctx.refreshState()
        }
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

    private final def produceResult(ctx: Context): Unit = {
        val state = ctx.stack.peek[ShuntingYardState]

        reduceAll(state)

        assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")

        val atom = state.atoms.peek[Atom]
        ctx.stack.exchange(wrap(atom.lvl, 0, atom.v))
        ctx.inc()
    }

    private final def reduce(state: ShuntingYardState): Unit = {
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
    private final def reduceAll(state: ShuntingYardState): Unit = if (state.operators.nonEmpty) {
        reduce(state)
        reduceAll(state)
    }

    @tailrec
    private final def reduceWhilePrecGreater(state: ShuntingYardState, prec: Int): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) > 0) {
            reduce(state)
            reduceWhilePrecGreater(state, prec)
        }
    }

    @tailrec
    private final def reduceWhilePrecGreaterOrEqual(state: ShuntingYardState, prec: Int): Unit = {
        if (state.operators.nonEmpty && state.operators.peek.prec.compare(prec) >= 0) {
            reduce(state)
            reduceWhilePrecGreaterOrEqual(state, prec)
        }
    }

    private final def wrap(from: Int, to: Int, input: Any): Any = {
        assume(to <= from, "Target level must be less than or equal to current level")
        wraps(from)(to) match {
            // case _: =:=[_, _] => input // Would be faster for 2.12 (which would wrap currently). Slower for other versions.
            case _: <:<[_, _] => input // TODO: not tested?
            case wrap => wrap(input)
        }
    }

    private final def restoreStateGetWidth(ctx: Context): Int = {
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
