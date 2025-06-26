/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.XAssert._
import parsley.internal.machine.Context
import parsley.expr.Fixity.{PrefixTag, PostfixTag, InfixLTag, InfixRTag, InfixNTag}
import scala.annotation.switch
import parsley.internal.machine.stacks.ArrayStack
import scala.annotation.tailrec

private [internal] sealed trait ShuntInput {
  val tag: Int
}

private [internal] case class Atom(v: Any, lvl: Int) extends ShuntInput {
  override val tag: Int = ShuntInput.AtomTag
}
private [internal] case class Operator(f: Any, fix: Int, prec: Int) extends ShuntInput {
  override val tag: Int = ShuntInput.OperatorTag
}

object ShuntInput {
  val AtomTag = 0
  val OperatorTag = 1
}

private [internal] case class ShuntingYardState(
  atoms: ArrayStack[Atom],
  operators: ArrayStack[Operator],
  var failOnNoConsumed: Boolean
)

object ShuntingYardState {
  def empty = new ShuntingYardState(new ArrayStack(), new ArrayStack(), true)
}

private [internal] final class Shunt(var prefixAtomLabel: Int, var postfixInfixLabel: Int, wraps: Array[Array[Any => Any]]) extends Instr {
  override def apply(ctx: Context): Unit = {
    if (ctx.good) {
      val input = ctx.stack.pop[ShuntInput]()
      val state = ctx.stack.peek[ShuntingYardState]
      // println("State: " + state)
      ctx.updateCheckOffset()

      if (input.tag == ShuntInput.AtomTag) handleAtom(ctx, input.asInstanceOf[Atom], state)
      else {
        val o@Operator(_, fix, _) = input.asInstanceOf[Operator]
        (fix: @switch) match {
          case PrefixTag => handlePrefixOperator(ctx, o, state)
          case PostfixTag => handlePostfixOperator(ctx, o, state)
          case InfixLTag => handleInfixLOperator(ctx, o, state)
          case InfixRTag => handleInfixROperator(ctx, o, state)
          case InfixNTag => handleInfixNOperator(ctx, o, state)
        }
      }
    } else handleBadContext(ctx)
  }

  private def handleAtom(ctx: Context, atom: Atom, state: ShuntingYardState): Unit = {
    state.atoms.push(atom)
    gotoPostInfix(ctx, state)
    updateState(ctx)
  }

  private def handlePrefixOperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
    if (!state.operators.isEmpty && o.prec.compare(state.operators.peek.prec) == -1) {
      // This is a malformed expression
      popHandler(ctx)
      val width = restoreStateGetWidth(ctx)
      ctx.expectedFail(Nil, width)
    } else {
      state.operators.push(o)
      gotoPreAtom(ctx, state)
      updateState(ctx)
    } 
  }

  private def handlePostfixOperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
    if (!state.operators.isEmpty && state.operators.peek.fix == PostfixTag && o.prec.compare(state.operators.peek.prec) == 1) {
      // This was an unexpected postfix operator
      popHandler(ctx)
      ctx.restoreState()
      produceResult(ctx)
    } else {
      reduceWhilePrecGreaterOrEqual(state, o.prec)
      state.operators.push(o)
      gotoPostInfix(ctx, state)
      updateState(ctx)
    }
  }

  private def handleInfixLOperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
    reduceWhilePrecGreaterOrEqual(state, o.prec)
    state.operators.push(o)
    gotoPreAtom(ctx, state)
    updateState(ctx)
  }

  private def handleInfixROperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
    reduceWhilePrecGreater(state, o.prec)
    state.operators.push(o)
    gotoPreAtom(ctx, state)
    updateState(ctx)
  }

  private def handleInfixNOperator(ctx: Context, o: Operator, state: ShuntingYardState): Unit = {
    reduceWhilePrecGreater(state, o.prec)
    if (!state.operators.isEmpty && state.operators.peek.fix == InfixNTag && state.operators.peek.prec == o.prec) {
      // This is a special case in which non-associative operators are chained
      popHandler(ctx)
      val width = restoreStateGetWidth(ctx)
      ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", width)
    } else {
      state.operators.push(o)
      gotoPreAtom(ctx, state)
      updateState(ctx)
    }
  }

  private def handleBadContext(ctx: Context): Unit = {
    if (ctx.offset != ctx.handlers.check || ctx.stack.peek[ShuntingYardState].failOnNoConsumed) {
      // consumed input and/or prefix/atom choice did not match, hard failure
      popHandler(ctx)
      popState(ctx)
      ctx.fail()
    } else {
      // The end of the expression has been reached
      popHandler(ctx)
      popState(ctx)
      ctx.good = true
      ctx.addErrorToHintsAndPop()
      produceResult(ctx)
    }
  }

  private def gotoPreAtom(ctx: Context, state: ShuntingYardState): Unit = {
    state.failOnNoConsumed = true
    ctx.pc = prefixAtomLabel
  }

  private def gotoPostInfix(ctx: Context, state: ShuntingYardState): Unit = {
    state.failOnNoConsumed = false
    ctx.pc = postfixInfixLabel
  }

  private def produceResult(ctx: Context): Unit = {
    val state = ctx.stack.pop[ShuntingYardState]()
    // println("State: " + state)

    reduceAll(state)

    assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")
    
    val Atom(v, lvl) = state.atoms.pop[Atom]()
    ctx.stack.push(wrap(lvl, 0, v))
    ctx.inc()
  }

  private def reduce(state: ShuntingYardState): Unit = {
    val op = state.operators.pop[Operator]()
    val result = (op.fix: @switch) match {
      case InfixLTag => {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.pop[Atom]()
        op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec, left.v), wrap(right.lvl, op.prec + 1, right.v))
      }
      case InfixRTag => {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.pop[Atom]()
        op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1, left.v), wrap(right.lvl, op.prec, right.v))
      }
      case InfixNTag => {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.pop[Atom]()
        op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1, left.v), wrap(right.lvl, op.prec + 1, right.v))
      }
      case PostfixTag | PrefixTag => {
        val input = state.atoms.pop[Atom]()
        op.f.asInstanceOf[Any => Any](wrap(input.lvl, op.prec, input.v))
      }
    }
    state.atoms.push(Atom(result, op.prec))
  }

  @tailrec
  private def reduceAll(state: ShuntingYardState): Unit = {
    if (state.operators.isEmpty) return
    else {
      reduce(state)
      reduceAll(state)
    }
  }

  @tailrec
  private def reduceWhilePrecGreater(state: ShuntingYardState, prec: Int): Unit = {
    if (state.operators.isEmpty || state.operators.peek.prec.compare(prec) != 1) return
    else {
      reduce(state)
      reduceWhilePrecGreater(state, prec)
    }
  }

  @tailrec
  private def reduceWhilePrecGreaterOrEqual(state: ShuntingYardState, prec: Int): Unit = {
    if (state.operators.isEmpty || state.operators.peek.prec.compare(prec) == -1) return
    else {
      reduce(state)
      reduceWhilePrecGreaterOrEqual(state, prec)
    }
  }

  private def wrap(from: Int, to: Int, input: Any): Any = {
    assume(to <= from, "Target level must be less than or equal to current level")
    wraps(from)(to) match {
      // case _: =:=[_, _] => input // Would be faster for 2.12 (which would wrap currently). Slower for other versions.
      case _: <:<[_, _] => input
      case wrap => wrap(input)
    }
  }

  private def restoreStateGetWidth(ctx: Context): Int = {
    val currentOffset = ctx.offset
    ctx.restoreState()
    currentOffset - ctx.offset
  }

  private def updateState(ctx: Context): Unit = {
    popState(ctx)
    ctx.saveState()
  }

  private def popState(ctx: Context): Unit = ctx.states = ctx.states.tail

  private def popHandler(ctx: Context): Unit = ctx.handlers = ctx.handlers.tail

  override def relabel(labels: Array[Int]): this.type = {
    prefixAtomLabel = labels(prefixAtomLabel)
    postfixInfixLabel = labels(postfixInfixLabel)
    this
  }

  override def toString(): String = s"Shunt(Prefix/Atom: $prefixAtomLabel, Postfix/Infix: $postfixInfixLabel)"
}