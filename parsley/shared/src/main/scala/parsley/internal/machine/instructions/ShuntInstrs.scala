package parsley.internal.machine.instructions

import parsley.XAssert._
import parsley.internal.machine.Context
import parsley.expr.Fixity.{PrefixTag, PostfixTag, InfixLTag, InfixRTag, InfixNTag}
import scala.annotation.switch
import parsley.internal.machine.stacks.ArrayStack

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
        val o@Operator(_, fix, prec) = input.asInstanceOf[Operator]
        (fix: @switch) match {
          case PrefixTag => handlePrefixOperator(ctx, o, prec, state)
          case PostfixTag => handlePostfixOperator(ctx, o, prec, state)
          case InfixLTag => handleInfixLOperator(ctx, o, prec, state)
          case InfixRTag | InfixNTag => handleInfixRNOperator(ctx, fix, o, prec, state)
        }
      }
    } else handleBadContext(ctx)
  }

  def handleAtom(ctx: Context, atom: Atom, state: ShuntingYardState): Unit = {
    state.atoms.push(atom)
    gotoPostInfix(ctx, state)
    updateState(ctx)
  }
  
  def handlePrefixOperator(ctx: Context, o: Operator, prec: Int, state: ShuntingYardState): Unit =
    if (!state.operators.isEmpty && prec < state.operators.peek.prec) {
      // This is a malformed expression
      val currentOffset = ctx.offset
      ctx.restoreState()
      ctx.handlers = ctx.handlers.tail
      ctx.expectedFail(Nil, currentOffset-ctx.offset)
    } else {
      state.operators.push(o)
      gotoPreAtom(ctx, state)
      updateState(ctx)
    }

  def handlePostfixOperator(ctx: Context, o: Operator, prec: Int, state: ShuntingYardState): Unit =
    if (!state.operators.isEmpty && state.operators.peek.fix == PostfixTag && prec > state.operators.peek.prec) {
      // This was an unexpected postfix operator
      ctx.restoreState()
      produceResult(ctx)
    } else {
      while (!state.operators.isEmpty && state.operators.peek.prec >= prec) {
        reduce(state)
      }
      state.operators.push(o)
      gotoPostInfix(ctx, state)
      updateState(ctx)
    }

  def handleInfixLOperator(ctx: Context, o: Operator, prec: Int, state: ShuntingYardState): Unit = {
    while (!state.operators.isEmpty && state.operators.peek.prec >= prec) {
      reduce(state)
    }
    state.operators.push(o)
    gotoPreAtom(ctx, state)
    updateState(ctx)
  }
  def handleInfixRNOperator(ctx: Context, fix: Int, o: Operator, prec: Int, state: ShuntingYardState): Unit = {
    while (!state.operators.isEmpty && state.operators.peek.prec > prec) {
      reduce(state)
    }
    if (fix == InfixNTag && !state.operators.isEmpty && state.operators.peek.fix == InfixNTag && state.operators.peek.prec == prec) {
      // This is a special case in which non-associative operators are chained
      val currentOffset = ctx.offset
      ctx.restoreState()
      ctx.handlers = ctx.handlers.tail
      ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", currentOffset-ctx.offset)
    } else {
      state.operators.push(o)
      gotoPreAtom(ctx, state)
      updateState(ctx)
    }
  }
  def handleBadContext(ctx: Context): Unit =
    if (ctx.offset != ctx.handlers.check || ctx.stack.peek[ShuntingYardState].failOnNoConsumed) {
      // consumed input and/or prefix/atom choice did not match, hard failure
      ctx.handlers = ctx.handlers.tail
      popState(ctx)
      ctx.fail()
    } else {
      // In this case, there was an error in Choice
      // It is a soft error in which nothing was consumed and we were looking for an infix/postfix
      // we are at the end of the expression

      ctx.good = true
      produceResult(ctx)
      popState(ctx)
      ctx.addErrorToHintsAndPop()
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

    while (!state.operators.isEmpty) reduce(state)

    assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")
    
    val Atom(v, lvl) = state.atoms.pop[Atom]()
    ctx.stack.push(wrap(lvl, 0, v))
    ctx.handlers = ctx.handlers.tail
    ctx.inc()
  }

  private def reduce(state: ShuntingYardState): Unit = {
    val op = state.operators.pop[Operator]()
    (op.fix: @switch) match {
      case InfixLTag => {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.pop[Atom]()
        val result = op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec, left.v), wrap(right.lvl, op.prec + 1, right.v))
        state.atoms.push(Atom(result, op.prec))
      }
      case InfixRTag => {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.pop[Atom]()
        val result = op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1, left.v), wrap(right.lvl, op.prec, right.v))
        state.atoms.push(Atom(result, op.prec))
      }
      case InfixNTag => {
        val right = state.atoms.pop[Atom]()
        val left = state.atoms.pop[Atom]()
        val result = op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1, left.v), wrap(right.lvl, op.prec + 1, right.v))
        state.atoms.push(Atom(result, op.prec))
      }
      case PostfixTag | PrefixTag => {
        val right = state.atoms.pop[Atom]()
        val result = op.f.asInstanceOf[Any => Any](wrap(right.lvl, op.prec, right.v))
        state.atoms.push(Atom(result, op.prec))
      }
    }
  }

  private def wrap(from: Int, to: Int, input: Any): Any = {
    assume(to <= from, "Target level must be less than or equal to current level")
    wraps(from)(to) match {
      // case _: =:=[_, _] => input // FIXME: remove =:= when 2.12 removed
      case _: <:<[_, _] => input
      case wrap => wrap(input)
    }
  }

  private def updateState(ctx: Context): Unit = {
    popState(ctx)
    ctx.saveState()
  }

  private def popState(ctx: Context): Unit = {
    ctx.states = ctx.states.tail
  }

  override def relabel(labels: Array[Int]): this.type = {
    prefixAtomLabel = labels(prefixAtomLabel)
    postfixInfixLabel = labels(postfixInfixLabel)
    this
  }

  override def toString(): String = s"Shunt(Pre/Atom label: $prefixAtomLabel, Post/Infix label: $postfixInfixLabel)"
}