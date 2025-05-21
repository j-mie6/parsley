package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.XAssert._
import parsley.internal.machine.Context
import parsley.expr.Fixity.{PR, PO, IL, IR, IN}
import scala.annotation.switch

private [internal] sealed trait ShuntInput

private [internal] case class Atom(v: Any, lvl: Int) extends ShuntInput
private [internal] case class Operator(f: Any, fix: Int, prec: Int) extends ShuntInput

private [internal] case class ShuntingYardState(
  atoms: mutable.Stack[Atom],
  operators: mutable.Stack[Operator],
  var failOnNoConsumed: Boolean
)

object ShuntingYardState {
  def empty = new ShuntingYardState(mutable.Stack.empty, mutable.Stack.empty, true)
}

private [internal] final class Shunt(var prefixAtomLabel: Int, var postfixInfixLabel: Int, wraps: Array[Any => Any]) extends Instr {
  override def apply(ctx: Context): Unit = {
    if (ctx.good) {
      val input = ctx.stack.pop[ShuntInput]()
      val state = ctx.stack.peek[ShuntingYardState]
      
      // println("State: " + state)

      ctx.updateCheckOffset()
      
      input match {
        case a@Atom(_, _) =>
          state.atoms.push(a)
          state.failOnNoConsumed = false
          ctx.pc = postfixInfixLabel
        case o@Operator(_, fix, prec) => (fix: @switch) match {
          case PR => {
            if (state.operators.nonEmpty && prec < state.operators.top.prec) {
              // This is a malformed expression
              val currentOffset = ctx.offset
              ctx.restoreState()
              ctx.handlers = ctx.handlers.tail
              ctx.expectedFail(Nil, currentOffset-ctx.offset)
              return
            }
            state.operators.push(o)
            state.failOnNoConsumed = true
            ctx.pc = prefixAtomLabel
          }
          case PO => {
            if (state.operators.nonEmpty && state.operators.top.fix == PO && prec > state.operators.top.prec) {
              // This was an unexpected postfix operator
              ctx.restoreState()
              produceResult(ctx)
              return
            }
            while (state.operators.nonEmpty && state.operators.top.prec >= prec) {
              reduce(state)
            }
            state.operators.push(o)
            state.failOnNoConsumed = false
            ctx.pc = postfixInfixLabel
          }
          case IL => {
            while (state.operators.nonEmpty && state.operators.top.prec >= prec) {
              reduce(state)
            }
            state.operators.push(o)
            state.failOnNoConsumed = true
            ctx.pc = prefixAtomLabel
          }
          case IR | IN => {
            while (state.operators.nonEmpty && state.operators.top.prec > prec) {
              reduce(state)
            }
            if (fix == IN && state.operators.nonEmpty && state.operators.top.fix == IN && state.operators.top.prec == prec) {
              // This is a special case in which non-associative operators are chained
              val currentOffset = ctx.offset
              ctx.restoreState()
              ctx.handlers = ctx.handlers.tail
              ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", currentOffset-ctx.offset)
              return
            }
            state.operators.push(o)
            state.failOnNoConsumed = true
            ctx.pc = prefixAtomLabel
          }
        }
      }
      updateState(ctx)
    } else {
      if (ctx.offset != ctx.handlers.check || ctx.stack.peek[ShuntingYardState].failOnNoConsumed) {
        // consumed input and/or prefix/atom choice did not match, hard failure
        ctx.handlers = ctx.handlers.tail
        popState(ctx)
        ctx.fail()
        return
      }
      // In this case, there was an error in Choice
      // It is a soft error in which nothing was consumed and we were looking for an infix/postfix
      // we are at the end of the expression
      
      ctx.good = true
      produceResult(ctx)
      popState(ctx)
      ctx.addErrorToHintsAndPop()
    }
  }

  private def produceResult(ctx: Context): Unit = {
    val state = ctx.stack.pop[ShuntingYardState]()
    // println("State: " + state)

    while (state.operators.nonEmpty) reduce(state)

    assume(state.atoms.size == 1, "Expected exactly one atom at the end of reduction")
    
    val Atom(v, lvl) = state.atoms.pop()
    ctx.stack.push(wrap(lvl, 0)(v))
    ctx.handlers = ctx.handlers.tail
    ctx.inc()
  }

  private def reduce(state: ShuntingYardState): Unit = {
    val op = state.operators.pop()
    (op.fix: @switch) match {
      case IL => {
        val right = state.atoms.pop()
        val left = state.atoms.pop()
        val result = op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec)(left.v), wrap(right.lvl, op.prec + 1)(right.v))
        state.atoms.push(Atom(result, op.prec))
      }
      case IR => {
        val right = state.atoms.pop()
        val left = state.atoms.pop()
        val result = op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1)(left.v), wrap(right.lvl, op.prec)(right.v))
        state.atoms.push(Atom(result, op.prec))
      }
      case IN => {
        val right = state.atoms.pop()
        val left = state.atoms.pop()
        val result = op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1)(left.v), wrap(right.lvl, op.prec + 1)(right.v))
        state.atoms.push(Atom(result, op.prec))
      }
      case PO | PR => {
        val right = state.atoms.pop()
        val result = op.f.asInstanceOf[Any => Any](wrap(right.lvl, op.prec)(right.v))
        state.atoms.push(Atom(result, op.prec))
      }
    }
  }

  private def wrap(currentLvl: Int, targetLvl: Int): (Any => Any) = {
    assume(targetLvl <= currentLvl, "Target level must be less than or equal to current level")
    if (targetLvl == currentLvl) return identity
    (currentLvl - 1 to targetLvl by -1).map(wraps).reduce(_ andThen _)
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