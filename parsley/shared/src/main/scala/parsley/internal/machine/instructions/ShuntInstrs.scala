package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.expr.{Fixity, Prefix, InfixL, InfixR, InfixN, Postfix}

private [internal] sealed trait ShuntInput

private [internal] case class Atom(v: Any, lvl: Int) extends ShuntInput
private [internal] case class Operator(f: Any, fix: Fixity, prec: Int) extends ShuntInput

private [internal] case class ShuntingYardState(
  atoms: mutable.Stack[Atom],
  operators: mutable.Stack[Operator],
  var failOnNoConsumed: Boolean
)

object ShuntingYardState {
  def empty = new ShuntingYardState(mutable.Stack.empty, mutable.Stack.empty, true)
}

private [internal] final class Shunt(var prefixAtomLabel: Int, var postfixInfixLabel: Int, wraps: List[Any => Any]) extends Instr {
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
        case o@Operator(_, fix, prec) => fix match {
          case Prefix => {
            if (state.operators.nonEmpty && o.prec < state.operators.top.prec) { // TODO: check case
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
          case InfixL => {
            while (state.operators.nonEmpty && state.operators.top.prec >= prec) {
              reduce(state)
            }
            state.operators.push(o)
            state.failOnNoConsumed = true
            ctx.pc = prefixAtomLabel
          }
          case InfixR | InfixN | Postfix => {
            while (state.operators.nonEmpty && state.operators.top.prec > prec) {
              reduce(state)
            }
            if (fix == InfixN && state.operators.nonEmpty && state.operators.top.fix == InfixN && state.operators.top.prec == prec) {
              // This is a special case in which non-associative operators are chained
              val currentOffset = ctx.offset
              ctx.restoreState()
              ctx.handlers = ctx.handlers.tail
              ctx.expectedFailWithReason(Nil, "operator cannot be applied in sequence as it is non-associative", currentOffset-ctx.offset)
              return
            }
            state.operators.push(o)
            state.failOnNoConsumed = fix != Postfix
            if (fix == Postfix) ctx.pc = postfixInfixLabel
            else ctx.pc = prefixAtomLabel
          }
        }
      }
      updateState(ctx)
    } else {
      if (ctx.offset != ctx.handlers.check || ctx.stack.peek[ShuntingYardState].failOnNoConsumed) {
        // consumed input or prefix/atom choice did not match, hard failure
        ctx.handlers = ctx.handlers.tail
        popState(ctx)
        ctx.fail()
        return
      }
      // In this case, there was an error in Choice
      // It is a soft error in which nothing was consumed and we were looking for an infix/postfix
      // we are at the end of the expression
      
      ctx.good = true
      val state = ctx.stack.peek[ShuntingYardState]
      // println("State: " + state)

      while (state.operators.nonEmpty) reduce(state)

      assume(state.atoms.size == 1, "Expected exactly one atom at the end of expression")
      
      state.atoms.pop() match {
        case Atom(v, lvl) => ctx.stack.exchange(wrap(lvl, 0)(v))
      }
      ctx.handlers = ctx.handlers.tail
      ctx.addErrorToHintsAndPop()
      popState(ctx)
      ctx.inc()
    }
  }

  private def reduce(state: ShuntingYardState): Unit = {
    val op = state.operators.pop()
    op.fix match {
      case InfixR | InfixL | InfixN => {
        val right = state.atoms.pop()
        val left = state.atoms.pop()
        val result = op.fix match {
          case InfixL => op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec)(left.v), wrap(right.lvl, op.prec + 1)(right.v))
          case InfixR => op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1)(left.v), wrap(right.lvl, op.prec)(right.v))
          case InfixN => op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1)(left.v), wrap(right.lvl, op.prec + 1)(right.v))
          case _ => throw new IllegalStateException("Will never happen") // TODO: remove this
        }
        state.atoms.push(Atom(result, op.prec))
      }
      case Postfix | Prefix => {
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