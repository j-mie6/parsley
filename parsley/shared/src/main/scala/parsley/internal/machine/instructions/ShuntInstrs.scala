package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.expr.{Fixity, Prefix, InfixL, InfixR, InfixN, Postfix}
import parsley.internal.errors.FlexibleCaret

private [internal] sealed trait ShuntInput

private [internal] case class Atom(v: Any, lvl: Int) extends ShuntInput
private [internal] case class Operator(f: Any, fix: Fixity, prec: Int) extends ShuntInput

private [internal] case class ShuntingYardState(
  atoms: mutable.Stack[Either[Operator, Atom]],
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
          state.atoms.push(Right(a))
          state.failOnNoConsumed = false
          ctx.pc = postfixInfixLabel
        case o@Operator(_, fix, prec) => fix match {
          case Prefix => {
            state.atoms.push(Left(o))
            state.failOnNoConsumed = true
            ctx.pc = prefixAtomLabel
          }
          case InfixL => {
            maybeReducePrefixes(prec, state)
            while (state.operators.nonEmpty && state.operators.top.prec >= prec) {
              reduce(state)
            }
            state.operators.push(o)
            state.failOnNoConsumed = true
            ctx.pc = prefixAtomLabel
          }
          case InfixR | InfixN | Postfix => {
            maybeReducePrefixes(prec, state)
            while (state.operators.nonEmpty && state.operators.top.prec > prec) {
              reduce(state)
            }
            if (fix == InfixN && state.operators.nonEmpty && state.operators.top.fix == InfixN && state.operators.top.prec == prec) {
              // This is a special case in which non-associative operators are chained
              val currentOffset = ctx.offset
              ctx.restoreState()
              ctx.handlers = ctx.handlers.tail
              ctx.failWithMessage(new FlexibleCaret(currentOffset-ctx.offset) , "Non-associative operator cannot be chained") // needs to be token length
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
    } else ctx.catchNoConsumed(ctx.handlers.check) {
      // In this case, there was an error in Choice
      // It is a soft error in which nothing was consumed so we are at the end of the input
      val state = ctx.stack.peek[ShuntingYardState]
      // println("State: " + state)

      if (state.failOnNoConsumed) {
        // The prefix/atom choice did not match, malformed expression
        ctx.good = false
        ctx.handlers = ctx.handlers.tail
        popState(ctx)
        ctx.fail()
      } else {
        // The postfix/infix choice did not match, this is the end of the input
        forceReducePrefixes(state)
        while (state.operators.nonEmpty) reduce(state)
        forceReducePrefixes(state)
        
        popAST(state.atoms) match {
          case Atom(v, lvl) => ctx.stack.exchange(wrap(lvl, 0)(v))
        }
        ctx.handlers = ctx.handlers.tail
        ctx.addErrorToHintsAndPop()
        ctx.inc()
        popState(ctx)
        // ctx.ret()
      }
    }
  }

  private def popAST(atoms: mutable.Stack[Either[Operator, Atom]]): Atom = atoms.pop() match {
    case Left(_) => {
      throw new IllegalStateException("Expected an atom, but got an operator")
      // ctx.fail()
    }
    case Right(a) => a 
  }

  private def popPrefix(atoms: mutable.Stack[Either[Operator, Atom]]): Operator = atoms.pop() match {
    case Left(op) => op.fix match {
      case Prefix => op
      case _ => throw new IllegalStateException("Expected a prefix operator, but got a different operator type")
    }
    case Right(_) => throw new IllegalStateException("Expected a prefix operator, but got an atom")
  }

  private def reduce(state: ShuntingYardState): Unit = {
    val op = state.operators.pop()
    op.fix match {
      case InfixR | InfixL | InfixN => {
        // this failure case should not occur due to the choice constraints
        // if (state.atoms.size < 2) throw new IllegalStateException("Not enough operands for infix operator")
        val right = popAST(state.atoms)
        val left = popAST(state.atoms)
        val result = op.fix match {
          case InfixL => op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec)(left.v), wrap(right.lvl, op.prec + 1)(right.v))
          case InfixR => op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1)(left.v), wrap(right.lvl, op.prec)(right.v))
          case InfixN => op.f.asInstanceOf[(Any, Any) => Any](wrap(left.lvl, op.prec + 1)(left.v), wrap(right.lvl, op.prec + 1)(right.v))
        }
        state.atoms.push(Right(Atom(result, op.prec)))
      }
      case Postfix => {
        // if (state.atoms.isEmpty) throw new IllegalStateException("Not enough operands for postfix operator")
        val right = popAST(state.atoms)
        val result = op.f.asInstanceOf[Any => Any](wrap(right.lvl, op.prec)(right.v))
        state.atoms.push(Right(Atom(result, op.prec)))
      }
      case _ => throw new IllegalStateException("Unexpected Prefix in operator stack")
    }
  }

  private def maybeReducePrefixes(nextOpPrec: Int, state: ShuntingYardState): Unit = {
    while (state.atoms.size >= 2 && state.atoms(1).isLeft && state.atoms(1).swap.getOrElse(throw new IllegalStateException("Expected a prefix operator")).prec > nextOpPrec) {
      val operand = popAST(state.atoms)
      val prefixOp = popPrefix(state.atoms)
      val result = prefixOp.f.asInstanceOf[Any => Any](wrap(operand.lvl, prefixOp.prec)(operand.v))
      state.atoms.push(Right(Atom(result, prefixOp.prec)))
    }
  }

  private def forceReducePrefixes(state: ShuntingYardState): Unit = {
    while (state.atoms.size >= 2 && state.atoms(1).isLeft) {
        val operand = popAST(state.atoms)
        val prefixOp = popPrefix(state.atoms)
        val result = prefixOp.f.asInstanceOf[Any => Any](wrap(operand.lvl, prefixOp.prec)(operand.v))
        state.atoms.push(Right(Atom(result, prefixOp.prec)))
      }
  }

  private def wrap(currentLvl: Int, targetLvl: Int): (Any => Any) = {
    assume(targetLvl <= currentLvl)
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