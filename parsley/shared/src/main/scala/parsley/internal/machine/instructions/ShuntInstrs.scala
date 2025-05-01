package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.internal.machine.Context
import parsley.expr.{Fixity, Prefix, InfixL, InfixR, InfixN, Postfix}

private [internal] sealed trait ShuntInput

private [internal] case class Atom(v: Any) extends ShuntInput
private [internal] case class Operator(f: Any, fix: Fixity, prec: Int) extends ShuntInput

private [internal] case class ShuntingYardState(
  atoms: mutable.Stack[Either[Operator, Atom]],
  operators: mutable.Stack[Operator]
)

object ShuntingYardState {
  def empty = new ShuntingYardState(mutable.Stack.empty, mutable.Stack.empty)
}

private [internal] final class Shunt(var prefixAtomLabel: Int, var postfixInfixLabel: Int) extends Instr {
  override def apply(ctx: Context): Unit = {
    if (ctx.good) {
      val input = ctx.stack.pop[ShuntInput]()
      val state = ctx.stack.peek[ShuntingYardState]
      println("State: " + state)

      ctx.updateCheckOffset()
      
      input match {
        case a@Atom(_) =>
          state.atoms.push(Right(a))
          ctx.pc = postfixInfixLabel
        case o@Operator(_, fix, prec) => fix match {
          case Prefix => {
            state.atoms.push(Left(o))
            ctx.pc = prefixAtomLabel
          }
          case InfixL => {
            maybeReducePrefixes(prec, state)
            while (state.operators.nonEmpty && state.operators.top.prec >= prec) {
              reduce(state)
            }
            state.operators.push(o)
            ctx.pc = prefixAtomLabel
          }
          case InfixR | InfixN | Postfix => {
            maybeReducePrefixes(prec, state)
            while (state.operators.nonEmpty && state.operators.top.prec > prec) {
              reduce(state)
            }
            state.operators.push(o)
            if (fix == Postfix) ctx.pc = postfixInfixLabel
            else ctx.pc = prefixAtomLabel
          }
        }
      }
    } else ctx.catchNoConsumed(ctx.handlers.check) {
      // In this case, there was an error in Choice
      // It is a soft error in which nothing was consumed so we are at the end of the input
      val state = ctx.stack.peek[ShuntingYardState]
      
      forceReducePrefixes(state)
      while (state.operators.nonEmpty) reduce(state)
      forceReducePrefixes(state)
      
      if (state.atoms.size != 1) {
        throw new IllegalStateException(f"Expected a single result, but got ${state.atoms.size} results")
        // ctx.fail()
      }
      popAST(state.atoms) match {
        case Atom(v) => ctx.stack.exchange(v) // pops the state to clean up, replace with result
        case _ => {
          throw new IllegalStateException("Expected an atom, but got an operator as the final result")
          // ctx.fail()
        }
      }
      ctx.handlers = ctx.handlers.tail
      ctx.addErrorToHintsAndPop()
      ctx.inc()
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
      case _ => throw new IllegalStateException("Expected a prefix operator, but got an atom")
    }
    case Right(_) => throw new IllegalStateException("Expected a prefix operator, but got an atom")
  }

  private def reduce(state: ShuntingYardState): Unit = {
    val op = state.operators.pop()
    op.fix match {
      case InfixR | InfixL | InfixN => {
        if (state.atoms.size < 2) throw new IllegalStateException("Not enough operands for infix operator")
        val right = popAST(state.atoms)
        val left = popAST(state.atoms)
        val result = op.f.asInstanceOf[(Any, Any) => Any](left.v, right.v)
        state.atoms.push(Right(Atom(result)))
      }
      case Postfix => {
        if (state.atoms.isEmpty) throw new IllegalStateException("Not enough operands for postfix operator")
        val right = popAST(state.atoms)
        val result = op.f.asInstanceOf[Any => Any](right.v)
        state.atoms.push(Right(Atom(result)))
      }
      case _ => throw new IllegalStateException("Unexpected Prefix in operator stack")
    }
  }

  private def maybeReducePrefixes(nextOpPrec: Int, state: ShuntingYardState): Unit = {
    while (state.atoms.size >= 2 && state.atoms(1).isLeft && state.atoms(1).swap.getOrElse(throw new IllegalStateException("Expected a prefix operator")).prec > nextOpPrec) {
      val operand = popAST(state.atoms)
      val prefixOp = popPrefix(state.atoms)
      val result = prefixOp.f.asInstanceOf[Any => Any](operand.v)
      state.atoms.push(Right(Atom(result)))
    }
  }

  private def forceReducePrefixes(state: ShuntingYardState): Unit = {
    while (state.atoms.size >= 2 && state.atoms(1).isLeft) {
        val operand = popAST(state.atoms)
        val prefixOp = popPrefix(state.atoms)
        val result = prefixOp.f.asInstanceOf[Any => Any](operand.v)
        state.atoms.push(Right(Atom(result)))
      }
  }

  override def relabel(labels: Array[Int]): this.type = {
    prefixAtomLabel = labels(prefixAtomLabel)
    postfixInfixLabel = labels(postfixInfixLabel)
    this
  }

  override def toString(): String = s"Shunt(Pre/Atom label: $prefixAtomLabel, Post/Infix label: $postfixInfixLabel)"
}