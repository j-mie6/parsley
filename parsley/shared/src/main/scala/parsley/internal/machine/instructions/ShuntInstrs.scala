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

private [internal] final class Shunt(var label: Int) extends InstrWithLabel {
  override def apply(ctx: Context): Unit = {
    if (ctx.good) {
      // A choice was made and a new "token" is at the top of the stack
      // This is the body of the for loop in the standard algorithm
      val input = ctx.stack.pop[ShuntInput]()
      val state = ctx.stack.peek[ShuntingYardState]

      input match {
        case a@Atom(_) =>
          state.atoms.push(Right(a))
          ctx.inc()
        case o@Operator(_, fix, prec) => fix match {
          case Prefix => state.atoms.push(Left(o))
          case InfixL => {
            maybeReducePrefixes(prec, state)
            while (state.operators.nonEmpty && state.operators.top.prec >= prec) {
              reduce(state)
            }
            state.operators.push(o)
          }
          case InfixR | InfixN | Postfix => {
            maybeReducePrefixes(prec, state)
            while (state.operators.nonEmpty && state.operators.top.prec > prec) {
              reduce(state)
            }
            state.operators.push(o)
          }
        }
      }
      ctx.updateCheckOffset()
      ctx.pc = label
    } else ctx.catchNoConsumed(ctx.handlers.check) {
      // In this case, there was an error in Choice
      // It is a soft error in which nothing was consumed so we are at the end of the input
      val state = ctx.stack.peek[ShuntingYardState]
      
      while (state.operators.nonEmpty) reduce(state)
      forceReducePrefixes(state)
      
      if (state.atoms.size != 1) {
        throw new IllegalStateException("Expected a single result, but got multiple")
      }
      popAST(state.atoms) match {
        case Atom(v) => ctx.stack.exchange(v) // pops the state to clean up, replace with result
        case _ => throw new IllegalStateException("Expected an atom, but got an operator as the final result")
      }
      ctx.handlers = ctx.handlers.tail
      ctx.addErrorToHintsAndPop()
      ctx.inc()
    }
  }

  private def popAST(atoms: mutable.Stack[Either[Operator, Atom]]): Atom = atoms.pop() match {
    case Left(_) => throw new IllegalStateException("Expected an atom, but got an operator")
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

  override def toString(): String = s"Shunt($label)"
}