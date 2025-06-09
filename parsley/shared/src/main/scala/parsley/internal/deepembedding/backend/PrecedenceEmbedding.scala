package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.singletons.Pure
import parsley.internal.collection.mutable.SinglyLinkedList
import parsley.internal.machine.instructions
import parsley.internal.machine.instructions.{ShuntInput, Atom, Operator}
import parsley.expr.Prefix
import parsley.internal.deepembedding.singletons.Fail
import parsley.internal.errors.FlexibleCaret
import parsley.expr.Fixity

private [deepembedding] final class Precedence[A](prefixAtomChoice: StrictParsley[ShuntInput], postfixInfixChoice: StrictParsley[ShuntInput], wraps: Array[Array[Any => Any]]) extends StrictParsley[A] {
  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = {
    val prefixAtomLabel = state.freshLabel()
    val postfixInfixLabel = state.freshLabel()
    val shuntLabel = state.freshLabel()
    instrs += new instructions.Fresh(instructions.ShuntingYardState.empty)
    instrs += new instructions.PushHandlerAndState(shuntLabel)
    instrs += new instructions.Label(prefixAtomLabel)
    suspend(prefixAtomChoice.codeGen[M, R](producesResults = true)) >> {
      instrs += new instructions.Jump(shuntLabel)
      instrs += new instructions.Label(postfixInfixLabel)
      suspend(postfixInfixChoice.codeGen[M, R](producesResults = true)) |> {
        instrs += new instructions.Label(shuntLabel)
        instrs += new instructions.Shunt(prefixAtomLabel, postfixInfixLabel, wraps)
      }
    }
  }

  override private[deepembedding] def inlinable: Boolean = false

  override private[deepembedding] def pretty: String = "precedence" // TODO: implement pretty printing
}

private [deepembedding] object Precedence {
  def apply[A](table: StrictPrec): Precedence[A] = {
    val (prefixAtomOptions, postfixInfixOptions) = buildChoiceOptions(table)
    val prefixAtomChoice = buildChoiceNode(prefixAtomOptions)
    val postfixInfixChoice = buildChoiceNode(postfixInfixOptions)
    new Precedence(prefixAtomChoice, postfixInfixChoice, buildPrecomputedWraps(table.wraps))
  }

  private def buildChoiceNode[A](options: List[StrictParsley[A]]): StrictParsley[A] = options.map(_.optimise) match {
    case Nil => new Fail(new FlexibleCaret(0))
    case p :: Nil => p
    case p1 :: p2 :: Nil => <|>(p1, p2)
    case p1 :: p2 :: p3 :: tail => new Choice(p1, p2, SinglyLinkedList(p3, tail: _*))
  }

  private def unwrapChoices(ps: List[StrictParsley[Any]]): List[StrictParsley[Any]] = ps.flatMap {
    case Choice(alt1, alt2, alts) => alt1 :: alt2 ::  alts.toList
    case p => p :: Nil
  }
  
  private def buildOpChoice(o: StrictOp): StrictParsley[Operator] = <*>(new Pure(r => Operator(r, Fixity.ordinal(o.fixity), o.prec)), o.op).optimise

  private def buildChoiceOptions(table: StrictPrec): (List[StrictParsley[ShuntInput]], List[StrictParsley[ShuntInput]]) = (
    table.ops.filter(_.fixity == Prefix).map(buildOpChoice) ::: unwrapChoices(table.atoms).map(a => <*>(new Pure(r => Atom(r, table.wraps.length)), a).optimise),
    table.ops.filter(_.fixity != Prefix).map(buildOpChoice)
  )

  private def buildPrecomputedWraps(wraps: Array[Any => Any]): Array[Array[Any => Any]] = {
    val d = wraps.length + 1
    val output = Array.ofDim[Any => Any](d, d)

    for (i <- 0 until d) output(i)(i) = identity
    
    for (from <- 0 until d) {
      for (to <- from - 1 to 0 by -1) {
        output(from)(to) = output(from)(to + 1) match {
          case _: <:<[_, _] => wraps(to)
          case prev => wraps(to) match {
            case _: <:<[_, _] => prev
            case next => prev andThen next
          }
        }
        // output(from)(to) = output(from)(to + 1) andThen wraps(to)
      }
    }
    output
  }
}