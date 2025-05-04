package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.singletons.Pure
import parsley.internal.collection.mutable.SinglyLinkedList
import parsley.internal.machine.instructions
import parsley.internal.machine.instructions.{ShuntInput, Atom, Operator}
import parsley.expr.Prefix

private [deepembedding] final class Precedence[A](prefixAtomChoice: StrictParsley[ShuntInput], postfixInfixChoice: StrictParsley[ShuntInput], wraps: List[Any => Any]) extends StrictParsley[A] {
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
    new Precedence(prefixAtomChoice, postfixInfixChoice, table.wraps)
  }

  private def buildChoiceNode[A](options: List[StrictParsley[A]]): StrictParsley[A] = options match {
    case Nil => throw new IllegalArgumentException("Cannot build choice node with empty options") // TODO: what goes here?
    case a :: Nil => a
    case a :: b :: Nil => <|>(a, b)
    case a :: b :: rest => new Choice(a, b, SinglyLinkedList(rest.head, rest.tail: _*))
  }

  private def buildChoiceOptions(table: StrictPrec): (List[StrictParsley[ShuntInput]], List[StrictParsley[ShuntInput]]) = {
    return (
      table.atoms.map(a => <*>(new Pure(r => Atom(r, table.wraps.length)), a)) ++ table.ops.filter(_.fixity == Prefix).map(o => <*>(new Pure(r => Operator(r, o.fixity, o.prec)), o.op)),
      table.ops.filter(_.fixity != Prefix).map(o => <*>(new Pure(r => Operator(r, o.fixity, o.prec)), o.op))
    )    
  }

  // private def buildChoiceOptions(table: StrictPrec[_], lvl: Int): (List[StrictParsley[ShuntInput]], List[StrictParsley[ShuntInput]]) = table match {
  //   case StrictPrec.Atoms(atoms) => (atoms.map(a => <*>(new Pure(r => Atom(r)), a)), Nil)
  //   case StrictPrec.Level(lower, ops) =>
  //     val lowerOptions = buildChoiceOptions(lower, lvl + 1)
  //     val opOptions = ops.ops.map(o => <*>(new Pure(r => Operator(r, ops.f, lvl)), o))
  //     ops.f match {
  //       case Prefix => (lowerOptions._1 ++ opOptions, lowerOptions._2)
  //       case _      => (lowerOptions._1, lowerOptions._2 ++ opOptions)
  //     }
  // }
}