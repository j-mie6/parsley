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

private [deepembedding] final class Precedence[A](prefixAtomChoice: StrictParsley[ShuntInput], postfixInfixChoice: StrictParsley[ShuntInput], wraps: Array[Array[(Any => Any, Boolean)]]) extends StrictParsley[A] {
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

  private def buildChoiceNode[A](options: List[StrictParsley[A]]): StrictParsley[A] = options match {
    case Nil => new Fail(new FlexibleCaret(0))
    case a :: Nil => a
    case a :: b :: Nil => <|>(a, b)
    case a :: b :: rest => new Choice(a, b, SinglyLinkedList(rest.head, rest.tail: _*))
  }

  private def buildChoiceOptions(table: StrictPrec): (List[StrictParsley[ShuntInput]], List[StrictParsley[ShuntInput]]) = {
    return (
      table.atoms.map(a => <*>(new Pure(r => Atom(r, table.wraps.length)), a)) ++ table.ops.filter(_.fixity == Prefix).map(o => <*>(new Pure(r => Operator(r, Fixity.ordinal(o.fixity), o.prec)), o.op)),
      table.ops.filter(_.fixity != Prefix).map(o => <*>(new Pure(r => Operator(r, Fixity.ordinal(o.fixity), o.prec)), o.op))
    )
  }

  private def buildPrecomputedWraps(wraps: Array[(Any => Any, Boolean)]): Array[Array[(Any => Any, Boolean)]] = {
    val d = wraps.length + 1
    val output = Array.ofDim[(Any => Any, Boolean)](d, d)

    for (i <- 0 until d) output(i)(i) = (identity, true)
    
    for (from <- 0 until d) {
      for (to <- from - 1 to 0 by -1) {
        val (f, i) = output(from)(to + 1)
        val (g, j) = wraps(to)

        if (i) output(from)(to) = (g, j)
        else if (j) output(from)(to) = (f, i)
        else output(from)(to) = (f andThen g, false)
      }
    }
    // for (from <- 0 until d) {
    //   for (to <- 0 until d) {
    //     println(s"$from, $to")
    //     if (to > from) println("X")
    //     else println(s"(${output(from)(to)._1}, ${output(from)(to)._2})")
    //   }
    // }
    output
  }
}