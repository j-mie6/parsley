package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.singletons.Pure
import parsley.internal.collection.mutable.SinglyLinkedList
import parsley.internal.machine.instructions
import parsley.internal.machine.instructions.{ShuntInput, Atom, Operator}

private[deepembedding] case class PrecOperators[A, B](val ops: StrictOps[A, B], val precedence: Int)

private [deepembedding] final class Precedence[A](choice: StrictParsley[ShuntInput]) extends StrictParsley[A] {
  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = {
    val start = state.freshLabel()
    val handler = state.freshLabel()
    instrs += new instructions.Fresh(instructions.ShuntingYardState.empty)
    instrs += new instructions.PushHandler(handler)
    instrs += new instructions.Label(start)
    suspend(choice.codeGen[M, R](producesResults = true)) |> {
      instrs += new instructions.Label(handler)
      instrs += new instructions.Shunt(start)
    }
  }

  override private[deepembedding] def inlinable: Boolean = false

  override private[deepembedding] def pretty: String = "precedence" // TODO: implement pretty printing
}

private [deepembedding] object Precedence {
  def apply[A](table: StrictPrec[A]): Precedence[A] = {
    val options = buildChoiceOptions(table, 0)
    if (options.isEmpty) {
      throw new IllegalArgumentException("Precedence table must have at least one operator")
    } else if (options.length == 1) {
      new Precedence(options.head)
    } else if (options.length == 2) {
      new Precedence(<|>(options.head, options(1)))
    } else {
      new Precedence(new Choice(options.head, options(1), SinglyLinkedList(options(2), options.drop(3): _*)))
    }
  }

  private def buildChoiceOptions(table: StrictPrec[_], lvl: Int): List[StrictParsley[ShuntInput]] = table match {
    case StrictPrec.Atoms(atoms) => atoms.map(a => <*>(new Pure(r => Atom(r)), a))
    case StrictPrec.Level(lower, ops) =>
      val lowerChoice = buildChoiceOptions(lower, lvl + 1)
      val opChoice = ops.ops.map(o => <*>(new Pure(r => Operator(r, ops.f, lvl)), o))
      lowerChoice ++ opChoice
  }
}