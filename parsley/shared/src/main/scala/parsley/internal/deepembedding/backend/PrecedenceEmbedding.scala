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
  def apply[A](table: StrictPrec[A]): Precedence[A] = buildChoiceOptions(table, 0) match {
    case Nil => throw new IllegalArgumentException("Precedence table must have at least one operator")
    case a :: Nil => new Precedence(a)
    case a :: b :: Nil => new Precedence(<|>(a, b))
    case a :: b :: rest => new Precedence(new Choice(a, b, SinglyLinkedList(rest.head, rest.tail: _*)))
  }

  private def buildChoiceOptions(table: StrictPrec[_], lvl: Int): List[StrictParsley[ShuntInput]] = table match {
    case StrictPrec.Atoms(atoms) => atoms.map(a => <*>(new Pure(r => Atom(r)), a))
    case StrictPrec.Level(lower, ops) =>
      val lowerOptions = buildChoiceOptions(lower, lvl + 1)
      val opOptions = ops.ops.map(o => <*>(new Pure(r => Operator(r, ops.f, lvl)), o))
      lowerOptions ++ opOptions
  }
}