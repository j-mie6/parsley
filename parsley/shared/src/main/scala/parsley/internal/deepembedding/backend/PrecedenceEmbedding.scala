package parsley.internal.deepembedding.backend

import parsley.expr.Fixity
import parsley.internal.deepembedding.ContOps

sealed trait StrictPrec[+Out] {
  type In
}

object StrictPrec {
  final case class Atoms[A](atoms: List[StrictParsley[A]]) extends StrictPrec[A] {
    type In = A // never used
  }

  final case class Level[A, B](
    lower: StrictPrec[A],
    fixity: Fixity,
    operators: List[StrictParsley[Fixity#Op[A, B]]]
  ) extends StrictPrec[B] {
    type In = A
  }
}

private [deepembedding] final class Precedence[A](table: StrictPrec[A]) extends StrictParsley[A] {
  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = ???

  override private[deepembedding] def inlinable: Boolean = ???

  override private[deepembedding] def pretty: String = ???
}