package parsley.internal.deepembedding.backend

import parsley.expr.Fixity
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.Seq

sealed abstract class StrictOps[-A, B] {
    private [parsley] val f: Fixity
    private [parsley] val ops: List[StrictParsley[f.Op[A @uncheckedVariance, B]]]
}

object StrictOps {
    private [parsley] def apply[A, B](fixity: Fixity)(operators: Seq[StrictParsley[fixity.Op[A, B]]]) = new StrictOps[A, B] {
    val f: fixity.type = fixity
    val ops = operators.toList
  }
}

sealed trait StrictPrec[+Out]

object StrictPrec {
  final case class Atoms[A](atoms: List[StrictParsley[A]]) extends StrictPrec[A]

  final case class Level[A, B](
    lower: StrictPrec[A],
    ops: StrictOps[A, B]
  ) extends StrictPrec[B] {
    type In = A
  }
}