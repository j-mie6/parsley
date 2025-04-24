package parsley.internal.deepembedding.frontend

import parsley.expr.Fixity
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{ContAdapter}

sealed trait LazyPrec[+Out] {
  type In
}

object LazyPrec {
  final case class Atoms[A](atoms: List[LazyParsley[A]]) extends LazyPrec[A] {
    type In = A
  }
  
  final case class Level[A, B](
    lower: LazyPrec[A],
    fixity: Fixity,
    operators: List[LazyParsley[Fixity#Op[A, B]]]
  ) extends LazyPrec[B] {
    type In = A
  }

  def fromPrec[A](table: parsley.expr.Prec[A]): LazyPrec[A] = {
    import parsley.expr.{Atoms => PAtoms, Level => PLevel}

    table match {
      case PAtoms(atom0, atoms @ _*) => Atoms((atom0 +: atoms).toList.map(_.internal))
      case PLevel(lower, ops) => Level(fromPrec(lower), ops.f, ops.operators.toList.map(_.internal))
    }
  }

  def traverse_[M[_, +_]: ContOps, R, B](
    table: LazyPrec[_]
  )(
    fAtom: LazyParsley[_] => M[R, B],
    fOp: LazyParsley[_] => M[R, B]
  ): M[R, Unit] = table match {
    case Atoms(atoms) => TraverseHelper.traverse_[M, R, LazyParsley[_], B](atoms)(fAtom)
    case Level(lower, _, ops) => traverse_[M, R, B](lower)(fAtom, fOp) >> TraverseHelper.traverse_[M, R, LazyParsley[_], B](ops)(fOp)
  }
}