package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec

import parsley.expr.Fixity
import parsley.expr.{Prec, Atoms, Level}

private [parsley] case class LazyPrec(atoms: List[LazyParsley[Any]], ops: List[LazyOp], wraps: List[Any => Any])

private [parsley] case class LazyOp(fixity: Fixity, op: LazyParsley[Any], prec: Int)

object LazyPrec {
  def apply(table: Prec[_]): LazyPrec = fromPrec(table, 0)

  @tailrec
  def fromPrec(table: Prec[_], level: Int = 0, accAtoms: List[LazyParsley[Any]] = Nil, accOps: List[LazyOp] = Nil, accWraps: List[Any => Any] = Nil): LazyPrec = table match {
    case Atoms(atom0, atoms @ _*) => LazyPrec((atom0 +: atoms).toList.map(_.internal) ++ accAtoms, accOps, accWraps)
    case Level(lower, ops) => {
      val newOps = ops.operators.map(op => LazyOp(ops.f, op.internal, level))
      fromPrec(lower, level + 1, accAtoms, accOps ++ newOps, accWraps :+ ops.wrap)
    }
  }
}