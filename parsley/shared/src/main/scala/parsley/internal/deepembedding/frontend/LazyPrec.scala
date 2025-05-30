package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec

import parsley.expr.Fixity
import parsley.expr.{Prec, Atoms, Level}

private [parsley] case class LazyOp(fixity: Fixity, op: LazyParsley[Any], prec: Int)

private [parsley] case class LazyPrec(atoms: List[LazyParsley[Any]], ops: List[LazyOp], wraps: List[Any => Any])

object LazyPrec {
  def apply(table: Prec[_]): LazyPrec = fromPrec(table)

  @tailrec
  def fromPrec(table: Prec[_], level: Int = 0, accOps: List[LazyOp] = Nil, accWraps: List[Any => Any] = Nil): LazyPrec = table match {
    case Atoms(atom0, atoms @ _*) => LazyPrec((atom0 +: atoms).toList.map(_.internal), accOps, accWraps)
    case Level(lower, ops) => {
      val newOps = ops.operators.map(op => LazyOp(ops.f, op.internal, level))
      fromPrec(lower, level + 1, newOps.toList ++ accOps, accWraps :+ ops.wrap)
    }
  }

  // def fromPrec(table: Prec[_], level: Int = 0): LazyPrec = table match {
  //   case Atoms(atom0, atoms @ _*) => new LazyPrec((atom0 +: atoms).toList.map(_.internal), Nil, Nil)
  //   case Level(lower, ops) => {
  //     val LazyPrec(lowerAtoms, lowerOps, lowerWraps) = fromPrec(lower, level + 1)
  //     val newOps = ops.operators.map(op => LazyOp(ops.f, op.internal, level))
  //     new LazyPrec(lowerAtoms, lowerOps ++ newOps, ops.wrap +: lowerWraps)
  //   }
  // }
}