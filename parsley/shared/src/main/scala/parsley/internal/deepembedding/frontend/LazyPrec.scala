package parsley.internal.deepembedding.frontend

import parsley.expr.Fixity
import parsley.expr.{Prec, Atoms, Level}

private [parsley] case class LazyPrec(atoms: List[LazyParsley[Any]], ops: List[LazyOp], wraps: List[(Any => Any, Boolean)])

private [parsley] case class LazyOp(fixity: Fixity, op: LazyParsley[Any], prec: Int)

object LazyPrec {
  def apply(table: Prec[_]): LazyPrec = fromPrec(table, 0)

  def fromPrec(table: Prec[_], level: Int = 0): LazyPrec = table match {
    case Atoms(atom0, atoms @ _*) => new LazyPrec((atom0 +: atoms).toList.map(_.internal), Nil, Nil)
    case Level(lower, ops) => {
      val LazyPrec(lowerAtoms, lowerOps, lowerWraps) = fromPrec(lower, level + 1)
      val newOps = ops.operators.map(op => LazyOp(ops.f, op.internal, level))
      new LazyPrec(lowerAtoms, lowerOps ++ newOps, (ops.wrap, ops.wrapIsIdentity) +: lowerWraps)
    }
  }
}