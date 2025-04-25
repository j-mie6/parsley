package parsley.internal.deepembedding.frontend

import parsley.expr.Fixity
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{ContAdapter}
import parsley.internal.deepembedding.Traverse.{traverse_ => ltraverse_}
import parsley.expr.Ops
import scala.annotation.unchecked.uncheckedVariance

sealed abstract class LazyOps[-A, B] {
  private [parsley] val f: Fixity
  private [parsley] val ops: List[LazyParsley[f.Op[A @uncheckedVariance, B]]]
}

object LazyOps {
  private [parsley] def apply[A, B](fixity: Fixity)(operators: Seq[LazyParsley[fixity.Op[A, B]]]) = new LazyOps[A, B] {
    val f: fixity.type = fixity
    val ops = operators.toList
  }

  private [parsley] def apply[A, B](ops: Ops[A,B]): LazyOps[A, B] = {
    val o = ops
    new LazyOps[A, B] {
      val f: o.f.type = o.f
      val ops = o.operators.toList.map(_.internal)
    }
  }
}

sealed trait LazyPrec[+Out]

object LazyPrec {
  final case class Atoms[A](atoms: List[LazyParsley[A]]) extends LazyPrec[A]
  
  final case class Level[A, B](
    lvls: LazyPrec[A],
    ops: LazyOps[A, B]
  ) extends LazyPrec[B] {
    type In = A
  }

  def fromPrec[A](table: parsley.expr.Prec[A]): LazyPrec[A] = {
    import parsley.expr.{Atoms => PAtoms, Level => PLevel}

    table match {
      case PAtoms(atom0, atoms @ _*) => Atoms((atom0 +: atoms).toList.map(_.internal))
      case PLevel(lower, ops) => Level(fromPrec(lower), LazyOps(ops))
    }
  }

  def traverse_[M[_, +_]: ContOps, R, B](
    table: LazyPrec[_]
  )(
    fAtom: LazyParsley[_] => M[R, B],
    fOp: LazyParsley[_] => M[R, B]
  ): M[R, Unit] = table match {
    case Atoms(atoms) => ltraverse_[M, R, LazyParsley[_], B](atoms)(fAtom)
    case Level(lower, lOps) => traverse_[M, R, B](lower)(fAtom, fOp) >> ltraverse_[M, R, LazyParsley[_], B](lOps.ops)(fOp)
  }
}