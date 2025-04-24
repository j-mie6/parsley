package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.frontend.LazyPrec.{Atoms, Level, traverse_ => traversePrec_}

object TraverseHelper {
  def traverse[M[_, +_]: ContOps, R, A, B](xs: List[A])(f: A => M[R, B]): M[R, List[B]] = xs match {
    case Nil => result(Nil)
    case x :: xs => for {
        y <- f(x)
        ys <- traverse(xs)(f)
    } yield y :: ys
  }

  def traverse_[M[_, +_]: ContOps, R, A, B](xs: List[A])(f: A => M[R, B]): M[R, Unit] = xs match {
    case Nil => result(())
    case x :: xs => f(x) >> traverse_(xs)(f)
  }
}

private [parsley] final class Precedence[A](table: LazyPrec[A]) extends LazyParsley[A] {
  override protected def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R,Unit] = {
    traversePrec_(
      table
    )(
      a => suspend[M, R, Unit](a.findLets(seen)),
      op => suspend[M, R, Unit](op.findLets(seen))
    )
  }

  override protected def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R,StrictParsley[A_]] = for {
    strictPrec <- buildStrictPrec[M, R, A](table)
  } yield new backend.Precedence(strictPrec)

  private def buildStrictPrec[M[_, +_]: ContOps, R, X](lp: LazyPrec[X])(implicit lets: LetMap): M[R, backend.StrictPrec[X]] = lp match {
    case Atoms(atoms) => TraverseHelper.traverse(atoms)(_.optimised[M, R, X]).map(backend.StrictPrec.Atoms(_))
    case lvl @ Level(lower, lOps) => {
      type Op = lOps.f.Op[lvl.In, X]
      for {
        strictLower <- buildStrictPrec(lower)
        strictOps <- TraverseHelper.traverse(lOps.ops.asInstanceOf[List[LazyParsley[Op]]])(op => suspend(op.optimised[M, R, Op]))
      } yield backend.StrictPrec.Level(strictLower, backend.StrictOps(lOps.f)(strictOps))
    }
  }

  override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T,U], context: T): U[A] = visitor.visit(this, context)(table)

  override private[parsley] var debugName: String = "precedence"

}