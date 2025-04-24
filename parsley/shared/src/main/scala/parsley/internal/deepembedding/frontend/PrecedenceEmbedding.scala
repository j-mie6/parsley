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

  // override protected def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R,Unit] = {
  //   TraverseHelper.traverse_(table.atoms)(a => suspend[M, R, Unit](a.findLets(seen))) >>
  //   TraverseHelper.traverse_(table.operators)(op => suspend[M, R, Unit](op.parser.findLets(seen)))
  // }

  // override protected def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R,StrictParsley[A_]] = {
  //   for {
  //     atoms <- TraverseHelper.traverse[M, R, LazyParsley[_], StrictParsley[_]](table.atoms)(_.optimised[M, R, Any])
  //     ops <- TraverseHelper.traverse[M, R, LazyOperator[_, _], backend.StrictOperator[_, _]](table.operators) { op =>
  //       for { 
  //         p <- op.parser.optimised[M, R, op.fixity.Op[_, _]]
  //       } yield new backend.StrictOperator[op.fixity.Op[_, _], op.fixity.Op[_, _]] {
  //         val fixity = op.fixity
  //         val precedence = op.precedence
  //         val parser: StrictParsley[op.fixity.Op[_, _]] = p
  //       }
  //     }
  //   } yield new backend.Precedence(new backend.StrictPrec(ops, atoms))
  // }

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
    case lvl @ Level(lower, fixity, ops) => {
      type Op = fixity.Op[lvl.In, X]
      for {
        strictLower <- buildStrictPrec(lower)
        strictOps <- TraverseHelper.traverse(ops.asInstanceOf[List[LazyParsley[Op]]])(op => suspend(op.optimised[M, R, Op]))
      } yield backend.StrictPrec.Level(strictLower, fixity, strictOps)
    }
  }

  override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T,U], context: T): U[A] = visitor.visit(this, context)(table)

  override private[parsley] var debugName: String = "precedence"

}