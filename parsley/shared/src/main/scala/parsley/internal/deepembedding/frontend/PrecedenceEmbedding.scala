package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{result, suspend, ContAdapter}
import parsley.expr.Fixity
import parsley.internal.deepembedding.frontend.LazyPrec.Atoms
import parsley.internal.deepembedding.frontend.LazyPrec.Level

// LazyPrec needs to include an operators table, containing operators (LazyParsleys), their fixity, and precedence
// It also needs to include an atoms list, with LazyParsley parsers for the atoms
// final case class LazyOperator[F <: Fixity, A, B](
//   fixity: F,
//   precedence: Int,
//   parser: LazyParsley[F#Op[A, B]]
// )

// final case class LazyPrec(operators: List[LazyOperator[_, _, _]], atoms: List[LazyParsley[_]])

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
}


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

object LazyPrecTraverseHelper {
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
    LazyPrecTraverseHelper.traverse_(
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