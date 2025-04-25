package parsley.internal.deepembedding

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

object Traverse {
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