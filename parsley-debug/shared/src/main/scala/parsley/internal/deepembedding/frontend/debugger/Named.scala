package parsley.internal.deepembedding.frontend.debugger

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{ContAdapter, suspend}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LetFinderState, LetMap, RecMap}

// Wrapper parser class indicating explicitly named parsers
private [parsley] final case class Named[A]
  (par: LazyParsley[A], name: String) extends LazyParsley[A] {
  def make(p: StrictParsley[A]): StrictParsley[A] = p

  override def findLetsAux[Cont[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] =
    suspend(par.findLets(seen))

  override def preprocess[Cont[_, +_] : ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
    for (p <- suspend(par.optimised[Cont, R, A])) yield make(p)
}
