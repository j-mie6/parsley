package parsley.internal.deepembedding.frontend.debugger

import parsley.debugger.objects.DebugContext

import parsley.internal.deepembedding.ContOps.{ContAdapter, suspend}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.{backend, ContOps}
import parsley.internal.deepembedding.frontend.{LazyParsley, LetFinderState, LetMap, RecMap}

// Wrapper class signifying debugged classes
private [parsley] final class Debugged[A]
  (val origin: LazyParsley[A], var par: Option[LazyParsley[A]])
  (implicit dbgCtx: DebugContext) extends LazyParsley[A] {
  def make(p: StrictParsley[A]): StrictParsley[A] =
    new backend.debugger.Debugged(origin, p)

  def parser: LazyParsley[A] = par.get

  override def findLetsAux[Cont[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] =
    suspend(par.get.findLets(seen))

  override def preprocess[Cont[_, +_] : ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
    for (p <- suspend(par.get.optimised[Cont, R, A])) yield make(p)

  // Shortcuts to the given parser's name instead.
  def getTypeName: String = par.getClass.getTypeName
}
