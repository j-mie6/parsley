package parsley.internal.deepembedding.frontend.debugger

import parsley.debugger.objects.DebugContext

import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, Unary}

import parsley.internal.deepembedding.backend

// Wrapper class signifying debugged classes
private [parsley] final class Debugged[A]
  (val par: LazyParsley[A])
  (implicit dbgCtx: DebugContext) extends Unary[A, A](par) {
  override def make(p: StrictParsley[A]): StrictParsley[A] =
    new backend.debugger.Debugged(par, p)

  // Shortcuts to the given parser's name instead.
  def getTypeName: String = par.getClass.getTypeName
}
