package parsley.debugger.internal

import scala.collection.mutable

import parsley.internal.deepembedding.frontend.LazyParsley

// Helper class for reconstructing a debug tree.
// Not meant to be public.
private [parsley] case class DebugTreeBuilder(
  node: TransientDebugTree,
  bChildren: mutable.Map[SometimesEquatable[LazyParsley[Any]], DebugTreeBuilder] = mutable.LinkedHashMap()
) {
  private var uid = 0

  def reconstruct: TransientDebugTree = {
    node.children
      .addAll(
        bChildren.map { case (lp, cs) => (Rename(lp.item) + s"-#${{
          val uuid = uid
          uid = uid + 1
          uuid
        }}", cs.reconstruct) }.toList.reverse
      )

    node
  }
}
