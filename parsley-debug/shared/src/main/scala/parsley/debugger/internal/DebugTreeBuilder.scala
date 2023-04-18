package parsley.debugger.internal

import scala.collection.immutable.ListMap

import parsley.internal.deepembedding.frontend.LazyParsley

// Helper class for reconstructing a debug tree.
// Not meant to be public.
private [parsley] case class DebugTreeBuilder(
  node: TransientDebugTree,
  bChildren: Map[LazyParsley[Any], DebugTreeBuilder] = ListMap.empty,
) {
  private var uid = 0

  def addNode(path: List[LazyParsley[Any]], node: TransientDebugTree): DebugTreeBuilder =
    path match {
      case Nil      => DebugTreeBuilder(node, ListMap.empty)
      case p :: ps  =>
        // Pre: The path to this node must fully exist.
        // Tip: Add the shortest paths first!
        val child = this.bChildren.getOrElse(p, DebugTreeBuilder(node))
        DebugTreeBuilder(this.node, this.bChildren + ((p, child.addNode(ps, node))))
    }

  def reconstruct: TransientDebugTree = {
    node.children
      .addAll(
        bChildren.map { case (lp, cs) => (Rename(lp) + s"-#${{
          val uuid = uid
          uid = uid + 1
          uuid
        }}", cs.reconstruct) }
      )

    node
  }
}
