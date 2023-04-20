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
        }}", cs.reconstruct) }
      )

    node
  }
}

private [debugger] sealed abstract class SometimesEquatable[A](val item: A) {
  override def toString: String = s"$item [?=]"
}

private [debugger] final class Equatable[A](item: A)
  extends SometimesEquatable[A](item) {
  override def equals(obj: Any): Boolean = obj match {
    case eq: Equatable[A] => eq.item.equals(item)
    case _                => false
  }

  override def hashCode(): Int = item.hashCode()
}

private [debugger] final class Referential[A](item: A)
  extends SometimesEquatable[A](item)

private [debugger] object SometimesEquatable {
  def equatable[A](item: A): SometimesEquatable[A] = new Equatable[A](item)
  def referential[A](item: A): SometimesEquatable[A] = new Referential[A](item)
}
