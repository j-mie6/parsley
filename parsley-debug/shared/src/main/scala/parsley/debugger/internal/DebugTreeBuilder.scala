/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

import parsley.internal.deepembedding.frontend.LazyParsley

// Helper class for reconstructing a debug tree.
// Not meant to be public.
private [parsley] case class DebugTreeBuilder(
  foundName: Option[String],
  node: TransientDebugTree,
  bChildren: mutable.Map[Unique[LazyParsley[Any]], DebugTreeBuilder] = mutable.LinkedHashMap()
) {
  private var uid = 0

  def reconstruct: TransientDebugTree = {
    node.children
      .addAll(
        bChildren.map { case (lp, cs) => (Rename(foundName, lp()) + s"-#${{
          val uuid = uid
          uid = uid + 1
          uuid
        }}", cs.reconstruct) }.toList.reverse
      )

    node
  }
}
