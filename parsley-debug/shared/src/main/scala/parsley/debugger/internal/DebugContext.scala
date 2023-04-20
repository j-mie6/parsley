/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.immutable.ListMap
import scala.collection.mutable
import parsley.debugger.ParseAttempt
import parsley.internal.deepembedding.frontend.LazyParsley

import scala.collection.mutable.ListBuffer

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside an implicit variable.
private [parsley] class DebugContext {
  // Tracks where we are in the parser callstack.
  private var builderStack: ListBuffer[DebugTreeBuilder] =
    ListBuffer(DebugTreeBuilder(TransientDebugTree("ROOT", "ROOT", "NIL")))

  // Get the final DebugTreeBuilder from this context.
  def getFinalBuilder: DebugTreeBuilder =
    builderStack.head.bChildren.collectFirst { case (_, x) => x }.get

  // Add an attempt of parsing at the current stack point.
  def addParseAttempt(attempt: ParseAttempt): Unit =
    builderStack.head.node.parses.append(attempt)

  // Reset this context back to zero.
  def reset(): Unit = {
    builderStack = ListBuffer(DebugTreeBuilder(TransientDebugTree("ROOT", "ROOT", "NIL")))
  }

  private def mapHead[A](f: A => A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case y :: ys => f(y) :: ys
  }

  // Push a new parser onto the parser callstack.
  def push(fullInput: String, parser: LazyParsley[_], iterative: Boolean): Unit = {
    lazy val eq: SometimesEquatable[LazyParsley[_]] = SometimesEquatable.equatable(parser)
    lazy val ref: SometimesEquatable[LazyParsley[_]] = SometimesEquatable.referential(parser)

    if (builderStack.head.bChildren.contains(eq)) {
      builderStack.prepend(builderStack.head.bChildren(eq))
    } else {
      val newTree = TransientDebugTree(fullInput = fullInput)
      newTree.name = Rename(parser)
      newTree.internal = Rename.partial(parser)

      val dtb = DebugTreeBuilder(newTree)

      builderStack.head.bChildren(if (iterative) ref else eq) = dtb
      builderStack.prepend(dtb)
    }
  }

  // Pop a parser off the parser callstack.
  def pop(): Unit =
    if (builderStack.isEmpty) {
      // Shouldn't happen, but just in case.
      println("WARNING: parser stack underflow on pop.")
    } else {
      // Remove first item.
      builderStack.remove(0)
    }
}
