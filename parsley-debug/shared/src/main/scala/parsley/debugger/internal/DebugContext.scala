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
  // Tracks how many parsers deep we are.
  //  private var currentParserStack: List[LazyParsley[_]] = Nil
  //
  //  private val nodes: mutable.Map[List[LazyParsley[_]], TransientDebugTree] = new mutable.LinkedHashMap()

  private var builderStack: ListBuffer[DebugTreeBuilder] =
    ListBuffer(DebugTreeBuilder(TransientDebugTree("ROOT", "ROOT", "NIL")))

  //  // Get an immutable map of nodes.
  //  def getNodes: Map[List[LazyParsley[_]], TransientDebugTree] =
  //    nodes.foldRight[ListMap[List[LazyParsley[_]], TransientDebugTree]](ListMap())((p, acc) => acc + p)

  // Get the final DebugTreeBuilder from this context.
  def getFinalBuilder: DebugTreeBuilder =
    builderStack.head.bChildren.collectFirst { case (_, x) => x }.get

  // Add an attempt of parsing at the current stack point.
  def addParseAttempt(attempt: ParseAttempt): Unit =
    builderStack.head.node.parses.append(attempt)
  //    currentParserStack match {
  //      case Nil    =>
  //        // This shouldn't ever be reached unless something horribly wrong happened to the
  //        // instruction generation or execution of the parser.
  //        println("WARNING: parser stack underflow when adding attempt.")
  //      case p :: _ =>
  //        // This tree will be populated as the parser is run.
  //        // The name of the parser will be the class name of the parser, translated into
  //        // something more human-friendly.
  //        val tree = nodes.getOrElseUpdate(currentParserStack, {
  //          val newTree = TransientDebugTree(fullInput = fullInput)
  //          newTree.name = Rename(p)
  //          newTree.internal = Rename.partial(p)
  //          newTree
  //        })
  //
  //        tree.parses.append(attempt)
  //    }

  // Reset this context back to zero.
  def reset(): Unit = {
    builderStack = ListBuffer(DebugTreeBuilder(TransientDebugTree("ROOT", "ROOT", "NIL")))
  }

  private def mapHead[A](f: A => A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case y :: ys => f(y) :: ys
  }

  // Push a new parser onto the parser callstack.
  def push(fullInput: String, parser: LazyParsley[_]): Unit =
    if (builderStack.head.bChildren.contains(parser)) {
      builderStack.prepend(builderStack.head.bChildren(parser))
    } else {
      val newTree = TransientDebugTree(fullInput = fullInput)
      newTree.name = Rename(parser)
      newTree.internal = Rename.partial(parser)

      val dtb = DebugTreeBuilder(newTree)

      builderStack.head.bChildren(parser) = dtb
      builderStack.prepend(dtb)
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
