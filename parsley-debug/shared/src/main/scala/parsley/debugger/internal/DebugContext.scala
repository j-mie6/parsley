/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import parsley.debugger.ParseAttempt

import scala.collection.immutable.ListMap
import scala.collection.mutable
import parsley.internal.deepembedding.frontend.LazyParsley

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside an implicit variable.
private [parsley] class DebugContext {
  // Tracks how many parsers deep we are.
  private var currentParserStack: List[LazyParsley[_]] = Nil

  private val nodes: mutable.Map[List[LazyParsley[_]], TransientDebugTree] = new mutable.LinkedHashMap()

  // Get an immutable map of nodes.
  def getNodes: Map[List[LazyParsley[_]], TransientDebugTree] =
    nodes.foldRight[ListMap[List[LazyParsley[_]], TransientDebugTree]](ListMap())((p, acc) => acc + p)

  // Add an attempt of parsing at the current stack point.
  def addParseAttempt(fullInput: String, attempt: ParseAttempt): Unit =
    currentParserStack match {
      case Nil    =>
        // This shouldn't ever be reached unless something horribly wrong happened to the
        // instruction generation or execution of the parser.
        println("WARNING: parser stack underflow when adding attempt.")
      case p :: _ =>
        // This tree will be populated as the parser is run.
        // The name of the parser will be the class name of the parser, translated into
        // something more human-friendly.
        val tree = nodes.getOrElseUpdate(currentParserStack, {
          val newTree = TransientDebugTree(fullInput = fullInput)
          newTree.name = Rename(p)
          newTree.internal = Rename.partial(p)
          newTree
        })

        tree.parses.append(attempt)
    }

  // Reset this context back to zero.
  def reset(): Unit = {
    currentParserStack = Nil
    nodes.clear()
  }

  // Push a new parser onto the parser callstack.
  def push(parser: LazyParsley[_]): Unit =
    currentParserStack = parser :: currentParserStack

  // Pop a parser off the parser callstack.
  def pop(): Unit =
    currentParserStack = currentParserStack match {
      case Nil       =>
        // Shouldn't happen, but just in case.
        println("WARNING: parser stack underflow on pop.")
        Nil
      case _ :: rest =>
        rest
    }
}
