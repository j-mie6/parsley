/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.objects

import scala.collection.mutable

import parsley.debugger.internal.Rename

import parsley.internal.deepembedding.frontend.LazyParsley

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside a clozure.
private [parsley] class DebugContext {
  // Tracks how many parsers deep we are.
  private var currentParserStack: List[LazyParsley[_]] = Nil

  private val nodes: mutable.Map[List[LazyParsley[_]], TransientDebugTree] = new mutable.LinkedHashMap()

  // Get an immutable map of nodes.
  def getNodes: Map[List[LazyParsley[_]], TransientDebugTree] =
    nodes.toMap

  // Add an attempt of parsing at the current stack point.
  def addParseAttempt(input: String, success: Boolean): Unit =
    currentParserStack match {
      case Nil    =>
        println("WARNING: parser stack underflow when adding attempt.")
      case p :: _ =>
        // This tree will be populated as the parser is run.
        // The name of the parser will be the class name of the parser, translated into
        // something more human-friendly.
        val tree = nodes.getOrElseUpdate(currentParserStack, {
          val newTree = TransientDebugTree()
          newTree.name = Rename(p)
          newTree
        })

        tree.parses.append((input, success))
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
