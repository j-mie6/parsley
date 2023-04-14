/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import scala.collection.mutable

import parsley.Parsley
import parsley.Parsley.fresh
import parsley.debugger.frontend.DebugGUI
import parsley.debugger.objects.{DebugContext, DebugTree, DebugTreeBuilder, TransientDebugTree}

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.{traverseDown, Debugged}

/** Package containing various helpers and debug combinators.
  */
package object combinators {
  /** Attaches a debugger to a parser, returning a reference to the debug tree produced by
    * the parser's parse tree formed as it runs.
    *
    * Ideally, this is called on the highest level parser that you want to debug, as this combinator
    * will cascade downwards and apply debugging as far down as it can.
    *
    * After instrumentation, you would run the instrumented parser first, then after the parser
    * finishes running on ONE input ideally, you would run the debug tree generator function in
    * order to acquire the debug tree.
    *
    * @param parser The parser to debug.
    * @tparam A Output type of original parser.
    * @return A pair of the finalised tree, and the instrumented parser.
    */
  def attachDebugger[A](parser: Parsley[A]): (() => DebugTree, Parsley[A]) = {
    implicit val seen: mutable.Map[LazyParsley[_], Debugged[_]] = new mutable.LinkedHashMap()
    implicit val context: DebugContext = new DebugContext()

    val attached = traverseDown(parser.internal)
    (() => rebuildMasterTree(context.getNodes), new Parsley(attached))
  }

  // Helper for rebuilding full trees (with children, from scratch).
  private def rebuildMasterTree(trees: Map[List[LazyParsley[_]], TransientDebugTree]): DebugTree = {
    // Reverse is required for the overall list generated from the tree map as parsers are pushed into
    // the linked map LIFO, but we want a FIFO ordering before length sort.
    // Pre: the Scala implementation's sort method uses a stable sort.
    val asFlat = trees.toList.reverse.map { case (stk, t) => (stk.reverse, t) }.sortBy(_._1.size)
    val root = DebugTreeBuilder(TransientDebugTree(name = "ROOT"), Map.empty)

    // Construct the root tree, which will be stripped later.
    val frozen = asFlat.foldLeft(root)((tree, lp) => lp match {
      case (k, t) => tree.addNode(k, t)
    }).reconstruct
      .freeze

    // Extract the first node in the root tree, which should be the only node child of the
    // root tree.
    frozen.nodeChildren(frozen.nodeChildren.keys.collectFirst(s => s).get)
  }

  /** Attach a debugger and an implicitly-available GUI frontend in which the debug tree should be
    * rendered in.
    *
    * One would normally obtain a [[DebugGUI]] frontend from its respective package via a call to
    * `newInstance` or similar.
    *
    * The instrumented parser will automatically call the GUI to render the debug tree.
    *
    * @param parser The parser to debug.
    * @param gui The GUI frontend instance to render with.
    * @tparam A Output type of parser.
    * @return A modified parser which will ask the frontend to render the produced debug tree after
    *         a call to [[Parsley.parse]] is made.
    */
  def attachDebuggerGUI[A](parser: Parsley[A])(implicit gui: DebugGUI): Parsley[A] = {
    val (tree, attached) = attachDebugger(parser)

    // Ideally, this should run 'attached', and render the tree regardless of the parser's success.
    attached <~ fresh(gui.render(tree()))
  }
}
