/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import parsley.Parsley
import parsley.Parsley.pure

import parsley.debugger.objects.{DebugContext, DebugGUI, DebugTree, DebugTreeBuilder, TransientDebugTree}

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.traverseDown

import scala.collection.mutable

/** Package containing various helpers and debug combinators.
  */
package object combinators {
  /** Attaches a debugger to a parser, returning a reference to the debug tree produced by
    * the parser's parse tree formed as it runs.
    *
    * Ideally, this is called on the highest level parser that you want to debug, as this combinator
    * will cascade downwards and apply debugging as far down as it can.
    *
    * @param parser The parser to debug.
    * @tparam A Output type of original parser.
    * @return A pair of the finalised tree, and the instrumented parser.
    */
  def attachDebugger[A](parser: Parsley[A]): (() => DebugTree, Parsley[A]) = {
    implicit val seen: mutable.Set[LazyParsley[_]] = new mutable.LinkedHashSet()
    implicit val context: DebugContext = new DebugContext()

    val attached = traverseDown(parser.internal)
    (() => rebuildMasterTree(parser.internal, context.getNodes), new Parsley(attached))
  }

  // Rebuild a full tree (with children, from scratch).
  private def rebuildMasterTree(orig: LazyParsley[_], trees: Map[List[LazyParsley[_]], TransientDebugTree]): DebugTree = {
    val asFlat = trees.toList.map { case (stk, t) => (stk.reverse, t) }.sortBy(_._1.size)
    val root = DebugTreeBuilder(TransientDebugTree(name = "ROOT"), Map.empty)

    asFlat.foldLeft(root)((tree, lp) => lp match {
      case (k, t) => tree.addNode(k, t)
    }).reconstruct
      .freeze
  }

  /** Attach a debugger and an implicitly-available GUI frontend in which the debug tree should be
    * rendered in.
    *
    * One would normally obtain a [[DebugGUI]] frontend from its respective package via a call to
    * `newInstance` or similar.
    *
    * @param parser The parser to debug.
    * @param gui The GUI frontend instance to render with.
    * @tparam A Output type of parser.
    * @return A modified parser which will ask the frontend to render the produced debug tree after
    *         a call to [[Parsley.parse]] is made.
    */
  def attachDebuggerGUI[A](parser: Parsley[A])(implicit gui: DebugGUI): Parsley[A] = {
    val (tree, attached) = attachDebugger(parser)

    // Ideally, this should run 'attached', and render the tree regardless if 'attached' succeeds or
    // not. However, Parsley.empty may not be a good idea as the error message from 'attached' will
    // get lost into the ether.
    // TODO: find a way to preserve the error message from 'attached' should it fail.
    (attached <~ pure(gui.render(tree()))) <|> (pure(gui.render(tree())) ~> Parsley.empty)
  }
}
