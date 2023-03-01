/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import parsley.Parsley
import parsley.Parsley.pure
import parsley.debugger.objects.{DebugGUI, TransientDebugTree}

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.traverseDown

import scala.collection.mutable

/** Package containing various helpers and debug combinators.
  */
package object combinators {
  /** Attaches a debugger to a parser, returning a reference to the mutable debug tree produced by
    * the shape of the parser.
    *
    * Ideally, this is called on the highest level parser that you want to debug, as this combinator
    * will cascade downwards and apply debugging as far down as it can.
    *
    * Remember to freeze the debug tree after running the parser before analysis!
    *
    * @param parser The parser to debug.
    * @tparam A Output type of parser.
    * @return Reference to the mutable debug tree. This is only populated after [[Parsley.parse]] is
    *         executed for the parser being debugged.
    */
  def attachDebugger[A](parser: Parsley[A]): (TransientDebugTree, Parsley[A]) = {
    implicit val seen: mutable.Set[LazyParsley[_]] = new mutable.HashSet[LazyParsley[_]]()
    val (tree, lazyAttached) = traverseDown(parser.internal)
    (tree, new Parsley[A](lazyAttached))
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
    attached <~ pure(gui.render(tree.freeze))
  }
}
