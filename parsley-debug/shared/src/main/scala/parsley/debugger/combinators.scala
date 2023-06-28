/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import scala.collection.mutable

import parsley.Parsley
import parsley.Parsley.{attempt, empty, fresh}
import parsley.debugger.frontend.DebugGUI
import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.{Debugged, Named}
import parsley.internal.deepembedding.frontend.debugger.helpers.traverseDown

/** This object contains the two main debug combinators, `attachDebugger` and `attachDebuggerGUI`. */
object combinators {
  /** Attaches a debugger to a parser, returning a reference to the debug tree produced by
    * the parser's parse tree formed as it runs.
    *
    * Ideally, this is called on the highest level parser that you want to debug, as this combinator
    * will traverse downwards in the parser tree and attach debuggers to all the child parsers. As a
    * consequence of this, it is not really recommended to include debugged parsers as lower-level
    * parsers within your main parser, and rather to isolate their testing.
    *
    * Before running the parser, it may be advised to see if the current platform's implementation
    * of the debugger supports [[parsley.debugger.util.Collectors]], which allows you to input an
    * object or class instance containing parsers, in order to analyse it via reflection to assign
    * the names given to the parser fields / methods in that object to disambiguate the parser names
    * in the tree in a much easier manner.
    *
    * Without that, many parsers will appear as `packageanon` or similar, which can make the
    * debugging process much harder.
    * will cascade downwards and apply debugging as far down as it can.
    *
    * After instrumentation, you would run the instrumented parser first, then after the parser
    * finishes running on ONE input ideally, you would run the debug tree generator function in
    * order to acquire the debug tree.
    *
    * This tree can then be introspected manually in a (runtime) debugger, or passed over to one
    * of the GUI frontends in [[parsley.debugger.frontend]] in one of the extra companion libraries
    * for this debugger.
    *
    * It is recommended that you save the debug trees somewhere should you want to re-use the same
    * debugged parser, or if you want to use the same debugged parser as a child parser on
    * multiple parts of your main parser, make sure you use [[attachReusable]], as re-use of the
    * same debugged parser across multiple parent parsers will cause the different parse trees to
    * incorrectly merge in an undefined manner.
    *
    * A small warning: debugging an already debugged parser (via [[attachDebugger]] and friends) is
    * an undefined behaviour.
    *
    * See [[attachDebuggerGUI]] to automate the GUI rendering process after parsing.
    *
    * @param parser The parser to debug.
    * @tparam A Output type of original parser.
    * @return A pair of the finalised tree, and the instrumented parser.
    */
  def attachDebugger[A](parser: Parsley[A]): (() => DebugTree, Parsley[A]) = {
    implicit val seen: mutable.Map[LazyParsley[_], Debugged[_]] = new mutable.LinkedHashMap()
    implicit val context: DebugContext = new DebugContext()

    val attached = traverseDown(parser.internal)
    (() => context.getFinalBuilder.reconstruct, fresh(context.reset()) *> new Parsley(attached))
  }

  /** Create a closure that freshly attaches a debugger to a parser every time it is called.
    * This is used for creating debugged parsers that can be used as children to multiple parent
    * parsers, as using the same debugged parser as a child to multiple parsers is unsafe.
    *
    * See [[attachDebugger]] for more information.
    *
    * @return Generator closure for debugged versions of the input parser.
    */
  def attachReusable[A](parser: Parsley[A]): () => (() => DebugTree, Parsley[A]) =
    () => attachDebugger(parser)

  /** Attach a debugger and an explicitly-available GUI frontend in which the debug tree should be
    * rendered in.
    *
    * You would normally obtain a [[parsley.debugger.frontend.DebugGUI]] frontend from its
    * respective package as either a static object or an instance object depending on whether the
    * renderer stores state. In the latter case, it is better to regenerate the GUI with every new
    * debugged parser.
    *
    * The instrumented parser will automatically call the GUI to render the debug tree, so it may
    * be recommended that you only use this with smaller parsers as large parsers may cause large
    * amounts of memory to be used for rendering the tree.
    *
    * See [[attachDebugger]] for more information on how attachment works and things you may want
    * to do before using this debug combinator, as well as some warnings on what not to do when
    * using this debugger (such as regarding re-use of debugged parsers).
    *
    * @param parser The parser to debug.
    * @param gui    The GUI frontend instance to render with.
    * @tparam A Output type of parser.
    * @return A modified parser which will ask the frontend to render the produced debug tree after
    *         a call to [[Parsley.parse]] is made.
    */
  def attachDebuggerGUI[A](parser: Parsley[A], gui: DebugGUI): Parsley[A] = {
    val (tree, attached) = attachDebugger(parser)

    // Ideally, this should run 'attached', and render the tree regardless of the parser's success.
    val renderer = fresh {
      val frozen = tree()
      val input = frozen.fullInput

      gui.render(input, frozen)
    }

    attempt(attached <* renderer) <|> (renderer *> empty)
  }

  /** Create a closure that freshly attaches a debugger and a tree-rendering GUI to a parser every
    * time it is called.
    *
    * See [[attachReusable]] for more information.
    *
    * @return Generator closure for GUI-debugged versions of the input parser.
    */
  def attachReusableGUI[A](parser: Parsley[A], gui: () => DebugGUI): () => Parsley[A] =
    () => attachDebuggerGUI(parser, gui())

  /** Attach a debugger and an implicitly-available GUI frontend in which the debug tree should be
    * rendered in. See [[attachDebuggerGUI]] for more information.
    */
  def attachDebuggerIGUI[A](parser: Parsley[A])(implicit gui: DebugGUI): Parsley[A]
    = attachDebuggerGUI(parser, gui)

  /** Attach a name to a parser, for display within the debugger output.
    * This name has a higher precedence than names collected with [[parsley.debugger.util.Collectors]].
    */
  def named[A](parser: Parsley[A], name: String): Parsley[A] =
    parser.internal match {
      case Named(i, _) => new Parsley(Named(i.asInstanceOf[LazyParsley[A]], name))
      case _           => new Parsley(Named(parser.internal, name))
    }

  /** Dot accessor versions of the combinators, in case that is your preference. */
  implicit class DebuggerOps[A](par: Parsley[A]) {
    /** Dot accessor version of [[combinators.attachDebugger]]. */
    def attachDebugger: (() => DebugTree, Parsley[A]) =
      combinators.attachDebugger(par)

    /** Dot accessor version of [[combinators.attachReusable]]. */
    def attachReusable: () => (() => DebugTree, Parsley[A]) =
      combinators.attachReusable(par)

    /** Dot accessor version of [[combinators.attachDebuggerGUI]]. */
    def attachDebuggerGUI(gui: DebugGUI): Parsley[A] =
      combinators.attachDebuggerGUI(par, gui)

    /** Dot accessor version of [[combinators.attachReusableGUI]]. */
    def attachReusableGUI(gui: () => DebugGUI): () => Parsley[A] =
      combinators.attachReusableGUI(par, gui)

    /** Dot accessor version of [[combinators.attachDebuggerIGUI]]. */
    def attachDebuggerIGUI(implicit gui: DebugGUI): Parsley[A] =
      combinators.attachDebuggerIGUI(par)

    /** Dot accessor version of [[combinators.named]]. */
    def named(name: String): Parsley[A] =
      combinators.named(par, name)
  }
}
