package parsley.debugger

import scala.collection.mutable

import parsley.Parsley
import parsley.Parsley.{attempt, empty, fresh}
import parsley.debugger.frontend.{ConsoleGUI, DebugGUI}
import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.Debugged
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
    * of the GUI frontends in [[parsley.debugger.frontend]]. A console frontend is provided as
    * [[ConsoleGUI]] should the current platform not provide a dedicated GUI renderer.
    *
    * It is recommended that you save the debug trees somewhere should you want to re-use the same
    * debugged parser.
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

  /** Attach a debugger and an explicitly-available GUI frontend in which the debug tree should be
    * rendered in.
    *
    * One would normally obtain a [[DebugGUI]] frontend from its respective package as either a
    * static object or an instance object depending on whether the renderer stores state. In the
    * latter case, it is better to regenerate the GUI with every new debugged parser.
    *
    * The instrumented parser will automatically call the GUI to render the debug tree.
    *
    * See [[attachDebugger]] for more information on how attachment works and things you may want
    * to do before using this debug combinator.
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

  /** Attach a debugger and an implicitly-available GUI frontend in which the debug tree should be
    * rendered in. See [[attachDebuggerGUI]] for more information.
    */
  def attachDebuggerIGUI[A](parser: Parsley[A])(implicit gui: DebugGUI): Parsley[A]
    = attachDebuggerGUI(parser, gui)
}
