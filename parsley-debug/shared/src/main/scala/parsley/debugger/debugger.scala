package parsley

import scala.collection.mutable

import parsley.Parsley.{attempt, empty, fresh}
import parsley.debugger.frontend.{ConsoleGUI, DebugGUI}
import parsley.debugger.internal.{DebugContext, DebugTreeBuilder, TransientDebugTree}

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.{traverseDown, Debugged}

/** This package contains the two main debug combinators, `attachDebugger` and `attachDebuggerGUI`. */
package object debugger {
  /** Attaches a debugger to a parser, returning a reference to the debug tree produced by
    * the parser's parse tree formed as it runs.
    *
    * Ideally, this is called on the highest level parser that you want to debug, as this combinator
    * will traverse downwards in the parser tree and attach debuggers to all the child parsers. As a
    * consequence of this, it is not really recommended to include debugged parsers as lower-level
    * parsers within your main parser, and rather to isolate their testing.
    *
    * Before running the parser, it may be advised to see if the current platform's implementation
    * of the debugger supports [[parsley.debugger.util.collectNames]], which allows you to input an
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
    * @example {{{
    * scala> // Assume ExprParser is an object that contains some named parsers that evaluate a
    * simple addition or subtraction expression and return the result of the expression.
    * scala> // Remember, collectNames is optional, but helps a lot when it is available.
    * scala> import parsley.debugger.util.collectNames
    * scala> import parsley.debugger.combinators.attachDebugger
    * scala> collectNames(ExprParser)
    * scala> val (tree, debugged) = attachDebugger(ExprParser.mainParser)
    * val tree: () => parsley.debugger.objects.DebugTree = parsley.debugger.combinator.package$$$Lambda$8617/0x000000080227d020@aaa80a5
    * val debugged: parsley.Parsley[Int] = parsley.Parsley@3c9e38eb
    * scala> debugged.parse("5 + 3 - 2")
    * val res1: parsley.Result[String,Int] = Success(6)
    * scala> println(tree())
    * [ mainParser ]: ("5 + 3 - 2", Success)
    * |
    * +-[ Chainl ]: ("5 + 3 - 2", Success)
    * |
    * +-[ *> ]: ("5", Success), (" 3", Success), (" 2", Success)
    * | |
    * | +-[ SkipMany ]: ("", Success), (" ", Success), (" ", Success)
    * | | |
    * | | +-[ ErrorLabel ]: ("5", Failure), (" ", Success), ("3", Failure), (" ", Success), ("2", Failure)
    * | |   |
    * | |   +-[ Satisfy ]: ("5", Failure), (" ", Success), ("3", Failure), (" ", Success), ("2", Failure)
    * | |
    * | +-[ positiveNum ]: ("5", Success), ("3", Success), ("2", Success)
    * |   |
    * |   +-[ Pure ]: ("", Success), ("", Success), ("", Success)
    * |   |
    * |   +-[ Many ]: ("5", Success), ("3", Success), ("2", Success)
    * |     |
    * |     +-[ Satisfy ]: ("5", Success), (" ", Failure), ("3", Success), (" ", Failure), ("2", Success), ("", Failure)
    * |
    * +-[ *> ]: (" +", Success), (" -", Success), ("", Failure)
    * |
    * +-[ SkipMany ]: (" ", Success), (" ", Success), ("", Success)
    * | |
    * | +-[ ErrorLabel ]: (" ", Success), ("+", Failure), (" ", Success), ("-", Failure), ("", Failure)
    * |   |
    * |   +-[ Satisfy ]: (" ", Success), ("+", Failure), (" ", Success), ("-", Failure), ("", Failure)
    * |
    * +-[ mainParser ]: ("+", Success), ("-", Success), ("", Failure)
    * |
    * +-[ *> ]: ("+", Success), ("-", Failure), ("", Failure)
    * | |
    * | +-[ CharTok ]: ("+", Success), ("-", Failure), ("", Failure)
    * | |
    * | +-[ Pure ]: ("", Success)
    * |
    * +-[ *> ]: ("-", Success), ("", Failure)
    * |
    * +-[ CharTok ]: ("-", Success), ("", Failure)
    * |
    * +-[ Pure ]: ("", Success)
    * }}}
    * @param parser The parser to debug.
    * @tparam A Output type of original parser.
    * @return A pair of the finalised tree, and the instrumented parser.
    */
  def attachDebugger[A](parser: Parsley[A]): (() => DebugTree, Parsley[A]) = {
    implicit val seen: mutable.Map[LazyParsley[_], Debugged[_]] = new mutable.LinkedHashMap()
    implicit val context: DebugContext = new DebugContext()

    val attached = traverseDown(parser.internal)
    (() => rebuildMasterTree(context.getNodes),
      fresh(context.reset()) *> new Parsley(attached))
  }

  // Helper for rebuilding full trees (with children, from scratch).
  private def rebuildMasterTree(trees: Map[List[LazyParsley[_]], TransientDebugTree]): DebugTree = {
    // Reverse is required for the overall list generated from the tree map as parsers are pushed into
    // the linked map LIFO, but we want a FIFO ordering before length sort.
    // Pre: the Scala implementation's sort method uses a stable sort.
    val asFlat = trees.toList.reverse.map { case (stk, t) => (stk.reverse, t) }.sortBy(_._1.size)

    // This root node is required as a sort of building block to build the rest of the tree off of,
    // and will be discarded later to return its sole child.
    val root = DebugTreeBuilder(TransientDebugTree(name = "ROOT", ""), Map.empty)

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
    * `newInstance` or similar. [[ConsoleGUI]] is included as a fallback option.
    *
    * The instrumented parser will automatically call the GUI to render the debug tree.
    *
    * See [[attachDebugger]] for more information on how attachment works and things you may want
    * to do before using this debug combinator. The example there also mostly applies here, where
    * the only differences are that the tree is not exposed to you, and will be automatically
    * rendered by the implicitly given backend after parsing.
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
    val renderer = fresh {
      val frozen = tree()
      val input  = frozen.fullInput

      gui.render(input, frozen)
    }

    attempt(attached <* renderer) <|> (renderer *> empty)
  }
}
