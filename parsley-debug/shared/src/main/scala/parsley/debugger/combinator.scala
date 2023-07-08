/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import scala.collection.mutable

import parsley.Parsley
import parsley.Parsley.{attempt, empty, fresh}
import parsley.debugger.frontend.DebugFrontend
import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.Named
import parsley.internal.deepembedding.frontend.debugger.helper.{DebugInjectingVisitor, ParserTracker}

/** This object contains the two main debug combinators, `attachDebugger` and `attachWithFrontend`. */
object combinator {
    /** Attaches a debugger to a parser, returning a reference to the debug tree produced by
      * the parser's parse tree formed as it runs.
      *
      * Ideally, this is called on the highest level parser that you want to debug, as this combinator
      * will traverse downwards in the parser tree and attach debuggers to all the child parsers. As a
      * consequence of this, it is not really recommended to include debugged parsers as lower-level
      * parsers within your main parser, and rather to isolate their testing.
      *
      * Before running the parser, it may be advised to see if the current platform's implementation
      * of the debugger supports [[parsley.debugger.util.Collector]], which allows you to input an
      * object or class instance containing parsers, in order to analyse it via reflection to assign
      * the names given to the parser fields / methods in that object to disambiguate the parser names
      * in the tree in a much easier manner.
      *
      * Without that, many parsers will appear as `packageanon` or similar, which can make the
      * debugging process much harder.
      *
      * After instrumentation, you would run the instrumented parser first, then after the parser
      * finishes running on ONE input ideally, you would run the debug tree generator function in
      * order to acquire the debug tree.
      *
      * This tree can then be introspected manually in a (runtime) debugger, or passed over to one
      * of the frontends in [[parsley.debugger.frontend]] in one of the extra companion libraries
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
      * See [[attachWithFrontend]] to automate the tree processing after parsing.
      *
      * @note Do not run a parser through this combinator multiple times.
      * @param parser The parser to debug.
      * @tparam A Output type of original parser.
      * @return A pair of the finalised tree, and the instrumented parser.
      */
    def attachDebugger[A](parser: Parsley[A]): (() => DebugTree, Parsley[A]) = {
        val seen: ParserTracker = new ParserTracker(new mutable.LinkedHashMap())
        val context: DebugContext = new DebugContext()
        val visitor: DebugInjectingVisitor = new DebugInjectingVisitor(context)

        val attached: LazyParsley[A] = parser.internal.visit[ParserTracker, LazyParsley](visitor, seen)
        (() => context.getFinalBuilder.reconstruct, fresh(context.reset()) *> new Parsley(attached))
    }

    // $COVERAGE-OFF$
    /** Create a closure that freshly attaches a debugger to a parser every time it is called.
      * This is used for creating debugged parsers that can be used as children to multiple parent
      * parsers, as using the same debugged parser as a child to multiple parsers is unsafe.
      *
      * See [[attachDebugger]] for more information, or [[attachWithFrontend]] for a version that
      * also accepts a [[parsley.debugger.frontend.DebugFrontend]].
      *
      * @note Do not run a parser through this combinator multiple times.
      * @return Generator closure for debugged versions of the input parser.
      */
    def attachReusable[A](parser: Parsley[A]): () => (() => DebugTree, Parsley[A]) =
        () => attachDebugger(parser)

    /** Attach a debugger and an explicitly-available frontend in which the debug tree should be
      * proessed with.
      *
      * You would normally obtain a [[parsley.debugger.frontend.DebugFrontend]] frontend from its
      * respective package as either a static object or an instance object depending on whether the
      * renderer stores state. In the latter case, it is better to regenerate the frontend with
      * every new debugged parser.
      *
      * The instrumented parser will automatically call the frontend to render the debug tree, so it
      * may be recommended that you only use this with smaller parsers as large parsers may cause
      * large amounts of memory to be used for processing the tree.
      *
      * See [[attachDebugger]] for more information on how attachment works and things you may want
      * to do before using this debug combinator, as well as some warnings on what not to do when
      * using this debugger (such as regarding re-use of debugged parsers).
      *
      * @note Do not run a parser through this combinator multiple times.
      * @param parser The parser to debug.
      * @param frontend The frontend instance to render with.
      * @tparam A Output type of parser.
      * @return A modified parser which will ask the frontend to process the produced debug tree after
      *         a call to [[Parsley.parse]] is made.
      */
    def attachWithFrontend[A](parser: Parsley[A], frontend: DebugFrontend): Parsley[A] = {
        val (tree, attached) = attachDebugger(parser)

        // Ideally, this should run 'attached', and render the tree regardless of the parser's success.
        val renderer = fresh {
            val frozen = tree()
            val input = frozen.fullInput

            frontend.process(input, frozen)
        }

        attempt(attached <* renderer) <|> (renderer *> empty)
    }

    /** Create a closure that freshly attaches a debugger and a tree-processing frontend to a parser every
      * time it is called.
      *
      * See [[attachReusable]] for more information.
      *
      * @note Do not run a parser through this combinator multiple times.
      *
      * @return Generator closure for frontend-debugged versions of the input parser.
      */
    def attachReusableWithFrontend[A](parser: Parsley[A], frontend: () => DebugFrontend): () => Parsley[A] =
        () => attachWithFrontend(parser, frontend())

    /** Attach a debugger and an implicitly-available frontend in which the debug tree should be
      * processed with.
      *
      * See [[attachWithFrontend]] for more information.
      *
      * @note Do not run a parser through this combinator multiple times.
      */
    def attachWithImplicitFrontend[A](parser: Parsley[A])(implicit frontend: DebugFrontend): Parsley[A]
    = attachWithFrontend(parser, frontend)

    /** Attach a name to a parser, for display within the debugger output.
      * This name has a higher precedence than names collected with [[parsley.debugger.util.Collector]].
      */
    def named[A](parser: Parsley[A], name: String): Parsley[A] =
        parser.internal match {
            case Named(i, _) => new Parsley(Named(i.asInstanceOf[LazyParsley[A]], name))
            case _           => new Parsley(Named(parser.internal, name))
        }

    /** Dot accessor versions of the combinators, in case that is your preference. */
    implicit class DebuggerOps[A](par: Parsley[A]) {
        /** Dot accessor version of [[combinator.attachDebugger]]. */
        def attachDebugger: (() => DebugTree, Parsley[A]) =
            combinator.attachDebugger(par)

        /** Dot accessor version of [[combinator.attachReusable]]. */
        def attachReusable: () => (() => DebugTree, Parsley[A]) =
            combinator.attachReusable(par)

        /** Dot accessor version of [[combinator.attachWithFrontend]]. */
        def attachWithFrontend(frontend: DebugFrontend): Parsley[A] =
            combinator.attachWithFrontend(par, frontend)

        /** Dot accessor version of [[combinator.attachReusableWithFrontend]]. */
        def attachReusableWithFrontend(frontend: () => DebugFrontend): () => Parsley[A] =
            combinator.attachReusableWithFrontend(par, frontend)

        /** Dot accessor version of [[combinator.attachWithImplicitFrontend]]. */
        def attachWithImplicitFrontend(implicit frontend: DebugFrontend): Parsley[A] =
            combinator.attachWithImplicitFrontend(par)

        /** Dot accessor version of [[combinator.named]]. */
        def named(name: String): Parsley[A] =
            combinator.named(par, name)
    }
    // $COVERAGE-ON$
}