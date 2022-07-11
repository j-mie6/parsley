/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.errors.ErrorBuilder

import parsley.internal.deepembedding.frontend

/** This module contains the very useful debugging combinator, as well as breakpoints.
  *
  * @groupprio comb 0
  * @groupname comb Debug Combinator Extension Methods
  * @groupdesc comb
  *     These are the debugging combinators, which are enabled by bringing these implicit classes
  *     into scope.
  *
  * @groupprio break 10
  * @groupname break Breakpoints
  * @groupdesc break
  *     These can be used to control how the `debug` combinator operates: when a breakpoint is used
  *     it can halt the execution of the combinator and print out information about the parsers state.
  *
  * @groupprio ctrl 5
  * @groupname ctrl Debug Control
  * @groupdesc ctrl
  *     These methods can control how the debug mechanism functions in a general way.
  */
// $COVERAGE-OFF$
object debug {
    /** Base trait for breakpoints.
      *
      * @group break
      */
    sealed trait Breakpoint
    /** Indicates that no breaking should occur.
      *
      * @group break
      */
    case object NoBreak extends Breakpoint
    /** Break on entry to the combinator, require user input to advance.
      *
      * @group break
      */
    case object EntryBreak extends Breakpoint
    /** Break on exit to the combinator, require user input to advance.
      *
      * @group break
      */
    case object ExitBreak extends Breakpoint
    /** Break on both entry and exit to the combinator, require user input to advance in both cases.
      *
      * @group break
      */
    case object FullBreak extends Breakpoint

    private [parsley] var renderAscii = false
    /** This method can be used to disable the coloured debug output for terminals that don't support it.
      *
      * @group ctrl
      */
    def disableColourRendering(): Unit = renderAscii = true

    /** This class enables the `debug` combinator on parsers.
      *
      * This extension class operates on values that are convertible to parsers. It enables the use of
      * the `debug` combinator, which can be used to trace the execution through a parser.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param p the value that this class is enabling methods on.
      * @param con a conversion that allows values convertible to parsers to be used.
      * @tparam P the type of base value that this class is used on (the conversion to `Parsley`) is summoned automatically.
      * @group comb
      */
    implicit class DebugCombinators[P, A](p: P)(implicit con: P => Parsley[A]) {
        /** This combinator allows this parser to be debugged by providing a trace through the execution.
          *
          * When this combinator is entered, it will print the name assigned to the parser,
          * as well as the current currenting input context for a few characters on either side.
          * This parser is then executed. If it succeeded, this combinator again reports the
          * name along with "`Good`" and the input context. If it failed, it reports the name
          * along with "`Bad`" and the input context.
          *
          * When breakpoints are used, the execution of the combinator will pause on either entry,
          * exit, or both. The parse is resumed by entering a newline on standard input. Breakpoints
          * will cause additional information about the internal state of the parser to be reported.
          *
          * @example {{{
          * scala> import parsley.debug.DebugCombinators, parsley.character.string, parsley.Parsley.attempt
          * scala> val abc = attempt(string("abc").debug("string abc")).debug("attempt")
          * scala> val abd = string("abd").debug("string abd")
          * scala> val p = (abc <|> abd).debug("or")
          * scala> p.parse("abd")
          * >or> (1, 1): abd•
          *              ^
          *   >attempt> (1, 1): abd•
          *                     ^
          *     >string abc> (1, 1): abd•
          *                          ^
          *     <string abc< (1, 3): abd• Fail
          *                            ^
          *   <attempt< (1, 1): abd• Fail
          *                     ^
          *   >string abd> (1, 1): abd•
          *                        ^
          *   <string abd< (1, 4): abd• Good
          *                           ^
          * <or< (1, 4): abd• Good
          *                 ^
          * val res0 = Success("abd")
          * }}}
          *
          * @param name The name to be assigned to this parser
          * @param break The breakpoint properties of this parser, defaults to NoBreak
          * @param coloured Whether to render with colour (default true: render colours)
          */
        def debug(name: String, break: Breakpoint = NoBreak, coloured: Boolean = true): Parsley[A] = {
            new Parsley(new frontend.Debug[A](con(p).internal, name, !coloured, break))
        }

        def debugError(name: String, coloured: Boolean = true)(implicit errBuilder: ErrorBuilder[_]): Parsley[A] = {
            new Parsley(new frontend.DebugError[A](con(p).internal, name, !coloured, errBuilder))
        }
    }
}
// $COVERAGE-ON$
