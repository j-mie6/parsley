package parsley

import parsley.internal.deepembedding.frontend

import scala.language.implicitConversions

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
    /** Base trait for breakpoints
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
    /**
      * This method can be used to disable the coloured debug output for terminals that don't support it
      *
      * @group ctrl
      */
    def disableColourRendering(): Unit = renderAscii = true

    /** This class enables the `debug` combinator on parsers
      *
      * @group comb
      */
    implicit class DebugCombinators[P, A](p: P)(implicit con: P => Parsley[A]) {
        /**
          * Using this method enables debugging functionality for this parser. When it is entered a snapshot is taken and
          * presented on exit. It will signify when a parser is entered and exited as well. Use the break parameter to halt
          * execution on either entry, exit, both or neither.
          * @param name The name to be assigned to this parser
          * @param break The breakpoint properties of this parser, defaults to NoBreak
          * @param coloured Whether to render with colour (default true: render colours)
          */
        def debug(name: String, break: Breakpoint = NoBreak, coloured: Boolean = true): Parsley[A] = {
            new Parsley(new frontend.Debug[A](con(p).internal, name, !coloured, break))
        }
    }
}
// $COVERAGE-ON$