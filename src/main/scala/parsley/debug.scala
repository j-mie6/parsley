package parsley

import parsley.internal.deepembedding

/** This module contains the very useful debugging combinator, as well as breakpoints. */
object debug {
    trait Breakpoint
    case object NoBreak extends Breakpoint
    case object EntryBreak extends Breakpoint
    case object ExitBreak extends Breakpoint
    case object FullBreak extends Breakpoint

    /** This class enables the `debug` combinator on parsers */
    implicit class DebugCombinators[P, +A](val p: P)(implicit val con: P => Parsley[A]) {
        /**
          * Using this method enables debugging functionality for this parser. When it is entered a snapshot is taken and
          * presented on exit. It will signify when a parser is entered and exited as well. Use the break parameter to halt
          * execution on either entry, exit, both or neither.
          * @param name The name to be assigned to this parser
          * @param break The breakpoint properties of this parser, defaults to NoBreak
          */
          def debug[A_ >: A](name: String, break: Breakpoint = NoBreak): Parsley[A_] = new Parsley(new deepembedding.Debug[A_](p.internal, name, break))
    }
}