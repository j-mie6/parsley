package parsley

import parsley.internal.machine
import parsley.internal.deepembedding

/** This module contains various things that shouldn't be used without care and caution
  * @since 1.6.0
  */
object unsafe {
    // UNSAFE EXECUTION
    // This is hard to test, because it's not thread-safe!
    // $COVERAGE-OFF$
    /**
      * This function returns a fresh Context. Contexts are used by the parsers to store their state.
      * You should only need to use this if you are using `runParserFastUnsafe` and you need separate
      * execution contexts due to multi-threaded parsing.
      * @return A fresh execution context for parsers
      * @since 1.6.0
      */
    def giveContext: Context = new Context(machine.Context.empty)

    /** This class enables a bunch of unsafe running functionality on parsers, which makes them run faster
      * at the cost of thread-safety. Use at your own risk.
      * @since 1.6.0
      */
    implicit class FastRun[A](private val p: Parsley[A])(implicit ctx: Context = internalCtx) {
        /** This method allows you to run a parser with a cached context, which improves performance.
          * If no implicit context can be found, the parsley default context is used. This will
          * cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
          * each thread should request its own context with `parsley.giveContext`. This value may be
          * implicit for convenience.
          * @since 3.0.0
          */
        def parseFastUnsafe(input: String): Result[A] = ctx.internal(p.internal.instrs, input).runParser()
    }

    final class Context private [parsley] (private [parsley] val internal: machine.Context)

    // Internals
    private [parsley] val internalCtx = giveContext
    // $COVERAGE-ON$

    // UNSAFE ERRORS
    /** This class enables faster, but potentially misleading error behaviour
      *  @since 2.6.0
      */
    implicit class ErrorLabel[P, A](p: =>P)(implicit con: P => Parsley[A]) {
        /** Sets the expected message for a parser. If the parser fails then `expected msg` will added to the error.
          * This will supercede '''all''' labels that that are present in the parser `p`. Whilst this does improve
          * the speed of the parser, it may render your error messages useless if not used carefully. This method
          * should ''only'' be used for '''non-terminals''' in the grammar
          * @since 2.6.0
          */
        def unsafeLabel(msg: String): Parsley[A] = new Parsley(new deepembedding.UnsafeErrorRelabel(p.internal, msg))
    }
}
