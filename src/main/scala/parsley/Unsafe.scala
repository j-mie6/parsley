package parsley

import parsley.instructions.Context

object unsafe {
    /**
      * This function returns a fresh Context. Contexts are used by the parsers to store their state.
      * You should only need to use this if you are using `runParserFastUnsafe` and you need separate
      * execution contexts due to multi-threaded parsing.
      * @return A fresh execution context for parsers
      */
    def giveContext: Context = new Context(null, Array.emptyCharArray)

    implicit class FastRun[A](val p: Parsley[A])(implicit ctx: Context = internalCtx) {
        /** This method allows you to run a parser with a cached context, which improves performance.
        *  If no implicit context can be found, the parsley default context is used. This will
        *  cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
        *  each thread should request its own context with `parsley.giveContext`. This value may be
        *  implicit for convenience.*/
        def runParserFastUnsafe(input: String): Result[A] = runParserFastUnsafe(input.toCharArray)
        /** This method allows you to run a parser with a cached context, which improves performance.
        *  If no implicit context can be found, the parsley default context is used. This will
        *  cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
        *  each thread should request its own context with `parsley.giveContext`. This value may be
        *  implicit for convenience.*/
        def runParserFastUnsafe(input: Array[Char]): Result[A] = ctx(p.internal.instrs, input).runParser()
    }

    // Internals
    private [parsley] val internalCtx = giveContext
}