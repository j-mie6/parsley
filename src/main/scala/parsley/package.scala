import parsley.internal.instructions.Context
import parsley.unsafe.FastRun

import scala.annotation.implicitAmbiguous
import scala.language.implicitConversions

package object parsley
{
    // $COVERAGE-OFF$
    // Public API
    /** This method is responsible for actually executing parsers. Given a `Parsley[A]` and an input
      * string, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param p The parser to run
      * @param input The input to run against
      * @tparam A The type of the result of parsing
      * @return Either a success with a value of type `A` or a failure with error message
      */
    @deprecated("Use p.runParser instead", "parsley-1.5.1")
    def runParser[A](p: Parsley[A], input: String): Result[A] = p.runParser(input.toCharArray)
    /** This method is responsible for actually executing parsers. Given a `Parsley[A]` and an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param p The parser to run
      * @param input The input to run against
      * @tparam A The type of the result of parsing
      * @return Either a success with a value of type `A` or a failure with error message
      */
    @deprecated("Use p.runParser instead", "parsley-1.5.1")
    def runParser[A](p: Parsley[A], input: Array[Char]): Result[A] = p.runParser(input)
    /** This method is responsible for actually executing parsers. Given a `Parsley[A]` and an input
      * string, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param p The parser to run
      * @param input The input to run against
      * @param ctx The provided context from the user
      * @tparam A The type of the result of parsing
      * @return Either a success with a value of type `A` or a failure with error message
      */
    @deprecated("The ability to provide a context to a threadsafe parser is pointless", "parsley-1.5.1")
    def runParser[A](p: Parsley[A], input: String, ctx: Context): Result[A] = runParser(p, input.toCharArray, ctx)
    /** This method is responsible for actually executing parsers. Given a `Parsley[A]` and an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param p The parser to run
      * @param input The input to run against
      * @param ctx The provided context from the user
      * @tparam A The type of the result of parsing
      * @return Either a success with a value of type `A` or a failure with error message
      */
      @deprecated("The ability to provide a context to a threadsafe parser is pointless", "parsley-1.5.1")
    def runParser[A](p: Parsley[A], input: Array[Char], ctx: Context): Result[A] = ctx(p.internal.threadSafeInstrs, input).runParser()

    // Public API - With context reuse
    /** This method allows you to run a parser with a cached context, which improves performance.
     *  If no implicit context can be found, the parsley default context is used. This will
     *  cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
     *  each thread should request its own context with `parsley.giveContext`. This value may be
     *  implicit for convenience.*/
    @deprecated("parsley.unsafe.FastRun should be imported instead", "parsley-1.5.1")
    def runParserFastUnsafe[A](p: Parsley[A], input: String)(implicit ctx: Context = unsafe.internalCtx): Result[A] = {
        p.runParserFastUnsafe(input.toCharArray)
    }

    /**
      * This method allows you to run parsers in parallel in a thread-safe fashion. This is safer
      * than runParser in the case where the parser maintains internal states, but is otherwise
      * likely slower for serial executions of the same parser.
      */
    @deprecated("This is the default behaviour of runParser", "parsley-1.5.1")
    def runParserThreadSafe[A](p: Parsley[A], input: String, ctx: Context = unsafe.giveContext): Result[A] = runParser(p, input.toCharArray, ctx)

    /**
      * This function returns a fresh Context. Contexts are used by the parsers to store their state.
      * You should only need to use this if you are using `runParserFastUnsafe` and you need separate
      * execution contexts due to multi-threaded parsing.
      * @return A fresh execution context for parsers
      */
    @deprecated("parsley.unsafe.giveContext should be used instead", "parsley-1.5.1")
    def giveContext: Context = unsafe.giveContext
    // $COVERAGE-ON$
}
