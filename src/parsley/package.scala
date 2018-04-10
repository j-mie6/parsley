import parsley.instructions.Context

import scala.annotation.tailrec

package object parsley
{
    // Public API
    /** This method is responsible for actually executing parsers. Given a `Parsley[A]` and an input
      * string, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param p The parser to run
      * @param input The input to run against
      * @tparam A The type of the result of parsing
      * @return Either a success with a value of type `A` or a failure with error message
      */
    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toCharArray)
    /** This method is responsible for actually executing parsers. Given a `Parsley[A]` and an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param p The parser to run
      * @param input The input to run against
      * @tparam A The type of the result of parsing
      * @return Either a success with a value of type `A` or a failure with error message
      */
    def runParser[A](p: Parsley[A], input: Array[Char]): Result[A] = new Context(p.instrs, input).runParser()
    
    // Public API - With context reuse
    /** This method allows you to run a parser with a cached context, which improves performance. 
     *  If no implicit context can be found, the parsley default context is used. This will
     *  cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
     *  each thread should request its own context with `parsley.giveContext`. This value may be
     *  implicit for convenience.*/
    def runParserFastUnsafe[A](p: Parsley[A], input: String)(implicit ctx: Context = internalCtx): Result[A] = runParser[A](p, input.toCharArray, ctx)
    private [parsley] def runParser[A](p: Parsley[A], input: Array[Char], ctx: Context): Result[A] = ctx(p.instrs, input).runParser()

    /**
      * This function returns a fresh Context. Contexts are used by the parsers to store their state.
      * You should only need to use this if you are using `runParserFastUnsafe` and you need separate
      * execution contexts due to multi-threaded parsing.
      * @return A fresh execution context for parsers
      */
    def giveContext: Context = new Context(null, Array.emptyCharArray)

    // Internals
    private [parsley] val internalCtx = giveContext
    private [parsley] type UnsafeOption[A >: Null] = A

    /**
      * This class is used to index registers within the mutable state.
      * Currently, there are only 4 available registers, so use them wisely!
      * @param v The index of the register to interact with
      */
    case class Var(v: Int) extends AnyVal

    /**
      * Result of a parser. Either a `Success[A]` or a `Failure`
      * @tparam A The type of expected success result
      */
    sealed abstract class Result[+A]

    /**
      * Returned when a parser succeeded.
      * @param x The result value of the successful parse
      * @tparam A The type of expected success result
      */
    case class Success[A](x: A) extends Result[A]

    /**
      * Returned on parsing failure
      * @param msg The error message reported by the parser
      */
    case class Failure(msg: String) extends Result[Nothing]

    // Trampoline for CPS
    private [parsley] sealed abstract class Bounce[A]
    {
        @tailrec final def run: A = this match
        {
            case thunk: Thunk[A] => thunk.cont().run
            case chunk: Chunk[A] => chunk.x
        }
    }
    private [parsley] final class Chunk[A](val x: A) extends Bounce[A]
    private [parsley] final class Thunk[A](val cont: () => Bounce[A]) extends Bounce[A]

    private [parsley] sealed abstract class Continuation
    {
        //noinspection TypeCheckCanBeMatch
        @tailrec final def run(): Unit = if (this.isInstanceOf[Suspended]) this.asInstanceOf[Suspended]().run()
    }
    private [parsley] final object Terminate extends Continuation
    private [parsley] final class Suspended(cont: =>Continuation) extends Continuation { def apply(): Continuation = cont }
    
    // This is designed to be a lighter-weight wrapper around Array to make it resizeable
    import scala.reflect.ClassTag
    private [parsley] final class ResizableArray[A: ClassTag](initialSize: Int = 16)
    {
        private [this] var array: Array[A] = new Array(initialSize)
        private [this] var size = 0
        
        def +=(x: A): Unit =
        {
            val arrayLength: Long = array.length
            if (arrayLength == size)
            {
                val newSize: Long = Math.min(arrayLength * 2, Int.MaxValue)
                val newArray: Array[A] = new Array(newSize.toInt)
                java.lang.System.arraycopy(array, 0, newArray, 0, size)
                array = newArray
            }
            array(size) = x
            size += 1
        }
        def length: Int = size
        def toArray: Array[A] = array
    }

    // This is designed to be a very optimised and light-weight implementation of a BitSet for characters
    private [parsley] final class BitSet(gen: Either[Set[Char], Char => Boolean]) extends Function[Char, Boolean]
    {
        val (max, arr) = gen match
        {
            case Left(set) => setup(set)
            case Right(f) => setup(f)
        }

        def setup(set: Set[Char]): (Int, Array[Int]) =
        {
            val max = if (set.isEmpty) -1 else set.max
            val arr = new Array[Int]((max >> 5) + 1)

            for (c <- set)
            {
                // c / 32 finds the index int, c % 32 finds the index bit
                val index = c >> 5
                arr(index) = arr(index) ^ (1 << (c & 31))
            }
            (max, arr)
        }
        def setup(f: Char => Boolean): (Int, Array[Int]) =
        {
            var i: Int = 0
            var max = 0
            val bigarr = new Array[Int](2048)
            while (i < 65535)
            {
                if (f(i.toChar))
                {
                    max = i
                    val index = i >> 5
                    bigarr(index) = bigarr(index) ^ (1 << (i & 31))
                }
                i += 1
            }
            val arr = new Array[Int]((max >> 5) + 1)
            java.lang.System.arraycopy(bigarr, 0, arr, 0, (max >> 5) + 1)
            (max, arr)
        }

        def contains(c: Char): Boolean = c <= max && ((arr(c >> 5) >> (c & 31)) & 1) == 1
        def apply(c: Char): Boolean = c <= max && ((arr(c >> 5) >> (c & 31)) & 1) == 1
    }
}
