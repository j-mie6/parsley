import parsley.instructions.Context
import scala.annotation.tailrec

package object parsley
{
    // Public API
    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toCharArray)
    def runParser[A](p: Parsley[A], input: Array[Char]): Result[A] = new Context(p.instrs, input).runParser()
    
    // Public API - With context reuse
    /** This method allows you to run a parser with a cached context, which improves performance. 
     *  If no implicit context can be found, the parsley default context is used. This will
     *  cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
     *  each thread should request its own context with `parsley.giveContext`. This value may be
     *  implicit for convenience.*/
    def runParserFastUnsafe[A](p: Parsley[A], input: String)(implicit ctx: Context = internalCtx): Result[A] = runParser[A](p, input.toCharArray, ctx)
    def runParser[A](p: Parsley[A], input: Array[Char], ctx: Context): Result[A] = ctx(p.instrs, input).runParser()
    def giveContext: Context = new Context(null, Array.emptyCharArray)

    // Internals
    private [parsley] val internalCtx = giveContext
    private [parsley] type UnsafeOption[A] = A

    sealed abstract class Result[A]
    case class Success[A](x: A) extends Result[A]
    case class Failure[A](msg: String) extends Result[A]

    // Trampoline for CPS
    sealed abstract class Bounce[A]
    {
        @tailrec final def run: A = this match
        {
            case thunk: Thunk[A] => thunk.cont().run
            case chunk: Chunk[A] => chunk.x
        }
    }
    final class Chunk[A](val x: A) extends Bounce[A]
    final class Thunk[A](val cont: () => Bounce[A]) extends Bounce[A]

    sealed abstract class Continuation
    {
        //noinspection TypeCheckCanBeMatch
        @tailrec final def run(): Unit = if (this.isInstanceOf[Suspended]) this.asInstanceOf[Suspended]().run()
    }
    final object Terminate extends Continuation
    final class Suspended(cont: =>Continuation) extends Continuation { def apply(): Continuation = cont }
    
    // This is designed to be a lighter weight wrapper around Array to make it resizeable
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
}
