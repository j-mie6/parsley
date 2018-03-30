import scala.annotation.tailrec

package object parsley
{
    import parsley.Stack._
    // Public API
    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toCharArray)
    def runParser[A](p: Parsley[A], input: Array[Char]): Result[A] = runParser_[A](new Context(p.instrs, input))
    
    // Public API - With context reuse
    /** This method allows you to run a parser with a cached context, which improves performance. 
     *  If no implicit context can be found, the parsley default context is used. This will
     *  cause issues with multi-threaded execution of parsers. In order to mitigate these issues,
     *  each thread should request its own context with `parsley.giveContext`. This value may be
     *  implicit for convenience.*/
    def runParserFastUnsafe[A](p: Parsley[A], input: String)(implicit ctx: Context = internalCtx): Result[A] = runParser[A](p, input.toCharArray, ctx)
    def runParser[A](p: Parsley[A], input: Array[Char], ctx: Context): Result[A] = runParser_[A](ctx(p.instrs, input))
    def giveContext: Context = new Context(null, Array.emptyCharArray)

    // Internals
    private [parsley] val internalCtx = giveContext
    private [parsley] type UnsafeOption[A] = A
    private [parsley] sealed abstract class Status
    private [parsley] case object Good extends Status
    private [parsley] case object Recover extends Status
    private [parsley] case object Failed extends Status

    private [parsley] abstract class Instr
    {
        def apply(ctx: Context): Unit
        // Instructions should override this if they have mutable state inside!
        def copy: Instr = this
    }
    
    private [parsley] abstract class JumpInstr extends Instr
    {
        var label: Int
    }

    // It's 2018 and Labels are making a come-back, along with 2 pass assembly
    private [parsley] final class Label(val i: Int) extends Instr
    {
        def apply(ctx: Context): Unit = throw new Exception("Cannot execute label")
    }

    sealed abstract class Result[A]
    case class Success[A](x: A) extends Result[A]
    case class Failure[A](msg: String) extends Result[A]

    @tailrec @inline private [this] def runParser_[A](ctx: Context): Result[A] =
    {
        //println(ctx)
        if (ctx.status eq Failed) return Failure(ctx.errorMessage)
        val pc = ctx.pc
        val instrs = ctx.instrs
        if (pc < instrs.length)
        {
            instrs(pc)(ctx)
            runParser_[A](ctx)
        }
        else if (isEmpty(ctx.calls)) Success(ctx.stack.peek.asInstanceOf[A])
        else
        {
            val frame = ctx.calls.head
            ctx.instrs = frame.instrs
            ctx.calls = ctx.calls.tail
            ctx.pc = frame.ret
            ctx.depth -= 1
            if (ctx.depth < ctx.overrideDepth)
            {
                ctx.overrideDepth = 0
                ctx.errorOverride = null
            }
            runParser_[A](ctx)
        }
    }

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

    // This stack class is designed to be ultra-fast: no virtual function calls
    // It will crash with NullPointerException if you try and use head or tail of empty stack
    // But that is illegal anyway
    private [parsley] final class Stack[A](var head: A, val tail: Stack[A])
    private [parsley] object Stack
    {
        def empty[A]: Stack[A] = null
        @inline def isEmpty(s: Stack[_]): Boolean = s == null
        @tailrec def drop[A](s: Stack[A], n: Int): Stack[A] = if (n > 0 && !isEmpty(s)) drop(s.tail, n - 1) else s
        def map[A, B](s: Stack[A], f: A => B): Stack[B] = if (!isEmpty(s)) new Stack(f(s.head), map(s.tail, f)) else empty
        def mkString(s: Stack[_], sep: String): String = if (isEmpty(s)) "" else s.head.toString + sep + mkString(s.tail, sep)
        def push[A](s: Stack[A], x: A) = new Stack(x, s)
    }

    // Designed to replace the operational stack
    // Since elements are of type Any, this serves as a optimised implementation
    // Its success may result in the deprecation of the Stack class in favour of a generic version of this!
    private [parsley] final class ArrayStack[A](initialSize: Int = 8)
    {
        private [this] var array: Array[Any] = new Array(initialSize)
        private [this] var sp = -1

        def push(x: A): Unit =
        {
            sp += 1
            if (array.length == sp)
            {
                val newArray: Array[Any] = new Array(sp * 2)
                java.lang.System.arraycopy(array, 0, newArray, 0, sp)
                array = newArray
            }
            array(sp) = x
        }

        def exchange(x: A): Unit = array(sp) = x
        def pop_(): Unit = sp -= 1
        def upop(): Any =
        {
            val x = array(sp)
            sp -= 1
            x
        }
        def pop[B <: A](): B = upop().asInstanceOf[B]
        def upeek: Any = array(sp)
        def peek[B <: A]: B = upeek.asInstanceOf[B]

        def update(off: Int, x: A): Unit = array(sp - off) = x
        def apply(off: Int): Any = array(sp - off)

        def drop(x: Int): Unit = sp -= x

        // This is off by one, but that's fine, if everything is also off by one :P
        def usize: Int = sp
        def size: Int = usize + 1
        def isEmpty: Boolean = sp == -1
        def mkString(sep: String): String = array.take(sp+1).reverse.mkString(sep)
        def clear(): Unit = 
        {
            sp = -1
            var i = array.length-1
            while (i >= 0)
            {
                array(i) = null
                i -= 1
            }
        }
    }
    
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
