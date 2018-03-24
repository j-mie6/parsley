import scala.annotation.tailrec
import scala.language.implicitConversions

package object parsley
{
    import parsley.Stack._
    // Public API
    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toCharArray)
    def runParser[A](p: Parsley[A], input: Array[Char]): Result[A] = runParser_[A](new Context(p.instrs, input))

    // Implicit Conversions
    @inline final implicit def stringLift(str: String): Parsley[String] = parsley.Parsley.string(str)
    @inline final implicit def charLift(c: Char): Parsley[Char] = parsley.Parsley.char(c)

    // Internals
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
        else if (isEmpty(ctx.calls)) Success(ctx.stack.head.asInstanceOf[A])
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
        @tailrec final def run(): Unit = if (this.isInstanceOf[Suspended]) this.asInstanceOf[Suspended]().run()
    }
    final object Terminate extends Continuation
    final class Suspended(cont: =>Continuation) extends Continuation { def apply() = cont }

    // This stack class is designed to be ultra-fast: no virtual function calls
    // It will crash with NullPointerException if you try and use head or tail of empty stack
    // But that is illegal anyway
    private [parsley] final class Stack[A](var head: A, val tail: Stack[A])
    /*{
        import Stack._
        lazy val size_ : Int = size(tail) + 1
    }*/
    private [parsley] object Stack
    {
        def empty[A]: Stack[A] = null
        @inline def isEmpty(s: Stack[_]): Boolean = s == null
        //def size(s: Stack[_]): Int = if (isEmpty(s)) 0 else s.size_
        @tailrec def drop[A](s: Stack[A], n: Int): Stack[A] = if (n > 0 && !isEmpty(s)) drop(s.tail, n - 1) else s
        def map[A, B](s: Stack[A], f: A => B): Stack[B] = if (!isEmpty(s)) new Stack(f(s.head), map(s.tail, f)) else empty
        def mkString(s: Stack[_], sep: String): String = if (isEmpty(s)) "" else s.head.toString + sep + mkString(s.tail, sep)
    }
    // This class is left in for niceness sake :)
    private [parsley] final implicit class StackCons[A](s: Stack[A])
    {
        def ::(x: A): Stack[A] = new Stack(x, s)
    }

    private [parsley] final class ArrayStack(initialSize: Int = 16)
    {
        private [this] var array: Array[Any] = new Array(initialSize)
        private [this] var sp = 0

        def ::=(x: Any): Unit =
        {
            insert(x, sp)
            sp += 1
        }

        def exchange(x: Any): Unit = array(sp) = x
        def pop(): Any =
        {
            val x = array(sp)
            sp -= 1
            x
        }
        def peek(): Any = array(sp)

        def apply(off: Int, x: Any): Unit =
        {
            array.update(sp - off, x)
        }

        def drop(x: Int): Unit = sp -= x

        def insert(x: Any, idx: Int): Unit =
        {
            val arrayLength: Int = array.length
            if (arrayLength >= idx)
            {
                val newSize: Int = arrayLength * 2
                val newArray: Array[Any] = new Array(newSize)
                java.lang.System.arraycopy(array, 0, newArray, 0, idx)
                array = newArray
            }
            array(idx) = x
        }

        def size: Int = sp
        def mkString(sep: String): String = array.mkString(sep)
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
