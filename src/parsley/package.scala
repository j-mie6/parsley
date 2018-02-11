import scala.annotation.tailrec
import scala.language.implicitConversions

package object parsley
{
    import parsley.Stack._
    // Public API
    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toList, input.length)
    def runParser[A](p: Parsley[A], input: Input, sz: Int): Result[A] = runParser_[A](new Context(p.instrArray, input, sz, p.subsMap))

    // Implicit Conversions
    @inline final implicit def stringLift(str: String): Parsley[String] = parsley.Parsley.string(str)
    @inline final implicit def charLift(c: Char): Parsley[Char] = parsley.Parsley.char(c)

    // Private internals
    private [parsley] type ProgramCounter = Int
    private [parsley] type CallStack = Stack[Frame]
    private [parsley] type Depth = Int
    private [parsley] type HandlerStack = Stack[Handler]
    // Yeah, turns out List[Char] is much faster than String... Not sure why?
    private [parsley] type Input = List[Char]
    private [parsley] type StateStack = Stack[State]

    private [parsley] final class Frame(val ret: ProgramCounter, val instrs: Array[Instr])
    {
        override def toString: String = s"[$instrs@$ret]"
    }
    private [parsley] final class Handler(val depth: Int, val pc: ProgramCounter, val stacksz: Int)
    {
        override def toString: String = s"Handler@$depth:$pc(-$stacksz)"
    }
    private [parsley] final class State(val sz: Int, val input: Input)
    {
        override def toString: String = input.mkString
    }

    private [parsley] sealed abstract class Status
    private [parsley] case object Good extends Status
    private [parsley] case object Recover extends Status
    private [parsley] case object Failed extends Status

    private [parsley] abstract class Instr
    {
        def apply(ctx: Context)
    }

    // It's 2018 and Labels are making a come-back, along with 2 pass assembly
    private [parsley] final case class Label(i: Int) extends Instr
    {
        def apply(ctx: Context) { throw new Exception("Cannot execute label") }
    }
    
    sealed abstract class Result[A]
    case class Success[A](x: A) extends Result[A]
    case class Failure[A](msg: String) extends Result[A]

    @tailrec @inline private [this] def runParser_[A](ctx: Context): Result[A] =
    {
        //println(ctx)
        if (ctx.status eq Failed) return Failure("Unknown error")
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
            runParser_[A](ctx)
        }
    }

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
        // This class is left in for niceness sake, but we shouldn't be using it if we can avoid it!
        final implicit class Cons[A](s: Stack[A])
        {
            def ::(x: A): Stack[A] = new Stack(x, s)
        }
    }
}
