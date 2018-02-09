import scala.annotation.tailrec
import scala.language.implicitConversions

package object parsley
{
    // Public API
    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toList, input.size)
    def runParser[A](p: Parsley[A], input: Input, sz: Int): Result[A] = runParser_[A](new Context(p.instrs_, input, sz, p.subs_))

    // Implicit Conversions
    @inline final implicit def stringLift(str: String): Parsley[String] = parsley.Parsley.string(str)
    @inline final implicit def charLift(c: Char): Parsley[Char] = parsley.Parsley.char(c)

    // Private internals
    private type ProgramCounter = Int
    private [parsley] type InstructionBuffer = Array[Instr]
    private type CallStack = Stack[Frame]
    private type Depth = Int
    private type HandlerStack = Stack[Handler]
    // Yeah, turns out List[Char] is much faster than String... Not sure why?
    private type Input = List[Char]
    private type StateStack = Stack[State]

    private [parsley] final class Frame(val ret: ProgramCounter, val instrs: InstructionBuffer)
    {
        override def toString(): String = s"[$instrs@$ret]"
    }
    private [parsley] final class Handler(val depth: Int, val pc: ProgramCounter, val stacksz: Int)
    {
        override def toString(): String = s"Handler@$depth:$pc(-$stacksz)"
    }
    private [parsley] final class State(val sz: Int, val input: Input)
    {
        override def toString(): String = input.mkString
    }

    private [parsley] sealed trait Status
    private [parsley] case object Good extends Status
    private [parsley] case object Recover extends Status
    private [parsley] case object Failed extends Status

    private [parsley] final class Context(final var instrs: InstructionBuffer,
                                          final var input: Input,
                                          final var inputsz: Int,
                                          final val subs: Map[String, InstructionBuffer])
    {
        final var stack: Stack[Any] = Empty
        final var calls: CallStack = Empty
        final var states: StateStack = Empty
        final var stacksz: Int = 0
        final var checkStack: Stack[Int] = Empty
        final var status: Status = Good
        final var handlers: HandlerStack = Empty
        final var depth: Int = 0
        final var pc: ProgramCounter = 0

        final override def toString(): String =
        {
            s"""|[
                |  stack=[${stack.mkString(", ")}]
                |  instrs=${instrs.mkString("; ")}
                |  input=${input.mkString(", ")}
                |  status=$status
                |  pc=$pc
                |  depth=$depth
                |  rets=${calls.map(_.ret).mkString(", ")}
                |  handlers=$handlers
                |  recstates=$states
                |]""".stripMargin
        }

        final def fail()
        {
            if (handlers.isEmpty) status = Failed
            else
            {
                status = Recover
                val handler = handlers.head
                val diffdepth = depth - handler.depth - 1
                if (diffdepth >= 0)
                {
                    val calls_ = if (diffdepth != 0) calls.drop(diffdepth) else calls
                    instrs = calls_.head.instrs
                    calls = calls_.tail
                }
                pc = handler.pc
                handlers = handlers.tail
                val diffstack = stacksz - handler.stacksz
                if (diffstack > 0) stack = stack.drop(diffstack)
                stacksz = handler.stacksz
                depth = handler.depth
            }
        }
    }

    private [parsley] abstract class Instr
    {
        def apply(ctx: Context)
    }

    // It's 2018 and Labels are making a come-back, along with 2 pass assembly
    private [parsley] final case class Label(i: Int) extends Instr
    {
        def apply(ctx: Context) { ??? }
    }
    
    sealed trait Result[A]
    case class Success[A](x: A) extends Result[A]
    case class Failure[A](msg: String) extends Result[A]

    @tailrec
    private [this] def runParser_[A](ctx: Context): Result[A] =
    {
        //println(ctx)
        if (ctx.status == Failed) return Failure("Unknown error")
        val pc = ctx.pc
        val instrs = ctx.instrs
        if (pc < instrs.length)
        {
            instrs(pc)(ctx)
            runParser_[A](ctx)
        }
        else if (ctx.calls.isEmpty) Success(ctx.stack.head.asInstanceOf[A])
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
    
    private [parsley] sealed abstract class Stack[+A]
    {
        val head: A
        val tail: Stack[A]
        val isEmpty: Boolean
        val size: Int
        @tailrec final def drop(n: Int): Stack[A] = if (n > 0 && !isEmpty) tail.drop(n-1) else this
        final def map[B](f: A => B): Stack[B] = if (!isEmpty) f(head)::tail.map(f) else Empty
        def mkString(sep: String): String
        final def ::[A_ >: A](x: A_): Stack[A_] = new Elem(x, this)
    }
    private [parsley] object Empty extends Stack[Nothing]
    {
        final override lazy val head = ???
        final override lazy val tail = ???
        final override lazy val size = 0
        final override val isEmpty = true
        final override def mkString(sep: String) = ""
        final override def toString(): String = "[]"
    }
    private [parsley] final class Elem[A](override val head: A, override val tail: Stack[A]) extends Stack[A]
    {
        final override lazy val size = tail.size + 1
        final override val isEmpty = false
        final override def mkString(sep: String) = head.toString + sep + tail.mkString(sep)
        final override def toString(): String = s"$head::$tail"
    }
}
