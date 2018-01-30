import parsley.Parsley
import parsley.Instruction
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.mutable.Buffer

package object parsley
{
    type ProgramCounter = Int
    type InstructionBuffer = Array[Instruction]
    class Frame(val ret: ProgramCounter, val instrs: InstructionBuffer)
    type CallStack = Stack[Frame]
    type Depth = Int
    class Handler(val depth: Int, val pc: ProgramCounter, val stacksz: Int)
    type HandlerStack = Stack[Handler]
    type Input = List[Char]
    class State(val sz: Int, val input: Input)
    type StateStack = Stack[State]
    
    sealed trait Status
    case object Good extends Status
    case object Recover extends Status
    case object Failed extends Status

    final class Context(final var instrs: InstructionBuffer,
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
                |  inputs=${input.mkString(", ")}
                |  status=$status
                |  pc=$pc
                |  depth=$depth
                |  rets=${calls.map(_.ret).mkString(", ")}
                |  handlers=$handlers
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

    def runParser[A](p: Parsley[A], input: String): Result[A] = runParser[A](p, input.toList, input.size)
    def runParser[A](p: Parsley[A], input: List[Char], sz: Int): Result[A] = runParser_[A](new Context(p.instrs_, input, sz, p.subs_))
    def runParser[A](instrs: InstructionBuffer, subs: Map[String, InstructionBuffer], input: List[Char], sz: Int) = runParser_[A](new Context(instrs, input, sz, subs))
    
    sealed trait Result[A]
    case class Success[A](x: A) extends Result[A]
    case class Failure[A](msg: String) extends Result[A]

    @tailrec
    def runParser_[A](ctx: Context): Result[A] =
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
    
    @inline implicit def stringLift(str: String): Parsley[String] = parsley.Parsley.string(str)
    @inline implicit def charLift(c: Char): Parsley[Char] = parsley.Parsley.char(c)
    
    sealed abstract class Stack[+A]
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
    object Empty extends Stack[Nothing]
    {
        override lazy val head = ???
        override lazy val tail = ???
        override lazy val size = 0
        override val isEmpty = true
        override def mkString(sep: String) = ""
    }
    final class Elem[A](override val head: A, override val tail: Stack[A]) extends Stack[A]
    {
        override lazy val size = tail.size + 1
        override val isEmpty = false
        override def mkString(sep: String) = head.toString + sep + tail.mkString(sep)
    }

    /*sealed abstract class IntStack
    {
        val head: Int
        val tail: IntStack
        val isEmpty: Boolean
        val size: Int
        @tailrec final def drop(n: Int): IntStack = if (n > 0 && !isEmpty) tail.drop(n-1) else this
        def mkString(sep: String): String
        final def ::(x: Int): IntStack = new IntElem(x, this)
    }
    object IntEmpty extends IntStack
    {
        override lazy val head = ???
        override lazy val tail = ???
        override lazy val size = 0
        override val isEmpty = true
        override def mkString(sep: String) = ""
    }
    final class IntElem(override val head: Int, override val tail: IntStack) extends IntStack
    {
        override lazy val size = tail.size + 1
        override val isEmpty = false
        override def mkString(sep: String) = head.toString + sep + tail.mkString(sep)
    }*/
}
