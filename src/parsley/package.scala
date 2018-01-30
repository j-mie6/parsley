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
    type CallStack = List[Frame]
    type Depth = Int
    class Handler(val depth: Int, val pc: ProgramCounter, val stacksz: Int)
    type HandlerStack = List[Handler]
    type Input = List[Char]
    class State(val sz: Int, val input: Input)
    type StateStack = List[State]
    
    sealed trait Status
    case object Good extends Status
    case object Recover extends Status
    case object Failed extends Status

    class Context(var instrs: InstructionBuffer,
                  var input: Input,
                  var inputsz: Int,
                  val subs: Map[String, InstructionBuffer])
    {
        var stack: Stack[Any] = Empty
        var calls: CallStack = Nil
        var states: StateStack = Nil
        var stacksz: Int = 0
        var checkStack: Stack[Int] = IntEmpty
        var status: Status = Good
        var handlers: HandlerStack = Nil
        var depth: Int = 0
        var pc: ProgramCounter = 0
        
        override def toString(): String = 
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

        def fail() =
        {
            if (handlers.isEmpty) { status = Failed; this }
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
    
    trait Stack[@specialized(Int) +T]
    {
        val head: T
        val tail: Stack[T]
        val isEmpty: Boolean
        def size: Int = 0
        @tailrec
        final def drop(n: Int): Stack[T] = if (n > 0 && !isEmpty) tail.drop(n-1) else this
        def mkString(sep: String): String
    }
    object Empty extends Stack[Null]
    {
        override val head = null
        override val tail = null
        override val size = 0
        override val isEmpty = true
        override def mkString(sep: String) = ""
    }
    object IntEmpty extends Stack[Int]
    {
        override val head = 0
        override val tail = null
        override val size = 0
        override val isEmpty = true
        override def mkString(sep: String) = ""
    }
    class Elem[@specialized(Int) T](override val head: T, override val tail: Stack[T]) extends Stack[T]
    {
        override def size = tail.size + 1
        override val isEmpty = false
        override def mkString(sep: String) = head.toString + sep + tail.mkString(sep)
    }
}
