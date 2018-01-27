import parsley.Parsley
import parsley.Instruction
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.mutable.Buffer

package object parsley
{
    type Stack = List[Any]
    type InstructionStack = List[Buffer[Instruction]]
    type ProgramCounters = Int
    type Depth = Int
    type HandlerStack = List[(Depth, Int)]
    type InputStack = List[List[Char]]
    
    sealed trait Status
    case object Good extends Status
    case object Recover extends Status
    case object Failed extends Status

    case class Context(stack: Stack,
                       instrss: InstructionStack,
                       inputs: InputStack,
                       checkStack: InputStack,
                       subs: Map[String, Buffer[Instruction]],
                       status: Status,
                       handlers: HandlerStack,
                       pc: ProgramCounters)

    def runParser[A](p: Parsley[A], input: String) =
    {
        runParser_[A](Context(Nil, List(p.instrs), List(input.toList), Nil, p.subs, Good, Nil, 0))
    }
    
    def runParser[A](p: Parsley[A], input: List[Char]) =
    {
        runParser_[A](Context(Nil, List(p.instrs), List(input), Nil, p.subs, Good, Nil, 0))
    }
    
    sealed trait Result[A]
    case class Success[A](x: A) extends Result[A]
    case class Failure[A](msg: String) extends Result[A]

    @tailrec
    def runParser_[A](ctx: Context): Result[A] =
    {
        if (ctx.status == Failed) return Failure("Unknown error")
        val pc = ctx.pc
        val instrs = ctx.instrss.head
        if (pc < instrs.size) runParser_[A](instrs(pc)(ctx))
        else Success(ctx.stack.head.asInstanceOf[A])
    }
    
    @inline implicit def stringLift(str: String): Parsley[String] = parsley.Parsley.string(str)
    @inline implicit def charLift(c: Char): Parsley[Char] = parsley.Parsley.char(c)
}
