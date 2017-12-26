import parsley.Parsley
import parsley.Instruction

package object parsley
{
    type Stack = List[Any]
    type InstructionStack = List[Vector[Instruction]]
    type ProgramCounters = Int
    type Failed = Int
    type InputStack = List[String]

    case class Context(stack: Stack,
                       instrss: InstructionStack,
                       inputs: InputStack,
                       checkStack: InputStack,
                       subs: Map[String, Vector[Instruction]],
                       failed: Failed,
                       pc: ProgramCounters)

    def runParser[A](p: Parsley[A], input: String) =
    {
        runParser_[A](p.instrs, Context(Nil, List(p.instrs), List(input), Nil, p.subs, 0, 0))
    }

    def runParser_[A](instrs: Vector[Instruction], ctx: Context) =
    {
        ???
    }

    sealed trait Stream[S, X]
    {
        def uncons(): Option[(X, Stream[S, X])]
    }

    implicit class StringStream(s: String) extends Stream[String, Char]
    {
        def uncons(): Option[(Char, Stream[String, Char])] =
        {
            if (s.isEmpty()) None
            else Some(s.head, s.tail)
        }
        override def toString = s
    }
}
