import parsley.Parsley
import parsley.Instruction

package object parsley
{
    type Stack = List[Any]
    type InstructionBuffer = List[Vector[Instruction]]
    type ProgramCounter = Int
    type Context = (Stack, InstructionBuffer, ProgramCounter)

    def runParser[A, S, X](p: Parsley[A], input: Stream[S, X]) =
    {
        runParser_[A](p.instrs, (Nil, List(p.instrs), 0))
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
