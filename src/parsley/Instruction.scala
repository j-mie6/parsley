package parsley

import language.existentials
import scala.annotation.tailrec
import scala.collection.mutable.Buffer

trait Instruction
{
    def apply(ctx: Context): Context
}

case class Perform[-A, +B](f: A => B) extends Instruction
{
    val g = f.asInstanceOf[Function[Any, Any]] 
    override def apply(ctx: Context): Context =
    {
        ctx.stack = g(ctx.stack.head)::ctx.stack.tail 
        ctx.pc += 1
        ctx
    }
    override def toString: String = "Perform(f)"
}

case class Push[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        ctx.stack ::= x
        ctx.pc += 1
        ctx
    }
}

case object Pop extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        ctx.stack = ctx.stack.tail
        ctx.pc += 1
        ctx
    }
}

case class Exchange[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        ctx.stack = x::ctx.stack.tail
        ctx.pc += 1
        ctx
    }
}

case object Apply extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        val stacktail = ctx.stack.tail
        val f = stacktail.head.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]]
        ctx.stack = f(ctx.stack.head)::stacktail.tail
        ctx.pc += 1
        ctx
    }
}

case object Cons extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        val stacktail = ctx.stack.tail
        ctx.stack = (stacktail.head::ctx.stack.head.asInstanceOf[List[_]])::stacktail.tail
        ctx.pc += 1
        ctx
    }
}

case class Call(x: String) extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        ctx.instrss ::= (ctx.pc + 1, ctx.instrs)
        ctx.instrs = ctx.subs(x)
        ctx.pc = 0
        ctx
    }
}

case class DynSub[-A](f: A => Buffer[Instruction]) extends Instruction
{
    val g = f.asInstanceOf[Any => Buffer[Instruction]]
    override def apply(ctx: Context): Context = 
    {
        ctx.instrss ::= (ctx.pc + 1, ctx.instrs)
        ctx.instrs = g(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.pc = 0
        ctx
    }
}

case class FastFail[A](msggen: A=>String) extends Instruction
{
    val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Context = 
    {
        val msg = msggen_(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        new Fail(msg)(ctx)
    }
}

case class Fail(msg: String) extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        if (ctx.handlers.isEmpty) {ctx.status = Failed; ctx}
        else
        {
            val (depth, handler) = ctx.handlers.head
            val diff = ctx.depth - depth - 1
            val instrss = if (diff > 0) ctx.instrss.drop(diff) else ctx.instrss
            ctx.status = Recover
            ctx.instrs = instrss.head._2
            ctx.pc = handler
            ctx.handlers = ctx.handlers.tail
            ctx.instrss = instrss.tail
            ctx.depth = depth
            ctx
        }
    }
}

case class TryBegin(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Context = 
    {
        ctx.handlers ::= (ctx.depth, handler + ctx.pc)
        ctx.inputs ::= ctx.input
        ctx.pc += 1
        ctx
    }
}

case object TryEnd extends Instruction
{
    override def apply(ctx: Context): Context = 
    {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status == Good)
        {
            ctx.inputs = ctx.inputs.tail
            ctx.handlers = ctx.handlers.tail
            ctx.pc += 1
            ctx
        }
        // Pop input off head to recover
        else
        {
            ctx.input = ctx.inputs.head
            ctx.inputs = ctx.inputs.tail
            ctx.status = Good
            ctx.inputsz = ctx.input.size
            ctx.pc += 1
            ctx
        }
    }
}

//NOTE: If we implement this instruction in the future, we cannot use labels with it
//      for optimisation purposes all labels are forward jumps this means we can
//      resolve all label addresses in a single backwards pass during optimisation.
//      Instead, we need to ensure that we calculate the backwards offset immediately
//      at compile-time.
//case class Many(x: Int) extends Instruction

case class InputCheck(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        ctx.checkStack ::= ctx.inputsz
        ctx.handlers ::= (ctx.depth, handler + ctx.pc)
        ctx.pc += 1
        ctx
    }
}

case class JumpGood(label: Int) extends Instruction
{
    override def apply(ctx: Context): Context = 
    {
        if (ctx.status == Good)
        {
            ctx.handlers = ctx.handlers.tail
            ctx.checkStack = ctx.checkStack.tail
            ctx.pc += label
            ctx
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head)
        {
            if (ctx.handlers.isEmpty) { ctx.status = Failed; ctx }
            else
            {
                val (depth, handler) = ctx.handlers.head
                val diff = ctx.depth - depth - 1
                val instrss = if (diff > 0) ctx.instrss.drop(diff) else ctx.instrss
                ctx.status = Recover
                ctx.instrs = instrss.head._2
                ctx.pc = handler
                ctx.handlers = ctx.handlers.tail
                ctx.instrss = instrss.tail
                ctx.depth = depth
                ctx
            }
        }
        else 
        {
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.pc += 1
            ctx
        }
    }
}

case class CharTok(c: Char) extends Instruction
{
    val ac: Any = c
    override def apply(ctx: Context): Context = ctx.input match
    {
        case `c`::input =>
            ctx.stack ::= ac
            ctx.inputsz -= 1
            ctx.input = input
            ctx.pc += 1
            ctx
        case inputs =>
            if (ctx.handlers.isEmpty) { ctx.status = Failed; ctx }
            else
            {
                val (depth, handler) = ctx.handlers.head
                val diff = ctx.depth - depth - 1
                val instrss = if (diff > 0) ctx.instrss.drop(diff) else ctx.instrss
                ctx.status = Recover
                ctx.instrs = instrss.head._2
                ctx.pc = handler
                ctx.handlers = ctx.handlers.tail
                ctx.instrss = instrss.tail
                ctx.depth = depth
                ctx
            }
    }
}

case class Satisfies(f: Char => Boolean) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.input match
    {
        case c::input if f(c) => 
            ctx.stack ::= c
            ctx.inputsz -= 1
            ctx.input = input
            ctx.pc += 1
            ctx
        case input =>
            if (ctx.handlers.isEmpty) { ctx.status = Failed; ctx }
            else
            {
                val (depth, handler) = ctx.handlers.head
                val diff = ctx.depth - depth - 1
                val instrss = if (diff > 0) ctx.instrss.drop(diff) else ctx.instrss
                ctx.status = Recover
                ctx.instrs = instrss.head._2
                ctx.pc = handler
                ctx.handlers = ctx.handlers.tail
                ctx.instrss = instrss.tail
                ctx.depth = depth
                ctx
            }
    }
}

case class StringTok(s: String) extends Instruction
{
    val ls = s.toList
    val sz = s.size
    override def apply(ctx: Context): Context = ctx.input match
    {
        case input if input.startsWith(ls) => 
            ctx.stack ::= s
            ctx.input = input.drop(sz)
            ctx.inputsz -= sz
            ctx.pc += 1
            ctx
        case inputs =>
            if (ctx.handlers.isEmpty) { ctx.status = Failed; ctx }
            else
            {
                val (depth, handler) = ctx.handlers.head
                val diff = ctx.depth - depth - 1
                val instrss = if (diff > 0) ctx.instrss.drop(diff) else ctx.instrss
                ctx.status = Recover
                ctx.instrs = instrss.head._2
                ctx.pc = handler
                ctx.handlers = ctx.handlers.tail
                ctx.instrss = instrss.tail
                ctx.depth = depth
                ctx
            }
    }
}

object InstructionTests
{
    def main(args: Array[String]): Unit =
    {
        //Console.in.read()
        //println(Apply(Push(20)(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(Context(Nil, Nil, Nil, Nil, Map.empty, Good, Nil, 0))))))
        //println(Apply(Push(20)(Apply(Push(10)(Push[Int=>Int=>Int](x => y => x + y)(Context(Nil, Nil, Nil, Nil, Map.empty, Good, Nil, 0)))))))
        import parsley.Parsley._
        //val p = lift2[Char, Char, String]((x, y) => x.toString + y.toString, 'a', 'b')
        //val p = 'a' <::> ('b' #> Nil)
        //val p = 'a' *> 'b' #> "ab"
        val p = many('a') <* 'b'
        println(p)
        reset()
        println(runParser(p, "aaaab"))
        val start = System.currentTimeMillis()
        val input = "aaaab".toList
        val sz = input.size
        for (i <- 0 to 10000000) runParser(p, input, sz)
        println(System.currentTimeMillis() - start)
    }
}