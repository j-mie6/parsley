package parsley

import language.existentials
import scala.annotation.tailrec
import scala.collection.mutable.Buffer

trait Instruction
{
    def apply(ctx: Context): Unit
}

case class Perform[-A, +B](f: A => B) extends Instruction
{
    val g = f.asInstanceOf[Function[Any, Any]] 
    override def apply(ctx: Context): Unit =
    {
        ctx.stack = g(ctx.stack.head)::ctx.stack.tail 
        ctx.pc += 1
    }
    override def toString: String = "Perform(f)"
}

case class Push[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack ::= x
        ctx.stacksz += 1
        ctx.pc += 1
    }
}

case object Pop extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack = ctx.stack.tail
        ctx.stacksz -= 1
        ctx.pc += 1
    }
}

case object Flip extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        val y = ctx.stack.head
        val x = ctx.stack.tail.head
        ctx.stack = x::y::ctx.stack.tail.tail
        ctx.pc += 1
    }
}

case class Exchange[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack = x::ctx.stack.tail
        ctx.pc += 1
    }
}

case object Apply extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        val stacktail = ctx.stack.tail
        val f = stacktail.head.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]]
        ctx.stack = f(ctx.stack.head)::stacktail.tail
        ctx.pc += 1
        ctx.stacksz -= 1
    }
}

case object Cons extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        val stacktail = ctx.stack.tail
        ctx.stack = (stacktail.head::ctx.stack.head.asInstanceOf[List[_]])::stacktail.tail
        ctx.pc += 1
        ctx.stacksz -= 1
    }
}

case class Call(x: String) extends Instruction
{
    var instrs: InstructionBuffer = null
    override def apply(ctx: Context): Unit =
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = if (instrs == null)
        {
            instrs = ctx.subs(x)
            instrs
        } else instrs
        ctx.pc = 0
    }
}

case class DynSub[-A](f: A => InstructionBuffer) extends Instruction
{
    private[this] val g = f.asInstanceOf[Any => InstructionBuffer]
    override def apply(ctx: Context): Unit = 
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = g(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.pc = 0
        ctx.stacksz -= 1
    }
}

case class FastFail[A](msggen: A=>String) extends Instruction
{
    private[this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit = 
    {
        val msg = msggen_(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.stacksz -= 1
        new Fail(msg)(ctx)
    }
}

case class Fail(msg: String) extends Instruction
{
    // We need to do something with the message!
    override def apply(ctx: Context): Unit = ctx.fail()
}

case class TryBegin(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Unit = 
    {
        ctx.handlers ::= new Handler(ctx.depth, handler + ctx.pc, ctx.stacksz)
        ctx.inputs ::= new InputCache(ctx.inputsz, ctx.input)
        ctx.pc += 1
    }
}

case object TryEnd extends Instruction
{
    // FIXME: This handles too eagerly, investigate original parsec closer!
    override def apply(ctx: Context): Unit = 
    {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status == Good)
        {
            ctx.inputs = ctx.inputs.tail
            ctx.handlers = ctx.handlers.tail
            ctx.pc += 1
        }
        // Pop input off head then fail to next handler
        else
        {
            //ctx.status = Good
            val cache = ctx.inputs.head
            ctx.input = cache.input
            ctx.inputs = ctx.inputs.tail
            ctx.inputsz = cache.sz
            ctx.fail()
            //ctx.pc += 1
        }
    }
}

case class InputCheck(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Unit =
    {
        ctx.checkStack ::= ctx.inputsz
        ctx.handlers ::= new Handler(ctx.depth, handler + ctx.pc, ctx.stacksz)
        ctx.pc += 1
    }
}

case class JumpGood(label: Int) extends Instruction
{
    override def apply(ctx: Context): Unit = 
    {
        if (ctx.status == Good)
        {
            ctx.handlers = ctx.handlers.tail
            ctx.checkStack = ctx.checkStack.tail
            ctx.pc += label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) ctx.fail()
        else 
        {
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.pc += 1
        }
    }
}

class Many[A](label: Int) extends Instruction
{
    var acc: List[A] = Nil
    override def apply(ctx: Context): Unit = 
    {
        if (ctx.status == Good)
        {
            acc ::= ctx.stack.head.asInstanceOf[A]
            ctx.stack = ctx.stack.tail
            ctx.stacksz -= 1
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
            ctx.pc += label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) ctx.fail()
        else 
        {
            ctx.stack ::= acc.reverse
            ctx.stacksz += 1
            acc = Nil
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.pc += 1
        }
    }
}

case class CharTok(c: Char) extends Instruction
{
    private[this] val ac: Any = c
    override def apply(ctx: Context): Unit = ctx.input match
    {
        case `c`::input =>
            ctx.stack ::= ac
            ctx.stacksz += 1
            ctx.inputsz -= 1
            ctx.input = input
            ctx.pc += 1
        case inputs => ctx.fail()
    }
}

case class Satisfies(f: Char => Boolean) extends Instruction
{
    override def apply(ctx: Context): Unit = ctx.input match
    {
        case c::input if f(c) => 
            ctx.stack ::= c
            ctx.stacksz += 1
            ctx.inputsz -= 1
            ctx.input = input
            ctx.pc += 1
        case input => ctx.fail()
    }
}

case class StringTok(s: String) extends Instruction
{
    private[this] val ls = s.toList
    private[this] val sz = s.size
    override def apply(ctx: Context): Unit = ctx.input match
    {
        case input if input.startsWith(ls) => 
            ctx.stack ::= s
            ctx.input = input.drop(sz)
            ctx.stacksz += 1
            ctx.inputsz -= sz
            ctx.pc += 1
        case inputs => ctx.fail()
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
        val p = many(tryParse('a')) <* 'b'
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