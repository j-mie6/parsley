package parsley

import language.existentials
import scala.annotation.tailrec
import scala.collection.mutable.{Buffer, ListBuffer}

sealed abstract class Instruction
{
    def apply(ctx: Context)
}

final case class Perform[-A, +B](f: A => B) extends Instruction
{
    final private[this] val g = f.asInstanceOf[Function[Any, Any]] 
    final override def apply(ctx: Context)
    {
        ctx.stack = g(ctx.stack.head)::ctx.stack.tail
        ctx.pc += 1
    }
    final override def toString: String = "Perform(f)"
}

final case class Push[A](x: A) extends Instruction
{
    final override def apply(ctx: Context)
    {
        ctx.stack ::= x
        ctx.stacksz += 1
        ctx.pc += 1
    }
}

case object Pop extends Instruction
{
    final override def apply(ctx: Context)
    {
        ctx.stack = ctx.stack.tail
        ctx.stacksz -= 1
        ctx.pc += 1
    }
}

case object Flip extends Instruction
{
    final override def apply(ctx: Context)
    {
        val y = ctx.stack.head
        val x = ctx.stack.tail.head
        ctx.stack = x::y::ctx.stack.tail.tail
        ctx.pc += 1
    }
}

final case class Exchange[A](x: A) extends Instruction
{
    final override def apply(ctx: Context)
    {
        ctx.stack = x::ctx.stack.tail
        ctx.pc += 1
    }
}

case object Apply extends Instruction
{
    final override def apply(ctx: Context)
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
    final override def apply(ctx: Context)
    {
        val stacktail = ctx.stack.tail
        ctx.stack = (stacktail.head::ctx.stack.head.asInstanceOf[List[_]])::stacktail.tail
        ctx.pc += 1
        ctx.stacksz -= 1
    }
}

final case class Call(x: String) extends Instruction
{
    final private[this] var instrs: InstructionBuffer = null
    final override def apply(ctx: Context)
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = if (instrs == null)
        {
            instrs = ctx.subs(x)
            instrs
        } else instrs
        ctx.depth += 1
        ctx.pc = 0
    }
}

final case class DynSub[-A](f: A => InstructionBuffer) extends Instruction
{
    final private[this] val g = f.asInstanceOf[Any => InstructionBuffer]
    final override def apply(ctx: Context)
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = g(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.pc = 0
        ctx.stacksz -= 1
    }
}

final case class FastFail[A](msggen: A=>String) extends Instruction
{
    final private[this] val msggen_ = msggen.asInstanceOf[Any => String]
    final override def apply(ctx: Context)
    {
        val msg = msggen_(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.stacksz -= 1
        new Fail(msg)(ctx)
    }
}

final case class Fail(msg: String) extends Instruction
{
    // We need to do something with the message!
    final override def apply(ctx: Context) = ctx.fail()
}

final case class PushHandler(handler: Int) extends Instruction
{
    final override def apply(ctx: Context)
    {
        ctx.handlers ::= new Handler(ctx.depth, handler + ctx.pc, ctx.stacksz)
        ctx.states ::= new State(ctx.inputsz, ctx.input)
        ctx.pc += 1
    }
}

case object Try extends Instruction
{
    final override def apply(ctx: Context)
    {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status == Good)
        {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.pc += 1
        }
        // Pop input off head then fail to next handler
        else
        {
            val state = ctx.states.head
            ctx.input = state.input
            ctx.states = ctx.states.tail
            ctx.inputsz = state.sz
            ctx.fail()
        }
    }
}

// State is not preserved after lookahead, the position is reset etc
// This should be the behaviour of the below when State is augmented
// but ensure this is the case later!
case object Look extends Instruction
{
    final override def apply(ctx: Context)
    {
        if (ctx.status == Good)
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.input = state.input
            ctx.inputsz = state.sz
            ctx.handlers = ctx.handlers.tail
            ctx.pc += 1
        }
        else
        {
            ctx.states = ctx.states.tail
            ctx.fail()
        }
    }
}

final case class InputCheck(handler: Int) extends Instruction
{
    final override def apply(ctx: Context)
    {
        ctx.checkStack ::= ctx.inputsz
        ctx.handlers ::= new Handler(ctx.depth, handler + ctx.pc, ctx.stacksz)
        ctx.pc += 1
    }
}

final case class JumpGood(label: Int) extends Instruction
{
    final override def apply(ctx: Context)
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

final class Many[A](label: Int) extends Instruction
{
    final private[this] val acc: ListBuffer[A] = ListBuffer.empty
    final override def apply(ctx: Context)
    {
        if (ctx.status == Good)
        {
            acc += ctx.stack.head.asInstanceOf[A]
            ctx.stack = ctx.stack.tail
            ctx.stacksz -= 1
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
            ctx.pc += label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) {acc.clear(); ctx.fail()}
        else 
        {
            ctx.stack ::= acc.toList
            ctx.stacksz += 1
            acc.clear()
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.pc += 1
        }
    }
}

final class Chainl[A](label: Int) extends Instruction
{
    final private[this] var acc: Any = null
    final override def apply(ctx: Context)
    {
        // When acc is null, we are entering the instruction for the first time, a p will be on the stack
        if (acc == null)
        {
            acc = ctx.stack.tail.head
            ctx.stack = ctx.stack.head::ctx.stack.tail.tail
            ctx.stacksz -= 1
        }
        if (ctx.status == Good)
        {
            acc = ctx.stack.head.asInstanceOf[Function[Any, Any]](acc)
            ctx.stack = ctx.stack.tail
            ctx.stacksz -= 1
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
            ctx.pc += label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) {acc = null; ctx.fail()}
        else 
        {
            ctx.stack ::= acc
            ctx.stacksz += 1
            acc = null
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.pc += 1
        }
    }
}

final class SkipMany(label: Int) extends Instruction
{
    final override def apply(ctx: Context)
    {
        if (ctx.status == Good)
        {
            ctx.stack = ctx.stack.tail
            ctx.stacksz -= 1
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
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

final case class CharTok(c: Char) extends Instruction
{
    final private[this] val ac: Any = c
    final override def apply(ctx: Context)
    {
        ctx.input match
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
}

final class Satisfies(f: Char => Boolean) extends Instruction
{
    final override def apply(ctx: Context)
    {
        ctx.input match
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
}

final case class StringTok(s: String) extends Instruction
{
    final private[this] val ls = s.toList
    final private[this] val sz = s.size
    final override def apply(ctx: Context)
    {
        val input = ctx.input
        if (input.startsWith(ls))
        {
            ctx.stack ::= s
            ctx.input = input.drop(sz)
            ctx.stacksz += 1
            ctx.inputsz -= sz
            ctx.pc += 1
        }
        else ctx.fail()
    }
}

// It's 2018 and Labels are making a come-back, along with 2 pass assembly
final case class Label(i: Int) extends Instruction
{
    def apply(ctx: Context) { ??? }
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
        val q = chainl1_('1'.map(_.toInt), '+' #> ((x: Int) => (y: Int) => x + y))
        //val q = chainPre('1'.map(_.toInt), '+' #> ((x: Int) => x + 1))
        println(p)
        println(q)
        reset()
        println(runParser(p, "aaaab"))
        println(runParser(q, "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"))
        //println(runParser(q, "++++++++++++++++1"))
        val start = System.currentTimeMillis()
        val input = "aaaab".toList
        val input_ = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1".toList
        //val input_ = "++++++++++++++++1".toList
        val sz = input.size
        val sz_ = input_.size
        for (i <- 0 to 10000000) runParser(q, input_, sz_)
        println(System.currentTimeMillis() - start)
    }
}