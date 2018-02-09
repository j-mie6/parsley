package parsley

import language.existentials

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