package parsley

import language.existentials

private [parsley] final case class Perform[-A, +B](f: A => B) extends Instr
{
    private[this] val g = f.asInstanceOf[Function[Any, Any]]
    override def apply(ctx: Context)
    {
        ctx.stack = g(ctx.stack.head)::ctx.stack.tail
        ctx.pc += 1
    }
    override def toString: String = "Perform(?)"
}

private [parsley] final case class Push[A](x: A) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.stack ::= x
        ctx.stacksz += 1
        ctx.pc += 1
    }
}

private [parsley] case object Pop extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.stack = ctx.stack.tail
        ctx.stacksz -= 1
        ctx.pc += 1
    }
}

private [parsley] case object Flip extends Instr
{
    override def apply(ctx: Context)
    {
        val y = ctx.stack.head
        val x = ctx.stack.tail.head
        ctx.stack = x::y::ctx.stack.tail.tail
        ctx.pc += 1
    }
}

private [parsley] case object Apply extends Instr
{
    override def apply(ctx: Context)
    {
        val stacktail = ctx.stack.tail
        val f = stacktail.head.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]]
        ctx.stack = f(ctx.stack.head)::stacktail.tail
        ctx.pc += 1
        ctx.stacksz -= 1
    }
}

private [parsley] final case class Call(x: String) extends Instr
{
    private[this] var instrs: Array[Instr] = _
    override def apply(ctx: Context)
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

private [parsley] final case class DynSub[-A](f: A => Array[Instr]) extends Instr
{
    private[this] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context)
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = g(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.pc = 0
        ctx.stacksz -= 1
    }
}

private [parsley] final class Fail(msg: String) extends Instr
{
    // We need to do something with the message!
    override def apply(ctx: Context) { ctx.fail() }
    override def toString: String = s"Fail($msg)"
}

private [parsley] final case class PushHandler(handler: Int) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.handlers ::= new Handler(ctx.depth, handler + ctx.pc, ctx.stacksz)
        ctx.states ::= new State(ctx.inputsz, ctx.input)
        ctx.pc += 1
    }
}

private [parsley] case object Try extends Instr
{
    override def apply(ctx: Context)
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
private [parsley] case object Look extends Instr
{
    override def apply(ctx: Context)
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

private [parsley] final case class InputCheck(handler: Int) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.checkStack ::= ctx.inputsz
        ctx.handlers ::= new Handler(ctx.depth, handler + ctx.pc, ctx.stacksz)
        ctx.pc += 1
    }
}

private [parsley] final case class JumpGood(label: Int) extends Instr
{
    override def apply(ctx: Context)
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

private [parsley] final case class CharTok(c: Char) extends Instr
{
    private[this] val ac: Any = c
    override def apply(ctx: Context)
    {
        ctx.input match
        {
            case `c`::input =>
                ctx.stack ::= ac
                ctx.stacksz += 1
                ctx.inputsz -= 1
                ctx.input = input
                ctx.pc += 1
            case _ => ctx.fail()
        }
    }
}

private [parsley] final class Satisfies(f: Char => Boolean) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.input match
        {
            case c::input if f(c) =>
                ctx.stack ::= c
                ctx.stacksz += 1
                ctx.inputsz -= 1
                ctx.input = input
                ctx.pc += 1
            case _ => ctx.fail()
        }
    }
    override def toString: String = "Satisfies(?)"
}

private [parsley] final case class StringTok(s: String) extends Instr
{
    private[this] val ls = s.toList
    private[this] val sz = s.length
    override def apply(ctx: Context)
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