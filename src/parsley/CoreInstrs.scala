package parsley

import language.existentials

// Stack Manipulators
private [parsley] final class Push[A](private [Push] val x: A) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.pushStack(x)
        ctx.inc()
    }
    override def toString: String = s"Push($x)"
}

private [parsley] object Pop extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.popStack()
        ctx.inc()
    }
    override def toString: String = "Pop"
}

private [parsley] object Flip extends Instr
{
    override def apply(ctx: Context)
    {
        val y = ctx.stack.head
        ctx.exchangeStack(ctx.stack.tail.head)
        ctx.stack.tail.head = y
        ctx.inc()
    }
    override def toString: String = "Flip"
}

// Primitives
private [parsley] final class CharTok(private [CharTok] val c: Char) extends Instr
{
    private [this] val ac: Any = c
    override def apply(ctx: Context)
    {
        if (ctx.offset < ctx.input.length && ctx.input(ctx.offset) == c)
        {
                ctx.pushStack(ac)
                ctx.offset += 1
                ctx.inc()
        }
        else ctx.fail()
    }
    override def toString: String = s"Chr($c)"
}

private [parsley] final class Satisfies(f: Char => Boolean) extends Instr
{
    override def apply(ctx: Context)
    {
        /*ctx.input match
        {
            case c::input if f(c) =>
                ctx.pushStack(c)
                ctx.inputsz -= 1
                ctx.input = input
                ctx.inc()
            case _ => ctx.fail()
        }*/
    }
    override def toString: String = "Sat(?)"
}

private [parsley] final class StringTok(private [StringTok] val s: String) extends Instr
{
    private [this] val ls = s.toList
    private [this] val sz = s.length
    override def apply(ctx: Context)
    {
        val input = ctx.input
        //FIXME no longer true!
        if (input.startsWith(ls))
        {
            ctx.pushStack(s)
            ctx.offset += sz
            ctx.inc()
        }
        else ctx.fail()
    }
    override def toString: String = s"Str($s)"
}

// Applicative Functors
private [parsley] final class Perform[-A, +B](f: A => B) extends Instr
{
    private [Perform] val g = f.asInstanceOf[Function[Any, Any]]
    override def apply(ctx: Context)
    {
        ctx.exchangeStack(g(ctx.stack.head))
        ctx.inc()
    }
    override def toString: String = "Perform(?)"
}

private [parsley] object Apply extends Instr
{
    override def apply(ctx: Context)
    {
        val x = ctx.popStack()
        val f = ctx.stack.head.asInstanceOf[Function[Any, Any]]
        ctx.exchangeStack(f(x))
        ctx.inc()
    }
    override def toString: String = "Apply"
}

// Monadic
private [parsley] final class DynSub[-A](f: A => Array[Instr]) extends Instr
{
    private [DynSub] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context)
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = g(ctx.popStack())
        ctx.pc = 0
    }
    override def toString: String = "DynSub(?)"
}

// Control Flow
private [parsley] final class Call(private [Call] val x: String) extends Instr
{
    private [this] var instrs: Array[Instr] = _
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
    override def toString: String = "Call($x)"
}

private [parsley] final class Fail(private [Fail] val msg: String) extends Instr
{
    // We need to do something with the message!
    override def apply(ctx: Context) { ctx.fail() }
    override def toString: String = s"Fail($msg)"
}

private [parsley] final class PushHandler(private [PushHandler] val handler: Int) extends FwdJumpInstr
{
    override def apply(ctx: Context)
    {
        ctx.handlers ::= new Handler(ctx.depth, handler, ctx.stacksz)
        ctx.states ::= new State(ctx.offset)
        ctx.inc()
    }
    override def toString: String = s"PushHandler($handler)"
}

private [parsley] object Try extends Instr
{
    override def apply(ctx: Context)
    {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status eq Good)
        {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        // Pop input off head then fail to next handler
        else
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.fail()
        }
    }
    override def toString: String = "Try"
}

// State is not preserved after lookahead, the position is reset etc
// This should be the behaviour of the below when State is augmented
// but ensure this is the case later!
private [parsley] object Look extends Instr
{
    override def apply(ctx: Context)
    {
        if (ctx.status eq Good)
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else
        {
            ctx.states = ctx.states.tail
            ctx.fail()
        }
    }
    override def toString: String = "Look"
}

private [parsley] final class InputCheck(private [InputCheck] val handler: Int) extends FwdJumpInstr
{
    override def apply(ctx: Context)
    {
        ctx.checkStack ::= ctx.offset
        ctx.handlers ::= new Handler(ctx.depth, handler, ctx.stacksz)
        ctx.inc()
    }
    override def toString: String = s"InputCheck($handler)"
}

private [parsley] final class JumpGood(private [JumpGood] val label: Int) extends FwdJumpInstr
{
    override def apply(ctx: Context)
    {
        if (ctx.status eq Good)
        {
            ctx.handlers = ctx.handlers.tail
            ctx.checkStack = ctx.checkStack.tail
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) ctx.fail()
        else
        {
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"JumpGood($label)"
}

// Extractor Objects
private [parsley] object Push
{
    def unapply(self: Push[_]): Option[Any] = Some(self.x)
}

private [parsley] object CharTok
{
    def unapply(self: CharTok): Option[Char] = Some(self.c)
}

//private [parsley] object Satisfies

//private [parsley] object StringTok

private [parsley] object Perform
{
    def unapply(self: Perform[_, _]): Option[Any => Any] = Some(self.g)
}

//private [parsley] object DynSub

private [parsley] object Call
{
    def unapply(self: Call): Option[String] = Some(self.x)
}

//private [parsley] object Fail

private [parsley] object PushHandler
{
    def unapply(self: PushHandler): Option[Int] = Some(self.handler)
}

private [parsley] object InputCheck
{
    def unapply(self: InputCheck): Option[Int] = Some(self.handler)
}

private [parsley] object JumpGood
{
    def unapply(self: JumpGood): Option[Int] = Some(self.label)
}


