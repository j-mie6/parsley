package parsley

import language.existentials

// Stack Manipulators
private [parsley] final class Push[A](private [Push] val x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.pushStack(x)
        ctx.inc()
    }
    override def toString: String = s"Push($x)"
    override def copy: Push[A] = new Push(x)
}

private [parsley] object Pop extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.popStack()
        ctx.inc()
    }
    override def toString: String = "Pop"
    override def copy: Pop.type = Pop
}

private [parsley] object Flip extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        val y = ctx.stack.head
        ctx.exchangeStack(ctx.stack.tail.head)
        ctx.stack.tail.head = y
        ctx.inc()
    }
    override def toString: String = "Flip"
    override def copy: Flip.type = Flip
}

// Primitives
private [parsley] class CharTok(protected final val c: Char) extends ExpectingInstr("\"" + c.toString + "\"")
{
    protected final val ac: Any = c
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == c)
        {
                ctx.pushStack(ac)
                ctx.offset += 1
                ctx.col += 1
                ctx.inc()
        }
        else ctx.fail(expected)
    }
    override final def toString: String = s"Chr($c)"
    override def copy_ : ExpectingInstr = new CharTok(c)
}

// FIXME: Doesn't adjust the position!
private [parsley] final class Satisfies(f: Char => Boolean) extends ExpectingInstr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && f(ctx.input(ctx.offset)))
        {
            ctx.pushStack(ctx.input(ctx.offset))
            ctx.offset += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def toString: String = "Sat(?)"
    override def copy_ : ExpectingInstr = new Satisfies(f)
}

// FIXME: Doesn't adjust the position!
private [parsley] final class StringTok(private [StringTok] val s: String) extends ExpectingInstr("\"" + s + "\"")
{
    private [this] val cs = s.toCharArray
    private [this] val sz = cs.length
    private def matches(input: Array[Char], unread: Int, offset: Int): Boolean =
    {
        val sz = this.sz
        if (unread < sz) false
        else
        {
            var i = offset
            var j = 0
            val cs = this.cs
            while (j < sz)
            {
                if (input(i) != cs(j)) return false
                i += 1
                j += 1
            }
            true
        }
    }
    override def apply(ctx: Context): Unit =
    {
        if (matches(ctx.input, ctx.inputsz - ctx.offset, ctx.offset))
        {
            ctx.pushStack(s)
            ctx.offset += sz
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"Str($s)"
    override def copy_ : ExpectingInstr = new StringTok(s)
}

// Applicative Functors
private [parsley] final class Perform[-A, +B](f: A => B) extends Instr
{
    private [Perform] val g = f.asInstanceOf[Function[Any, Any]]
    override def apply(ctx: Context): Unit =
    {
        ctx.exchangeStack(g(ctx.stack.head))
        ctx.inc()
    }
    override def toString: String = "Perform(?)"
    override def copy: Perform[A, B] = new Perform(f)
}

private [parsley] object Apply extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        val x = ctx.popStack()
        val f = ctx.stack.head.asInstanceOf[Function[Any, Any]]
        ctx.exchangeStack(f(x))
        ctx.inc()
    }
    override def toString: String = "Apply"
    override def copy: Apply.type = Apply
}

// Monadic
private [parsley] final class DynSub[-A](f: A => Array[Instr]) extends ExpectingInstr
{
    private [DynSub] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context): Unit =
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        ctx.instrs = g(ctx.popStack())
        ctx.pc = 0
        if (expected != null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
    }
    override def toString: String = "DynSub(?)"
    override def copy_ : ExpectingInstr = new DynSub(f)
}

// Control Flow
private [parsley] final class Call(private [Call] val x: String) extends ExpectingInstr
{
    // TEMPORARY FIXME
    // It has been determined that single use arrays are, in fact, not compatible with
    // mutable intrinsics. Any instruction streams containing stateful instructions must
    // be deep-copied and have those instructions deep-copied also. For now, we will just
    // deep copy all streams, but that's very inefficient.
    // CODO: Use lazy val?
    private [this] var instrs: UnsafeOption[Array[Instr]] = _
    override def apply(ctx: Context): Unit =
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        if (instrs == null) instrs = ctx.subs(x)
        ctx.instrs = instrs.map(_.copy)
        ctx.depth += 1
        if (expected != null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
        ctx.pc = 0
    }
    override def toString: String = s"Call($x)"
    override def copy_ : ExpectingInstr = new Call(x)
}

private [parsley] final class Call_(_p: =>DeepEmbedding.Parsley[_]) extends ExpectingInstr
{
    private [Call_] lazy val p = _p
    // TEMPORARY FIXME
    // It has been determined that single use arrays are, in fact, not compatible with
    // mutable intrinsics. Any instruction streams containing stateful instructions must
    // be deep-copied and have those instructions deep-copied also. For now, we will just
    // deep copy all streams, but that's very inefficient.
    // CODO: Use lazy val?
    private [this] var instrs: UnsafeOption[Array[Instr]] = _
    override def apply(ctx: Context): Unit =
    {
        ctx.calls ::= new Frame(ctx.pc + 1, ctx.instrs)
        if (instrs == null) instrs = p.instrs(DeepEmbedding.ExecutionTime)
        ctx.instrs = instrs.map(_.copy)
        ctx.depth += 1
        if (expected != null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
        ctx.pc = 0
    }
    override def toString: String = s"Call($p)"
    override def copy_ : ExpectingInstr = new Call_(p)
}

private [parsley] final class Fail(private [Fail] val msg: String) extends ExpectingInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.fail(expected)
        ctx.raw ::= msg
    }
    override def toString: String = s"Fail($msg)"
    override def copy_ : ExpectingInstr = new Fail(msg)
}

private [parsley] final class Unexpected(private [Unexpected] val msg: String) extends ExpectingInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.fail(expected)
        ctx.unexpected = msg
        ctx.unexpectAnyway = true
    }
    override def toString: String = s"Unexpected($msg)"
    override def copy_ : ExpectingInstr = new Unexpected(msg)
}

private [parsley] final class Empty extends ExpectingInstr(null)
{
    override def apply(ctx: Context): Unit = 
    {
        val strip = ctx.expected.isEmpty
        ctx.fail(expected)
        if (strip) ctx.unexpected = null
    }
    override def toString: String = "Empty"
    override def copy_ : ExpectingInstr = new Empty
}

private [parsley] final class PushHandler(override val label: Int) extends FwdJumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.handlers ::= new Handler(ctx.depth, label, ctx.stacksz)
        ctx.states ::= new State(ctx.offset, ctx.line, ctx.col)
        ctx.inc()
    }
    override def toString: String = s"PushHandler($label)"
    override def copy_ : FwdJumpInstr = new PushHandler(label)
}

private [parsley] object Try extends Instr
{
    override def apply(ctx: Context): Unit =
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
            ctx.line = state.line
            ctx.col = state.col
            ctx.fail()
        }
    }
    override def toString: String = "Try"
    override def copy: Try.type = Try
}

// State is not preserved after lookahead, the position is reset etc
// This should be the behaviour of the below when State is augmented
// but ensure this is the case later!
private [parsley] object Look extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.line = state.line
            ctx.col = state.col
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
    override def copy: Look.type = Look
}

private [parsley] final class InputCheck(override val label: Int) extends FwdJumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.checkStack ::= ctx.offset
        ctx.handlers ::= new Handler(ctx.depth, label, ctx.stacksz)
        ctx.inc()
    }
    override def toString: String = s"InputCheck($label)"
    override def copy_ : FwdJumpInstr = new InputCheck(label)
}

private [parsley] final class JumpGood(override val label: Int) extends FwdJumpInstr
{
    override def apply(ctx: Context): Unit =
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
    override def copy_ : FwdJumpInstr = new JumpGood(label)
}

// Extractor Objects
private [parsley] object Push
{
    def unapply(self: Push[_]): Option[Any] = Some(self.x)
}

private [parsley] object CharTok
{
    import scala.annotation.switch
    def apply(c: Char): CharTok = (c: @switch) match
    {
        case '\n' => new Newline
        case '\t' => new Tab
        case _ => new CharTok(c)
    }
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

//private [parsley] object Unexpected

//private [parsley] object Empty

private [parsley] object PushHandler
{
    def unapply(self: PushHandler): Option[Int] = Some(self.label)
}

private [parsley] object InputCheck
{
    def unapply(self: InputCheck): Option[Int] = Some(self.label)
}

private [parsley] object JumpGood
{
    def unapply(self: JumpGood): Option[Int] = Some(self.label)
}


