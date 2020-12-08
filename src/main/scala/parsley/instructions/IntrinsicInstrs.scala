package parsley.instructions

import parsley.UnsafeOption
import parsley.deepembedding.DeepEmbedding
import Stack.isEmpty

import scala.collection.mutable.ListBuffer

private [parsley] final class Lift2[A, B, C](f: (A, B) => C) extends Instr
{
    private [this] val g = f.asInstanceOf[(Any, Any) => C]
    override def apply(ctx: Context): Unit =
    {
        val y = ctx.stack.upop()
        ctx.stack.exchange(g(ctx.stack.peek, y))
        ctx.inc()
    }
    override def toString: String = "Lift2(f)"
}

private [parsley] final class Lift3[A, B, C, D](f: (A, B, C) => D) extends Instr
{
    private [this] val g = f.asInstanceOf[(Any, Any, Any) => D]
    override def apply(ctx: Context): Unit =
    {
        val z = ctx.stack.upop()
        val y = ctx.stack.upop()
        ctx.stack.exchange(g(ctx.stack.peek, y, z))
        ctx.inc()
    }
    override def toString: String = "Lift3(f)"
}

private [parsley] final class Many(var label: Int) extends JumpInstr with Stateful
{
    private [this] val acc: ListBuffer[Any] = ListBuffer.empty
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            acc += ctx.stack.upop()
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) { ctx.checkStack = ctx.checkStack.tail; acc.clear(); ctx.fail() }
        else
        {
            ctx.stack.push(acc.toList)
            acc.clear()
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"Many($label)"
    override def copy: Many = new Many(label)
}

private [parsley] final class SkipMany(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            ctx.stack.pop_()
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) { ctx.checkStack = ctx.checkStack.tail; ctx.fail() }
        else
        {
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.stack.push(())
            ctx.inc()
        }
    }
    override def toString: String = s"SkipMany($label)"
}

private [parsley] final class ChainPost(var label: Int) extends JumpInstr with Stateful
{
    private [this] var acc: Any = _
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            // When acc is null, we are entering the instruction for the first time, a p will be on the stack
            if (acc == null)
            {
                // after this point, the inputCheck will roll back one too many items on the stack, because this item
                // was consumed. It should be adjusted
                val op = ctx.stack.upop()
                acc = ctx.stack.upeek
                ctx.stack.exchange(op)
                ctx.handlers.head.stacksz -= 1
            }
            acc = ctx.stack.pop[Any => Any]()(acc)
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) { ctx.checkStack = ctx.checkStack.tail; acc = null; ctx.fail() }
        else
        {
            // When acc is null, we have entered for first time but the op failed, so the result is already on the stack
            if (acc != null)
            {
                ctx.stack.push(acc)
                acc = null
            }
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"ChainPost($label)"
    override def copy: ChainPost = new ChainPost(label)
}

private [parsley] final class ChainPre(var label: Int) extends JumpInstr with Stateful
{
    private var acc: Any => Any = _
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            // If acc is null we are entering the instruction, so nothing to compose, this saves on an identity call
            if (acc == null) acc = ctx.stack.pop[Any => Any]()
            // We perform the acc after the tos function; the tos function is "closer" to the final p
            else acc = ctx.stack.pop[Any => Any]().andThen(acc)
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) { ctx.checkStack = ctx.checkStack.tail; acc = null; ctx.fail() }
        else
        {
            ctx.stack.push(if (acc == null) identity[Any] _ else acc)
            acc = null
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"ChainPre($label)"
    override def copy: ChainPre = new ChainPre(label)
}
private [parsley] final class Chainl[A, B](var label: Int, _wrap: A => B) extends JumpInstr with Stateful
{
    private [this] val wrap: Any => B = _wrap.asInstanceOf[Any => B]
    private [this] var acc: Any = _
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            val y = ctx.stack.upop()
            val op = ctx.stack.pop[(Any, Any) => Any]()
            // When acc is null, we are entering the instruction for the first time, a p will be on the stack
            if (acc == null)
            {
                // after this point, the inputCheck will roll back one too many items on the stack, because this item
                // was consumed. It should be adjusted
                acc = op(wrap(ctx.stack.upop()), y)
                ctx.handlers.head.stacksz -= 1
            }
            else acc = op(acc, y)
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head)
        {
            ctx.checkStack = ctx.checkStack.tail
            acc = null
            ctx.fail()
        }
        else
        {
            ctx.checkStack = ctx.checkStack.tail
            // if acc is null this is first entry, p already on the stack!
            if (acc != null) ctx.stack.push(acc)
            acc = null
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"Chainl($label)"
    override def copy: Chainl[A, B] = new Chainl(label, wrap)
}
private [parsley] final class Chainr[A, B](var label: Int, _wrap: A => B) extends JumpInstr with Stateful
{
    private [this] val wrap: Any => B = _wrap.asInstanceOf[Any => B]
    private [this] var acc: Any => Any = _
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            val f = ctx.stack.pop[(Any, Any) => Any]()
            val x = ctx.stack.upop()
            // If acc is null we are entering the instruction, so nothing to compose, this saves on an identity call
            if (acc == null) acc = (y: Any) => f(x, y)
            // We perform the acc after the tos function; the tos function is "closer" to the final p
            else
            {
                val acc_ = acc
                acc = (y: Any) => acc_(f(x, y))
            }
            // Pop H2 off the stack
            ctx.handlers = ctx.handlers.tail
            ctx.checkStack = ctx.checkStack.tail
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head)
        {
            // H1 might still be on the stack
            if (!isEmpty(ctx.handlers) && ctx.handlers.head.pc == ctx.pc)
            {
                ctx.handlers = ctx.handlers.tail
                ctx.checkStack = ctx.checkStack.tail.tail
            }
            else ctx.checkStack = ctx.checkStack.tail
            acc = null
            ctx.fail()
        }
        else
        {
            // H1 is on the stack, so p succeeded, just not op
            if (!isEmpty(ctx.handlers) && ctx.handlers.head.pc == ctx.pc)
            {
                if (acc != null) ctx.stack.exchange(acc(wrap(ctx.stack.upeek)))
                else ctx.stack.exchange(wrap(ctx.stack.upeek))
                ctx.checkStack = ctx.checkStack.tail.tail
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
                ctx.status = Good
            }
            // p did not succeed and hence neither did op
            else
            {
                ctx.checkStack = ctx.checkStack.tail
                ctx.fail()
            }
            acc = null
        }
    }
    override def toString: String = s"Chainr($label)"
    override def copy: Chainr[A, B] = new Chainr(label, wrap)
}

private [parsley] final class SepEndBy1(var label: Int) extends JumpInstr with Stateful
{
    private [this] val acc: ListBuffer[Any] = ListBuffer.empty
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            ctx.stack.pop_()
            acc += ctx.stack.upop()
            // Pop H2 off the stack
            ctx.handlers = ctx.handlers.tail
            // Pop a check off the stack and edit the other
            ctx.checkStack = ctx.checkStack.tail
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head)
        {
            // H1 might still be on the stack
            if (!isEmpty(ctx.handlers) && ctx.handlers.head.pc == ctx.pc)
            {
                ctx.handlers = ctx.handlers.tail
                ctx.checkStack = ctx.checkStack.tail.tail
            }
            else ctx.checkStack = ctx.checkStack.tail
            acc.clear()
            ctx.fail()
        }
        else
        {
            // H1 is on the stack, so p succeeded, just not sep
            if (!isEmpty(ctx.handlers) && ctx.handlers.head.pc == ctx.pc)
            {
                acc += ctx.stack.upop()
                ctx.checkStack = ctx.checkStack.tail.tail
                ctx.handlers = ctx.handlers.tail
            }
            else ctx.checkStack = ctx.checkStack.tail
            if (acc.isEmpty) ctx.fail()
            else
            {
                ctx.stack.push(acc.toList)
                acc.clear()
                ctx.status = Good
                ctx.inc()
            }
        }
    }
    override def toString: String = s"SepEndBy1($label)"
    override def copy: SepEndBy1 = new SepEndBy1(label)
}

private [parsley] final class ManyUntil(var label: Int) extends JumpInstr with Stateful
{
    private [this] val acc: ListBuffer[Any] = ListBuffer.empty
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            val x = ctx.stack.upop()
            if (x == DeepEmbedding.ManyUntil.Stop)
            {
                ctx.stack.push(acc.toList)
                acc.clear()
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
            }
            else
            {
                acc += x
                ctx.pc = label
            }
        }
        // ManyUntil is a fallthrough handler, it must be visited during failure, but does nothing to the external state
        else { acc.clear(); ctx.fail() }
    }
    override def toString: String = s"ManyUntil($label)"
    override def copy: ManyUntil = new ManyUntil(label)
}

private [parsley] final class If(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.stack.pop()) ctx.pc = label
        else ctx.inc()
    }
    override def toString: String = s"If(true: $label)"
}

private [parsley] final class Ensure[A](pred: A=>Boolean, expected: UnsafeOption[String]) extends Instr
{
    private [this] val pred_ = pred.asInstanceOf[Any=>Boolean]
    override def apply(ctx: Context): Unit =
    {
        if (pred_(ctx.stack.upeek)) ctx.inc()
        else
        {
            val strip = ctx.expected.isEmpty
            ctx.fail(expected)
            if (strip) ctx.unexpected = null
        }
    }
    override def toString: String = "Ensure(?)"
}

private [parsley] final class Guard[A](pred: A=>Boolean, msg: String, expected: UnsafeOption[String]) extends Instr
{
    private [this] val pred_ = pred.asInstanceOf[Any=>Boolean]
    override def apply(ctx: Context): Unit =
    {
        if (pred_(ctx.stack.upeek)) ctx.inc()
        else
        {
            ctx.stack.pop_() //this might not be needed? drop handles in fail
            ctx.fail(expected)
            ctx.raw ::= msg
        }
    }
    override def toString: String = s"Guard(?, $msg)"
}

private [parsley] final class FastGuard[A](pred: A=>Boolean, msggen: A=>String, expected: UnsafeOption[String]) extends Instr
{
    private [this] val pred_ = pred.asInstanceOf[Any=>Boolean]
    private [this] val msggen_ = msggen.asInstanceOf[Any=>String]
    override def apply(ctx: Context): Unit =
    {
        if (pred_(ctx.stack.upeek)) ctx.inc()
        else
        {
            val msg = msggen_(ctx.stack.upop())
            ctx.fail(expected)
            ctx.raw ::= msg
        }
    }
    override def toString: String = "FastGuard(?, ?)"
}

private [parsley] final class FastFail[A](msggen: A=>String, expected: UnsafeOption[String]) extends Instr
{
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit =
    {
        val msg = msggen_(ctx.stack.upop())
        ctx.fail(expected)
        ctx.raw ::= msg
    }
    override def toString: String = "FastFail(?)"
}

private [parsley] final class FastUnexpected[A](msggen: A=>String, expected: UnsafeOption[String]) extends Instr
{
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit =
    {
        val msg = msggen_(ctx.stack.upop())
        ctx.fail(expected)
        ctx.unexpected = msg
        ctx.unexpectAnyway = true
    }
    override def toString: String = "FastUnexpected(?)"
}

private [parsley] final class NotFollowedBy(expected: UnsafeOption[String]) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        // Recover the previous state; notFollowedBy NEVER consumes input
        val state = ctx.states.head
        ctx.states = ctx.states.tail
        ctx.offset = state.offset
        ctx.line = state.line
        ctx.col = state.col
        ctx.regs = state.regs
        // A previous success is a failure
        if (ctx.status eq Good)
        {
            ctx.handlers = ctx.handlers.tail
            val x = ctx.stack.upop()
            ctx.fail(expected)
            ctx.unexpected = "\"" + x.toString + "\""
            ctx.unexpectAnyway = true
        }
        // A failure is what we wanted
        else
        {
            ctx.status = Good
            ctx.stack.push(())
            ctx.inc()
        }
    }
    override def toString: String = "NotFollowedBy"
}

private [parsley] class Eof(_expected: UnsafeOption[String]) extends Instr
{
    val expected: String = if (_expected == null) "end of input" else _expected
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset == ctx.inputsz)
        {
            ctx.stack.push(())
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override final def toString: String = "Eof"
}