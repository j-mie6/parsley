package parsley

import scala.collection.mutable.ListBuffer

private [parsley] final class Lift[A, B, C](f: (A, B) => C) extends Instr
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

private [parsley] object Cons extends Instr
{
    final override def apply(ctx: Context): Unit =
    {
        val xs = ctx.stack.pop[List[_]]()
        ctx.stack.exchange(ctx.stack.peek::xs)
        ctx.inc()
    }
    override def toString: String = "Cons"
}

private [parsley] final class Many(var label: Int) extends JumpInstr
{
    private[this] val acc: ListBuffer[Any] = ListBuffer.empty
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            acc += ctx.stack.upop()
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) {acc.clear(); ctx.fail()}
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
        else if (ctx.offset != ctx.checkStack.head) ctx.fail()
        else
        {
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"SkipMany($label)"
    override def copy: SkipMany = new SkipMany(label)
}

private [parsley] final class Chainl(var label: Int) extends JumpInstr
{
    private[this] var acc: Any = _
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            // When acc is null, we are entering the instruction for the first time, a p will be on the stack
            if (acc == null)
            {
                val op = ctx.stack.upop()
                acc = ctx.stack.upeek
                ctx.stack.exchange(op)
            }
            acc = ctx.stack.pop[Any => Any]()(acc)
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) {acc = null; ctx.fail()}
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
    override def toString: String = s"Chainl($label)"
    override def copy: Chainl = new Chainl(label)
}

private [parsley] final class Chainr(var label: Int) extends JumpInstr
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
        else if (ctx.offset != ctx.checkStack.head) {acc = null; ctx.fail()}
        else
        {
            ctx.stack.push(if (acc == null) identity[Any](_) else acc)
            acc = null
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"Chainr($label)"
    override def copy: Chainr = new Chainr(label)
}