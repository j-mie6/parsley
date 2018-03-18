package parsley

import scala.collection.mutable.ListBuffer

private [parsley] final class Lift[A, B, C](f: (A, B) => C) extends Instr
{
    private [this] val g = f.asInstanceOf[(Any, Any) => C]
    override def apply(ctx: Context): Unit =
    {
        val y = ctx.popStack()
        ctx.exchangeStack(g(ctx.stack.head, y))
        ctx.inc()
    }
    override def toString: String = "Lift2(f)"
}

private [parsley] object Cons extends Instr
{
    final override def apply(ctx: Context): Unit =
    {
        val xs = ctx.popStack().asInstanceOf[List[_]]
        ctx.exchangeStack(ctx.stack.head::xs)
        ctx.inc()
    }
    override def toString: String = "Cons"
}

private [parsley] final class Many(val label: Int) extends Instr
{
    private[this] val acc: ListBuffer[Any] = ListBuffer.empty
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            acc += ctx.popStack()
            ctx.checkStack.head = ctx.offset
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) {acc.clear(); ctx.fail()}
        else
        {
            ctx.pushStack(acc.toList)
            acc.clear()
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"Many($label)"
    override def copy: Many = new Many(label)
}

private [parsley] final class SkipMany(val label: Int) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            ctx.popStack()
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

private [parsley] final class Chainl(val label: Int) extends Instr
{
    private[this] var acc: Any = _
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            // When acc is null, we are entering the instruction for the first time, a p will be on the stack
            if (acc == null)
            {
                val op = ctx.popStack()
                acc = ctx.stack.head
                ctx.exchangeStack(op)
            }
            acc = ctx.popStack().asInstanceOf[Any => Any](acc)
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
                ctx.pushStack(acc)
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

// TODO: What is the best way to implement this intrinsic?
private [parsley] final class Chainr(val label: Int) extends Instr
{
    override def apply(ctx: Context): Unit = ???
    override def copy: Chainr = new Chainr(label)
}

// Extractor Objects
private [parsley] object Many
{
    def unapply(self: Many): Option[Int] = Some(self.label)
}

private [parsley] object SkipMany
{
    def unapply(self: SkipMany): Option[Int] = Some(self.label)
}

private [parsley] object Chainl
{
    def unapply(self: Chainl): Option[Int] = Some(self.label)
}

private [parsley] object Chainr
{
    def unapply(self: Chainr): Option[Int] = Some(self.label)
}