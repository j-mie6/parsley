package parsley

import scala.collection.mutable.ListBuffer

private [parsley] object Cons extends Instr
{
    final override def apply(ctx: Context)
    {
        val stacktail = ctx.stack.tail
        ctx.stack = (stacktail.head::ctx.stack.head.asInstanceOf[List[_]])::stacktail.tail
        ctx.inc()
        ctx.decStack()
    }
    override def toString: String = "Cons"
}

private [parsley] final class Many[A](private [Many] val label: Int) extends Instr
{
    private[this] val acc: ListBuffer[A] = ListBuffer.empty
    override def apply(ctx: Context)
    {
        if (ctx.status == Good)
        {
            acc += ctx.stack.head.asInstanceOf[A]
            ctx.stack = ctx.stack.tail
            ctx.decStack()
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) {acc.clear(); ctx.fail()}
        else
        {
            ctx.stack ::= acc.toList
            ctx.incStack()
            acc.clear()
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"Many($label)"
}

private [parsley] final class SkipMany(private [SkipMany] val label: Int) extends Instr
{
    override def apply(ctx: Context)
    {
        if (ctx.status == Good)
        {
            ctx.stack = ctx.stack.tail
            ctx.decStack()
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) ctx.fail()
        else
        {
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"SkipMany($label)"
}

/* TODO: Chainl instruction has promising performance boost
   We need to ensure it actually performs correctly in all
   different error cases. If it does then we need to create
   a Chainr instruction too! */
private [parsley] final class Chainl[A](private [Chainl] val label: Int) extends Instr
{
    private[this] var acc: Any = _
    override def apply(ctx: Context)
    {
        // When acc is null, we are entering the instruction for the first time, a p will be on the stack
        if (acc == null)
        {
            acc = ctx.stack.tail.head
            ctx.stack = ctx.stack.head::ctx.stack.tail.tail
            ctx.decStack()
        }
        if (ctx.status == Good)
        {
            acc = ctx.stack.head.asInstanceOf[Function[Any, Any]](acc)
            ctx.stack = ctx.stack.tail
            ctx.decStack()
            ctx.checkStack = ctx.inputsz::ctx.checkStack.tail
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.inputsz != ctx.checkStack.head) {acc = null; ctx.fail()}
        else
        {
            ctx.stack ::= acc
            ctx.incStack()
            acc = null
            ctx.checkStack = ctx.checkStack.tail
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"Chainl($label)"
}

// Extractor Objects
private [parsley] object Many
{
    def unapply(self: Many[_]): Option[Int] = Some(self.label)
}

private [parsley] object SkipMany
{
    def unapply(self: SkipMany): Option[Int] = Some(self.label)
}

private [parsley] object Chainl
{
    def unapply(self: Chainl[_]): Option[Int] = Some(self.label)
}