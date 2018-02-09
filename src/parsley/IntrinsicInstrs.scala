package parsley

import scala.collection.mutable.ListBuffer

private [parsley] case object Cons extends Instr
{
    final override def apply(ctx: Context)
    {
        val stacktail = ctx.stack.tail
        ctx.stack = (stacktail.head::ctx.stack.head.asInstanceOf[List[_]])::stacktail.tail
        ctx.pc += 1
        ctx.stacksz -= 1
    }
}

private [parsley] final class Many[A](label: Int) extends Instr
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

private [parsley] final class SkipMany(label: Int) extends Instr
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

/* TODO: Chainl instruction has promising performance boost
   We need to ensure it actually performs correctly in all
   different error cases. If it does then we need to create
   a Chainr instruction too! */
private [parsley] final class Chainl[A](label: Int) extends Instr
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