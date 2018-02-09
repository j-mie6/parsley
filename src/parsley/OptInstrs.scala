package parsley

private [parsley] final case class Exchange[A](x: A) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.stack = x::ctx.stack.tail
        ctx.pc += 1
    }
}

private [parsley] final case class FastFail[A](msggen: A=>String) extends Instr
{
    private[this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context)
    {
        val msg = msggen_(ctx.stack.head)
        ctx.stack = ctx.stack.tail
        ctx.stacksz -= 1
        new Fail(msg)(ctx)
    }
}

// This instruction has GREAT potential, it should be integrated into peephole :)
// We should also make equivalents for Satisfy and String
// Also experiment: is CharTok; Exchange better than CharTokFastPerform with const?
private [parsley] final case class CharTokFastPerform(c: Char, f: Function[Char, Any]) extends Instr
{
    // This optimisation is probably very unsafe?
    private[this] val fc: Any = f(c)
    override def apply(ctx: Context)
    {
        ctx.input match
        {
            case `c`::input =>
                ctx.stack ::= fc
                ctx.stacksz += 1
                ctx.inputsz -= 1
                ctx.input = input
                ctx.pc += 1
            case inputs => ctx.fail()
        }
    }
    override def toString: String = s"CharTokFastPerform($c)"
}