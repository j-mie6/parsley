package parsley

private [parsley] final class Exchange[A](private [Exchange] val x: A) extends Instr
{
    override def apply(ctx: Context)
    {
        ctx.exchangeStack(x)
        ctx.inc()
    }
    override def toString: String = s"Ex($x)"
}

private [parsley] final class FastFail[A](private [FastFail] val msggen: A=>String) extends Instr
{
    private[this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context)
    {
        val msg = msggen_(ctx.popStack())
        new Fail(msg)(ctx)
    }
    override def toString: String = "FastFail(?)"
}

// This instruction has GREAT potential, it should be integrated into peephole :)
// We should also make equivalents for Satisfy and String
// Also experiment: is CharTok; Exchange better than CharTokFastPerform with const?
private [parsley] final class CharTokFastPerform(private [CharTokFastPerform] val c: Char, private [CharTokFastPerform] val f: Any => Any) extends Instr
{
    private [this] val fc: Any = f(c)
    override def apply(ctx: Context)
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == c)
        {
                ctx.pushStack(fc)
                ctx.offset += 1
                ctx.inc()
        }
        else ctx.fail()
    }
    override def toString: String = s"ChrPerform($c, ?)"
}

// Extractor Objects
private [parsley] object Exchange
{
    def unapply[A](self: Exchange[A]): Option[A] = Some(self.x)
}

private [parsley] object FastFail
{
    def unapply[A](self: FastFail[A]): Option[A => String] = Some(self.msggen)
}

private [parsley] object CharTokFastPerform
{
    def unapply(self: CharTokFastPerform): Option[(Char, Any=>Any)] = Some((self.c, self.f))
}