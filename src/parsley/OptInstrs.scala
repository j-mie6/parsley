package parsley

private [parsley] final class Exchange[A](private [Exchange] val x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.exchangeStack(x)
        ctx.inc()
    }
    override def toString: String = s"Ex($x)"
}

private [parsley] final class FastFail[A](private [FastFail] val msggen: A=>String) extends Instr
{
    private[this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit =
    {
        val msg = msggen_(ctx.popStack())
        new Fail(msg)(ctx)
    }
    override def toString: String = "FastFail(?)"
}

private [parsley] class Newline() extends CharTok('\n')
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\n')
        {
            ctx.pushStack(ac)
            ctx.offset += 1
            ctx.col = 1
            ctx.line += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def copy_(): ExpectingInstr = new Newline()
}

private [parsley] class Tab() extends CharTok('\t')
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\t')
        {
            ctx.pushStack(ac)
            ctx.offset += 1
            ctx.col += 4 - ((ctx.col - 1) & 3)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def copy_(): ExpectingInstr = new Newline()
}

// This instruction has GREAT potential, it should be integrated into peephole :)
// We should also make equivalents for Satisfy and String
// Also experiment: is CharTok; Exchange better than CharTokFastPerform with const?
private [parsley] class CharTokFastPerform(protected final val c: Char, protected final val f: Any => Any) extends ExpectingInstr(Some("\"" + c.toString + "\""))
{
    protected final val fc: Any = f(c)
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == c)
        {
                ctx.pushStack(fc)
                ctx.offset += 1
                ctx.col += 1
                ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def copy_(): ExpectingInstr = new CharTokFastPerform(c, f)
    override final def toString: String = s"ChrPerform($c, ?)"
}

private [parsley] final class NewlineFastPerform(private [this] val g: Any => Any) extends CharTokFastPerform('\n', g)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\n')
        {
            ctx.pushStack(fc)
            ctx.offset += 1
            ctx.col = 1
            ctx.line += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def copy_(): ExpectingInstr = new NewlineFastPerform(f)
}

private [parsley] final class TabFastPerform(private [this] val g: Any => Any) extends CharTokFastPerform('\t', g)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\t')
        {
            ctx.pushStack(fc)
            ctx.offset += 1
            ctx.col += 4 - ((ctx.col - 1) & 3)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def copy_(): ExpectingInstr = new TabFastPerform(f)
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
    import scala.annotation.switch
    def apply(c: Char, f: Any => Any): CharTokFastPerform = (c: @switch) match
    {
        case '\n' => new NewlineFastPerform(f)
        case '\t' => new TabFastPerform(f)
        case _ => new CharTokFastPerform(c, f)
    }
    def unapply(self: CharTokFastPerform): Option[(Char, Any=>Any)] = Some((self.c, self.f))
}