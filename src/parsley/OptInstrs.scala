package parsley
import scala.annotation.switch

private [parsley] final class Exchange[A](private [Exchange] val x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.exchangeStack(x)
        ctx.inc()
    }
    override def toString: String = s"Ex($x)"
    override def copy: Exchange[A] = new Exchange(x)
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
    override def copy: FastFail[A] = new FastFail(msggen)
}

private [parsley] class Newline extends CharTok('\n')
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
    override def copy_ : ExpectingInstr = new Newline
}

private [parsley] class Tab extends CharTok('\t')
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
    override def copy_ : ExpectingInstr = new Tab
}

// This instruction has GREAT potential, it should be integrated into peephole :)
// We should also make equivalents for Satisfy and String
private [parsley] class CharTokFastPerform(protected final val c: Char, protected final val f: Char => Any) extends ExpectingInstr("\"" + c.toString + "\"")
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
    override final def toString: String = s"ChrPerform($c, ?)"
    override def copy_ : ExpectingInstr = new CharTokFastPerform(c, f)
}

private [parsley] final class NewlineFastPerform(private [this] val g: Char => Any) extends CharTokFastPerform('\n', g)
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
    override def copy_ : ExpectingInstr = new NewlineFastPerform(f)
}

private [parsley] final class TabFastPerform(private [this] val g: Char => Any) extends CharTokFastPerform('\t', g)
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
    override def copy_ : ExpectingInstr = new TabFastPerform(f)
}

// Extractor Objects
private [parsley] object Exchange
{
    @deprecated("Will be removed upon branch merge", "")
    def unapply[A](self: Exchange[A]): Option[A] = Some(self.x)
}

private [parsley] object FastFail
{
    @deprecated("Will be removed upon branch merge", "")
    def unapply[A](self: FastFail[A]): Option[A => String] = Some(self.msggen)
}

private [parsley] object CharTokFastPerform
{
    def apply[A >: Char, B](c: Char, f: A => B): CharTokFastPerform = (c: @switch) match
    {
        case '\n' => new NewlineFastPerform(f)
        case '\t' => new TabFastPerform(f)
        case _ => new CharTokFastPerform(c, f)
    }
    @deprecated("Will be removed upon branch merge", "")
    def unapply(self: CharTokFastPerform): Option[(Char, Char=>Any)] = Some((self.c, self.f))
}