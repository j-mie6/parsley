package parsley
import scala.annotation.{switch, tailrec}

private [parsley] final class Perform[-A, +B](f: A => B) extends Instr
{
    private [Perform] val g = f.asInstanceOf[Any => B]
    override def apply(ctx: Context): Unit =
    {
        ctx.exchangeStack(g(ctx.stack.head))
        ctx.inc()
    }
    override def toString: String = "Perform(?)"
    override def copy: Perform[A, B] = new Perform(f)
}

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

private [parsley] final class FastFail[A](private [FastFail] val msggen: A=>String, expected: UnsafeOption[String]) extends ExpectingInstr(expected)
{
    private[this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit =
    {
        val msg = msggen_(ctx.popStack())
        new Fail(msg, expected)(ctx)
    }
    override def toString: String = "FastFail(?)"
    override def copy_ : ExpectingInstr = new FastFail(msggen, expected)
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

private [parsley] class CharTokFastPerform protected (protected final val c: Char, protected final val f: Char => Any) extends ExpectingInstr("\"" + c.toString + "\"")
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

private [parsley] final class StringTokFastPerform protected (private [this] val s: String, private [this] val f: String => Any) extends ExpectingInstr("\"" + s + "\"")
{
    private [this] val cs = s.toCharArray
    private [this] val sz = cs.length
    private [this] val fs: Any = f(s)
    private [this] val (colAdjust, lineAdjust) =
    {
        @tailrec def compute(cs: Array[Char], i: Int = 0, col: Int = 0, line: Int = 0)(implicit tabprefix: Option[Int] = None): (Int, Int, Option[Int]) =
        {
            if (i < cs.length) (cs(i): @switch) match
            {
                case '\n' => compute(cs, i+1, 1, line + 1)(Some(0))
                case '\t' if tabprefix.isEmpty => compute(cs, i+1, 0, line)(Some(col))
                case '\t' => compute(cs, i+1, col + 4 - ((col-1) & 3), line)
                case _ => compute(cs, i+1, col + 1, line)
            }
            else (col, line, tabprefix)
        }
        val (col, line, tabprefix) = compute(cs)
        if (line > 0) ((_: Int) => col, (x: Int) => x + line)
        else (tabprefix match
        {
            case Some(prefix) => 
                val outer = 4 + col + prefix
                val inner = prefix - 1
                (x: Int) => outer + x - ((x + inner) & 3)
            case None => (x: Int) => x + col
        }, (x: Int) => x)
    }
    override def apply(ctx: Context): Unit =
    {
        val strsz = this.sz
        val inputsz = ctx.inputsz
        val input = ctx.input
        var i = ctx.offset
        var j = 0
        val cs = this.cs
        if (inputsz != i)
        { 
            while (j < strsz)
            {
                val c = cs(j)
                if (i == inputsz || input(i) != c)
                {
                    ctx.offset = i
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            ctx.pushStack(fs)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"StrPerform($s, ?)"
    override def copy_ : ExpectingInstr = new StringTokFastPerform(s, f)
}

private [parsley] object CharTokFastPerform
{
    def apply[A >: Char, B](c: Char, f: A => B, expected: UnsafeOption[String]): CharTokFastPerform = (c: @switch) match
    {
        case '\n' => 
            val ct = new NewlineFastPerform(f)
            if (expected != null) ct.expected = expected
            ct
        case '\t' => 
            val ct = new TabFastPerform(f)
            if (expected != null) ct.expected = expected
            ct
        case _ => 
            val ct = new CharTokFastPerform(c, f)
            if (expected != null) ct.expected = expected
            ct
    }
}

private [parsley] object StringTokFastPerform
{
    def apply[A >: String, B](s: String, f: A => B, expected: UnsafeOption[String] = null): StringTokFastPerform =
    {
        val st = new StringTokFastPerform(s, f)
        if (expected != null) st.expected = expected
        st
    }
}