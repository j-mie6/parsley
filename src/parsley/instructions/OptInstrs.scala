package parsley.instructions

import parsley.UnsafeOption

import scala.annotation.{switch, tailrec}

private [parsley] final class Perform[-A, +B](f: A => B) extends Instr
{
    private [Perform] val g = f.asInstanceOf[Any => B]
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.exchange(g(ctx.stack.upeek))
        ctx.inc()
    }
    override def toString: String = "Perform(?)"
}

private [parsley] final class Exchange[A](x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.exchange(x)
        ctx.inc()
    }
    override def toString: String = s"Ex($x)"
}

private [parsley] class Newline(_expected: UnsafeOption[String]) extends CharTok('\n', _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\n')
        {
            ctx.stack.push(ac)
            ctx.offset += 1
            ctx.col = 1
            ctx.line += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] class Tab(_expected: UnsafeOption[String]) extends CharTok('\t', _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\t')
        {
            ctx.stack.push(ac)
            ctx.offset += 1
            ctx.col += 4 - ((ctx.col - 1) & 3)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] class CharTokFastPerform protected (protected final val c: Char, protected final val f: Char => Any, _expected: UnsafeOption[String]) extends Instr
{
    protected val expected: String = if (_expected == null) "\"" + c.toString + "\"" else _expected
    protected final val fc: Any = f(c)
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == c)
        {
                ctx.stack.push(fc)
                ctx.offset += 1
                ctx.col += 1
                ctx.inc()
        }
        else ctx.fail(expected)
    }
    override final def toString: String = s"ChrPerform($c, ?)"
}

private [parsley] final class NewlineFastPerform(g: Char => Any, _expected: UnsafeOption[String]) extends CharTokFastPerform('\n', g, _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\n')
        {
            ctx.stack.push(fc)
            ctx.offset += 1
            ctx.col = 1
            ctx.line += 1
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] final class TabFastPerform(g: Char => Any, _expected: UnsafeOption[String]) extends CharTokFastPerform('\t', g, _expected)
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) == '\t')
        {
            ctx.stack.push(fc)
            ctx.offset += 1
            ctx.col += 4 - ((ctx.col - 1) & 3)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
}

private [parsley] final class StringTokFastPerform(s: String, f: String => Any, _expected: UnsafeOption[String]) extends Instr
{
    protected val expected: String = if (_expected == null) "\"" + s + "\"" else _expected
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
            ctx.stack.push(fs)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"StrPerform($s, ?)"
}

private [parsley] object CharTokFastPerform
{
    def apply[A >: Char, B](c: Char, f: A => B, expected: UnsafeOption[String]): CharTokFastPerform = (c: @switch) match
    {
        case '\n' => new NewlineFastPerform(f, expected)
        case '\t' => new TabFastPerform(f, expected)
        case _ => new CharTokFastPerform(c, f, expected)
    }
}