package parsley.instructions

import scala.annotation.switch

// This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [parsley] class TokenSkipComments(start: String, end: String, line: String, nested: Boolean) extends Instr
{
    protected final val noLine = line.isEmpty
    protected final val noMulti = start.isEmpty
    override def apply(ctx: Context): Unit =
    {
        if (noLine && noMulti) ctx.inc()
        else if (noLine)
        {
            // No comments to read, parser technically fails
            if (!ctx.input.startsWith(start, ctx.offset)) ctx.fail()
            else
            {
                do if (!multiLineComment(ctx)) return
                while (ctx.moreInput && ctx.input.startsWith(start, ctx.offset))
                ctx.inc()
            }
        }
        else if (noMulti)
        {
            // No comments to read, parser technically fails
            if (!ctx.input.startsWith(line, ctx.offset)) ctx.fail()
            else
            {
                do singleLineComment(ctx)
                while (ctx.moreInput && ctx.input.startsWith(line, ctx.offset))
                ctx.inc()
            }
        }
        else
        {
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
            // No comments to read, parser technically fails
            if (!startsSingle && !startsMulti) ctx.fail()
            else
            {
                while (ctx.moreInput && (startsSingle || startsMulti))
                {
                    if (startsMulti)
                    {
                        if (!multiLineComment(ctx)) return
                    }
                    else singleLineComment(ctx)
                    startsSingle = ctx.input.startsWith(line, ctx.offset)
                    startsMulti = ctx.input.startsWith(start, ctx.offset)
                }
                ctx.inc()
            }
        }
    }

    protected final def singleLineComment(ctx: Context): Unit =
    {
        ctx.offset += line.length
        ctx.col += line.length
        while (ctx.moreInput && ctx.nextChar != '\n')
        {
            (ctx.nextChar: @switch) match
            {
                case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                case _ => ctx.col += 1
            }
            ctx.offset += 1
        }
        if (ctx.moreInput)
        {
            ctx.col = 1
            ctx.line += 1
            ctx.offset += 1
        }
    }

    protected final def multiLineComment(ctx: Context): Boolean =
    {
        ctx.offset += start.length
        ctx.col += start.length
        var n = 1
        while (n != 0)
        {
            if (ctx.input.startsWith(end, ctx.offset))
            {
                ctx.offset += end.length
                ctx.col += end.length
                n -= 1
            }
            else if (nested && ctx.input.startsWith(start, ctx.offset))
            {
                ctx.offset += start.length
                ctx.col += start.length
                n += 1
            }
            else if (ctx.moreInput)
            {
                (ctx.nextChar: @switch) match
                {
                    case '\n' => ctx.line += 1; ctx.col = 1
                    case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                    case _ => ctx.col += 1
                }
                ctx.offset += 1
            }
            else
            {
                ctx.fail("end of comment")
                return false
            }
        }
        true
    }
    override def toString: String = "TokenSkipComments"
}

private [parsley] final class TokenWhiteSpace(ws: Set[Char], start: String, end: String, line: String, nested: Boolean) extends TokenSkipComments(start, end, line, nested)
{
    override def apply(ctx: Context): Unit =
    {
        if (noLine && noMulti) ctx.inc()
        else if (noLine)
        {
            spaces(ctx)
            while (ctx.moreInput && ctx.input.startsWith(start, ctx.offset))
            {
                if (!multiLineComment(ctx)) return
                spaces(ctx)
            }
            ctx.inc()
        }
        else if (noMulti)
        {
            spaces(ctx)
            while (ctx.moreInput && ctx.input.startsWith(line, ctx.offset))
            {
                singleLineComment(ctx)
                spaces(ctx)
            }
            ctx.inc()
        }
        else
        {
            spaces(ctx)
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
            // No whitespace to read, parser technically fails
            while (ctx.moreInput && (startsSingle || startsMulti))
            {
                if (startsMulti)
                {
                    if (!multiLineComment(ctx)) return
                }
                else singleLineComment(ctx)
                spaces(ctx)
                startsSingle = ctx.input.startsWith(line, ctx.offset)
                startsMulti = ctx.input.startsWith(start, ctx.offset)
            }
            ctx.inc()
        }
    }

    private def spaces(ctx: Context): Unit =
    {
        while (ctx.moreInput && ws.contains(ctx.nextChar))
        {
            (ctx.nextChar: @switch) match
            {
                case '\n' => ctx.line += 1; ctx.col = 1
                case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                case _ => ctx.col += 1
            }
            ctx.offset += 1
        }
    }

    override def toString: String = "TokenWhiteSpace"
}
