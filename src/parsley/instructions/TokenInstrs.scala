package parsley.instructions

import scala.annotation.switch

// This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [parsley] class TokenWhiteSpace(ws: Set[Char], start: String, end: String, line: String, nested: Boolean) extends Instr
{
    private val noLine = line.isEmpty
    private val noMulti = start.isEmpty
    override final def apply(ctx: Context): Unit =
    {
        if (noLine && noMulti) ctx.inc()
        else if (noLine)
        {
            // No whitespace to read, parser technically fails
            if (!spaces(ctx) || !ctx.input.startsWith(start, ctx.offset)) ctx.fail()
            else
            {
                while (ctx.moreInput && ctx.input.startsWith(start, ctx.offset))
                {
                    if (!multiLineComment(ctx)) return
                    spaces(ctx)
                }
                ctx.inc()
            }
        }
        else if (noMulti)
        {
            // No whitespace to read, parser technically fails
            if (!spaces(ctx) || !ctx.input.startsWith(line, ctx.offset)) ctx.fail()
            else
            {
                while (ctx.moreInput && ctx.input.startsWith(line, ctx.offset))
                {
                    singleLineComment(ctx)
                    spaces(ctx)
                }
                ctx.inc()
            }
        }
        else
        {
            val foundSpaces = spaces(ctx)
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
            // No whitespace to read, parser technically fails
            if (!foundSpaces || (!startsSingle && !startsMulti)) ctx.fail()
            else
            {
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
    }

    private final def singleLineComment(ctx: Context): Unit =
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

    private final def multiLineComment(ctx: Context): Boolean =
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

    protected def spaces(ctx: Context): Boolean =
    {
        var found = false
        while (ctx.moreInput && ws.contains(ctx.nextChar))
        {
            (ctx.nextChar: @switch) match
            {
                case '\n' => ctx.line += 1; ctx.col = 1
                case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                case _ => ctx.col += 1
            }
            ctx.offset += 1
            found = true
        }
        found
    }

    override def toString: String = "TokenWhiteSpace"
}

private [parsley] final class TokenSkipComments(start: String, end: String, line: String, nested: Boolean) extends TokenWhiteSpace(null, start, end, line, nested)
{
    override protected def spaces(ctx: Context): Boolean = true
    override def toString: String = "TokenSkipComments"
}
