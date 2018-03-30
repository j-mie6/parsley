package parsley.instructions

import scala.annotation.switch

// This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [parsley] final class TokenSkipComments(start: String, end: String, line: String, nested: Boolean) extends Instr
{
    private val noLine = line.isEmpty
    private val noMulti = start.isEmpty
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
                while (ctx.offset < ctx.inputsz && ctx.input.startsWith(start, ctx.offset))
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
                while (ctx.offset < ctx.inputsz && ctx.input.startsWith(line, ctx.offset))
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
                while (ctx.offset < ctx.inputsz && (startsSingle || startsMulti))
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

    private def singleLineComment(ctx: Context): Unit =
    {
        ctx.offset += line.length
        ctx.col += line.length
        while (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) != '\n')
        {
            (ctx.input(ctx.offset): @switch) match
            {
                case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                case _ => ctx.col += 1
            }
            ctx.offset += 1
        }
        if (ctx.offset < ctx.inputsz)
        {
            ctx.col = 1
            ctx.line += 1
            ctx.offset += 1
        }
    }

    private def multiLineComment(ctx: Context): Boolean =
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
            else if (ctx.offset < ctx.inputsz)
            {
                (ctx.input(ctx.offset): @switch) match
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

// Could merge this with the intrinsic above, just override spaces in this one to perform actual behaviour, or other way round
private [parsley] final class TokenWhiteSpace(ws: Set[Char], start: String, end: String, line: String, nested: Boolean) extends Instr
{
    private val noLine = line.isEmpty
    private val noMulti = start.isEmpty
    override def apply(ctx: Context): Unit =
    {
        if (noLine && noMulti) ctx.inc()
        else if (noLine)
        {
            // No whitespace to read, parser technically fails
            if (!spaces(ctx) || !ctx.input.startsWith(start, ctx.offset)) ctx.fail()
            else
            {
                while (ctx.offset < ctx.inputsz && ctx.input.startsWith(start, ctx.offset))
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
                while (ctx.offset < ctx.inputsz && ctx.input.startsWith(line, ctx.offset))
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
            if (!foundSpaces && !startsSingle && !startsMulti) ctx.fail()
            else
            {
                while (ctx.offset < ctx.inputsz && (startsSingle || startsMulti))
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

    private def singleLineComment(ctx: Context): Unit =
    {
        ctx.offset += line.length
        ctx.col += line.length
        while (ctx.offset < ctx.inputsz && ctx.input(ctx.offset) != '\n')
        {
            (ctx.input(ctx.offset): @switch) match
            {
                case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                case _ => ctx.col += 1
            }
            ctx.offset += 1
        }
        if (ctx.offset < ctx.inputsz)
        {
            ctx.col = 1
            ctx.line += 1
            ctx.offset += 1
        }
    }

    private def multiLineComment(ctx: Context): Boolean =
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
            else if (ctx.offset < ctx.inputsz)
            {
                (ctx.input(ctx.offset): @switch) match
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

    private def spaces(ctx: Context): Boolean =
    {
        var found = false
        while (ctx.offset < ctx.inputsz && ws.contains(ctx.input(ctx.offset)))
        {
            (ctx.input(ctx.offset): @switch) match
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
