package parsley.instructions

import parsley.DeepToken.Sign._
import parsley.TokenParser.TokenSet
import parsley.UnsafeOption

import scala.annotation.{switch, tailrec}

// TODO This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [parsley] class TokenSkipComments(start: String, end: String, line: String, nested: Boolean) extends Instr with NoPush
{
    protected final val noLine = line.isEmpty
    protected final val noMulti = start.isEmpty
    override def apply(ctx: Context): Unit =
    {
        if (noLine && noMulti) ctx.inc()
        else if (noLine)
        {
            while (ctx.moreInput && ctx.input.startsWith(start, ctx.offset)) if (!multiLineComment(ctx)) return
            ctx.inc()
        }
        else if (noMulti)
        {
            while (ctx.moreInput && ctx.input.startsWith(line, ctx.offset)) singleLineComment(ctx)
            ctx.inc()
        }
        else
        {
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
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

// TODO This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [parsley] final class TokenComment(start: String, end: String, line: String, nested: Boolean) extends Instr
{
    protected final val noLine = line.isEmpty
    protected final val noMulti = start.isEmpty
    override def apply(ctx: Context): Unit =
    {
        if (!ctx.moreInput) ctx.fail("comment")
        else if (noLine && noMulti) ctx.fail("comment")
        else if (noLine)
        {
            if (!ctx.input.startsWith(start, ctx.offset)) ctx.fail("comment")
            else
            {
                if (!multiLineComment(ctx)) return
                ctx.stack.push(())
                ctx.inc()
            }
        }
        else if (noMulti)
        {
            if (!ctx.input.startsWith(line, ctx.offset)) ctx.fail("comment")
            else
            {
                singleLineComment(ctx)
                ctx.stack.push(())
                ctx.inc()
            }
        }
        else
        {
            val startsSingle = ctx.input.startsWith(line, ctx.offset)
            val startsMulti = ctx.input.startsWith(start, ctx.offset)
            if (!startsSingle && !startsMulti) ctx.fail("comment")
            else
            {
                if (startsMulti)
                {
                    if (!multiLineComment(ctx)) return
                }
                else singleLineComment(ctx)
                ctx.stack.push(())
                ctx.inc()
            }
        }
    }

    private def singleLineComment(ctx: Context): Unit =
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
    override def toString: String = "TokenComment"
}

private [parsley] final class TokenWhiteSpace(ws: TokenSet, start: String, end: String, line: String, nested: Boolean) extends TokenSkipComments(start, end, line, nested) with NoPush
{
    override def apply(ctx: Context): Unit =
    {
        if (noLine && noMulti)
        {
            spaces(ctx)
            ctx.inc()
        }
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
            // TODO This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
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
        while (ctx.moreInput && ws(ctx.nextChar))
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

private [parsley] final class TokenSign(ty: SignType, _expected: UnsafeOption[String]) extends Instr
{
    val expected = if (_expected == null) "sign" else _expected
    val neg: Any => Any = ty match
    {
        case IntType => ((x: Int) => -x).asInstanceOf[Any => Any]
        case DoubleType => ((x: Double) => -x).asInstanceOf[Any => Any]
    }
    val pos: Any => Any = x => x

    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput)
        {
            if (ctx.nextChar == '-')
            {
                ctx.offset += 1
                ctx.col += 1
                ctx.stack.push(neg)
            }
            else if (ctx.nextChar == '+')
            {
                ctx.offset += 1
                ctx.col += 1
                ctx.stack.push(pos)
            }
            else ctx.stack.push(pos)
        }
        ctx.inc()
    }

    override def toString: String = "TokenSign"
}

private [parsley] final class TokenNatural(_expected: UnsafeOption[String]) extends Instr
{
    val expected = if (_expected == null) "natural" else _expected
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput) (ctx.nextChar: @switch) match
        {
            case '0' =>
                ctx.offset += 1
                ctx.col += 1
                if (!ctx.moreInput)
                {
                    ctx.stack.push(0)
                    ctx.inc()
                }
                else
                {
                    (ctx.nextChar: @switch) match
                    {
                        case 'x' | 'X' =>
                            ctx.offset += 1
                            ctx.col += 1
                            if (ctx.moreInput)
                            {
                                (ctx.nextChar: @switch) match
                                {
                                    case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                                            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                                            | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                                        ctx.offset += 1
                                        ctx.col += 1
                                        ctx.stack.push(hexadecimal(ctx, d.asDigit))
                                        ctx.inc()
                                    case _ => ctx.fail(expected)
                                }
                            }
                            else ctx.fail(expected)
                        case 'o' | 'O' =>
                            ctx.offset += 1
                            ctx.col += 1
                            if (ctx.moreInput)
                            {
                                val d = ctx.nextChar
                                if (d >= '0' && d <= '7')
                                {
                                    ctx.offset += 1
                                    ctx.col += 1
                                    ctx.stack.push(octal(ctx, d.asDigit))
                                    ctx.inc()
                                }
                                else ctx.fail(expected)
                            }
                            else ctx.fail(expected)
                        case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                            ctx.offset += 1
                            ctx.col += 1
                            ctx.stack.push(decimal(ctx, d.asDigit))
                            ctx.inc()
                        case _ =>
                            ctx.stack.push(0)
                            ctx.inc()
                    }
                }
            case d@('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                ctx.offset += 1
                ctx.col += 1
                ctx.stack.push(decimal(ctx, d.asDigit))
                ctx.inc()
            case _ => ctx.fail(expected)
        }
        else ctx.fail(expected)
    }

    @tailrec private def decimal(ctx: Context, x: Int = 0): Int =
    {
        if (ctx.moreInput)
        {
            val d = ctx.nextChar
            if (d >= '0' && d <= '9')
            {
                ctx.offset += 1
                ctx.col += 1
                decimal(ctx, x * 10 + d.asDigit)
            }
            else x
        }
        else x
    }

    @tailrec private def hexadecimal(ctx: Context, x: Int = 0): Int =
    {
        if (ctx.moreInput)
        {
            (ctx.nextChar: @switch) match
            {
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                    ctx.offset += 1
                    ctx.col += 1
                    hexadecimal(ctx, x * 16 + d.asDigit)
                case _ => x
            }
        }
        else x
    }

    @tailrec private def octal(ctx: Context, x: Int = 0): Int =
    {
        if (ctx.moreInput)
        {
            val d = ctx.nextChar
            if (d >= '0' && d <= '7')
            {
                ctx.offset += 1
                ctx.col += 1
                octal(ctx, x * 8 + d.asDigit)
            }
            else x
        }
        else x
    }

    override def toString: String = "TokenNatural"
}

private [parsley] final class TokenFloat(_expected: UnsafeOption[String]) extends Instr with Stateful
{
    val expected = if (_expected == null) "unsigned float" else _expected
    var failed: Boolean = _
    override def apply(ctx: Context): Unit =
    {
        failed = false
        if (ctx.moreInput) (ctx.nextChar: @switch) match
        {
            case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                ctx.offset += 1
                ctx.col += 1
                val builder = new StringBuilder()
                decimal(ctx, builder += d, false)
                if (ctx.moreInput) (ctx.nextChar: @switch) match
                {
                    case '.' => // fraction
                        ctx.offset += 1
                        ctx.col += 1
                        decimal(ctx, builder += '.')
                        if (!failed)
                        {
                            if (ctx.moreInput && (ctx.nextChar == 'e' || ctx.nextChar == 'E'))
                            {
                                ctx.offset += 1
                                ctx.col += 1
                                exponent(ctx, builder += 'e')
                            }
                            if (!failed) try ctx.stack.push(builder.toString.toDouble)
                            catch { case _: NumberFormatException => failed = true }
                        }
                    case 'e' | 'E' => // exponent
                        ctx.offset += 1
                        ctx.col += 1
                        exponent(ctx, builder += 'e')
                        if (!failed) try ctx.stack.push(builder.toString.toDouble)
                        catch { case _: NumberFormatException => ctx.fail(expected) }
                    case _ => failed = true
                }
                else failed = true
            case _ => failed = true
        }
        else failed = true
        if (failed) ctx.fail(expected)
        else ctx.inc()
    }

    @tailrec private def decimal(ctx: Context, x: StringBuilder, first: Boolean = true): StringBuilder =
    {
        if (ctx.moreInput)
        {
            val d = ctx.nextChar
            if (d >= '0' && d <= '9')
            {
                ctx.offset += 1
                ctx.col += 1
                decimal(ctx, x += d, false)
            }
            else if (first)
            {
                failed = true
                x
            }
            else x
        }
        else if (first)
        {
            failed = true
            x
        }
        else x
    }

    private def exponent(ctx: Context, x: StringBuilder): StringBuilder =
    {
        if (ctx.moreInput)
        {
            (ctx.nextChar: @switch) match
            {
                case '+' =>
                    ctx.offset += 1
                    ctx.col += 1
                    decimal(ctx, x)
                case '-' =>
                    ctx.offset += 1
                    ctx.col += 1
                    decimal(ctx, x += '-')
                case _ => decimal(ctx, x)
            }
        }
        else
        {
            failed = true
            x
        }
    }

    override def toString: String = "TokenFloat"
    override def copy: TokenFloat = new TokenFloat(expected)
}

private [parsley] class TokenEscape(_expected: UnsafeOption[String]) extends Instr with Stateful
{
    private [this] final val expected = if (_expected == null) "escape code" else _expected
    protected var escapeChar: Char = _
    protected var badCode: Boolean = _
    override def apply(ctx: Context): Unit =
    {
        badCode = false
        if (escape(ctx))
        {
            ctx.stack.push(escapeChar)
            ctx.inc()
        }
        else
        {
            ctx.fail(expected)
            if (badCode) ctx.raw ::= "invalid escape sequence"
        }
    }

    protected final def escape(ctx: Context): Boolean =
    {
        if (ctx.moreInput)
        {
            (ctx.nextChar: @switch) match
            {
                case 'a' => ctx.offset += 1; ctx.col += 1; escapeChar = '\u0007'
                case 'b' => ctx.offset += 1; ctx.col += 1; escapeChar = '\b'
                case 'f' => ctx.offset += 1; ctx.col += 1; escapeChar = '\u000c'
                case 'n' => ctx.offset += 1; ctx.col += 1; escapeChar = '\n'
                case 'r' => ctx.offset += 1; ctx.col += 1; escapeChar = '\r'
                case 't' => ctx.offset += 1; ctx.col += 1; escapeChar = '\t'
                case 'v' => ctx.offset += 1; ctx.col += 1; escapeChar = '\u000b'
                case '\\' => ctx.offset += 1; ctx.col += 1; escapeChar = '\\'
                case '\"' => ctx.offset += 1; ctx.col += 1; escapeChar = '\"'
                case '\'' => ctx.offset += 1; ctx.col += 1; escapeChar = '\''
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                    ctx.offset += 1
                    ctx.col += 1
                    val escapeCode = decimal(ctx, d.asDigit)
                    if (escapeCode <= 0x10FFFF) escapeChar = escapeCode.toChar
                    else
                    {
                        badCode = true
                        return false
                    }
                case 'x' =>
                    ctx.offset += 1
                    ctx.col += 1
                    if (ctx.moreInput)
                    {
                        (ctx.nextChar: @switch) match
                        {
                            case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                                    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                                    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                                ctx.offset += 1
                                ctx.col += 1
                                val escapeCode = hexadecimal(ctx, d.asDigit)
                                if (escapeCode <= 0x10FFFF) escapeChar = escapeCode.toChar
                                else
                                {
                                    badCode = true
                                    return false
                                }
                            case _ => return false
                        }
                    }
                    else return false
                case 'o' =>
                    ctx.offset += 1
                    ctx.col += 1
                    if (ctx.moreInput)
                    {
                        val d = ctx.nextChar
                        if (d >= '0' && d <= '7')
                        {
                            ctx.offset += 1
                            ctx.col += 1
                            val escapeCode = octal(ctx, d.asDigit)
                            if (escapeCode <= 0x10FFFF) escapeChar = escapeCode.toChar
                            else
                            {
                                badCode = true
                                return false
                            }
                        }
                        else return false
                    }
                    else return false
                case '^' =>
                    ctx.offset += 1
                    ctx.col += 1
                    if (ctx.moreInput)
                    {
                        val c = ctx.nextChar
                        if (c >= 'A' && c <= 'Z')
                        {
                            ctx.offset += 1
                            ctx.col += 1
                            escapeChar = (c - 'A' + 1).toChar
                        }
                        else return false
                    }
                    else return false
                case 'A' => //ACK
                    if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'C' && ctx.input(ctx.offset + 2) == 'K')
                    {
                        ctx.offset += 3
                        ctx.col += 3
                        escapeChar = '\u0006'
                    }
                    else return false
                case 'B' => //BS BEL
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u0008'
                    }
                    else if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'E' && ctx.input(ctx.offset + 2) == 'L')
                    {
                        ctx.offset += 3
                        ctx.col += 3
                        escapeChar = '\u0007'
                    }
                    else return false
                case 'C' => //CR CAN
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'R')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u000d'
                    }
                    else if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'A' && ctx.input(ctx.offset + 2) == 'N')
                    {
                        ctx.offset += 3
                        ctx.col += 3
                        escapeChar = '\u0018'
                    }
                    else return false
                case 'D' => //DC1 DC2 DC3 DC4 DEL DLE
                    if (ctx.offset + 2 < ctx.inputsz) (ctx.input(ctx.offset + 1): @switch) match
                    {
                        case 'C' => (ctx.input(ctx.offset + 2): @switch) match
                        {
                            case '1' => ctx.offset += 3; ctx.col += 3; escapeChar = '\u0011'
                            case '2' => ctx.offset += 3; ctx.col += 3; escapeChar = '\u0012'
                            case '3' => ctx.offset += 3; ctx.col += 3; escapeChar = '\u0013'
                            case '4' => ctx.offset += 3; ctx.col += 3; escapeChar = '\u0014'
                            case _ => return false
                        }
                        case 'E' =>
                            if (ctx.input(ctx.offset + 2) == 'L') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u001f' }
                            else return false
                        case 'L' =>
                            if (ctx.input(ctx.offset + 2) == 'E') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0010' }
                            else return false
                        case _ => return false
                    }
                    else return false
                case 'E' => //EM ETX ETB ESC EOT ENQ
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'M')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u0019'
                    }
                    else if (ctx.offset + 2 < ctx.inputsz) (ctx.input(ctx.offset + 1): @switch) match
                    {
                        case 'N' =>
                            if (ctx.input(ctx.offset + 2) == 'Q') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0005' }
                            else return false
                        case 'O' =>
                            if (ctx.input(ctx.offset + 2) == 'T') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0004' }
                            else return false
                        case 'S' =>
                            if (ctx.input(ctx.offset + 2) == 'C') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u001b' }
                            else return false
                        case 'T' =>
                            if (ctx.input(ctx.offset + 2) == 'X') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0003' }
                            else if (ctx.input(ctx.offset + 2) == 'B') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0017' }
                            else return false
                        case _ => return false
                    }
                    else return false
                case 'F' => //FF FS
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'F')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u000c'
                    }
                    else if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u001c'
                    }
                    else return false
                case 'G' => //GS
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u001d'
                    }
                    else return false
                case 'H' => //HT
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'T')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u0009'
                    }
                    else return false
                case 'L' => //LF
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'F')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\n'
                    }
                    else return false
                case 'N' => //NUL NAK
                    if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'U' && ctx.input(ctx.offset + 2) == 'L')
                    {
                        ctx.offset += 3
                        ctx.col += 3
                        escapeChar = '\u0000'
                    }
                    else if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'A' && ctx.input(ctx.offset + 2) == 'K')
                    {
                        ctx.offset += 3
                        ctx.col += 3
                        escapeChar = '\u0015'
                    }
                    else return false
                case 'R' => //RS
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u001e'
                    }
                    else return false
                case 'S' => //SO SI SP SOH STX SYN SUB
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'O')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u000e'
                    }
                    else if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'I')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u000f'
                    }
                    else if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'P')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u0020'
                    }
                    else if (ctx.offset + 2 < ctx.inputsz) (ctx.input(ctx.offset + 1): @switch) match
                    {
                        case 'O' =>
                            if (ctx.input(ctx.offset + 2) == 'H') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0001' }
                            else return false
                        case 'T' =>
                            if (ctx.input(ctx.offset + 2) == 'X') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0002' }
                            else return false
                        case 'Y' =>
                            if (ctx.input(ctx.offset + 2) == 'N') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u0016' }
                            else return false
                        case 'U' =>
                            if (ctx.input(ctx.offset + 2) == 'B') { ctx.offset += 3; ctx.col += 3; escapeChar = '\u001a' }
                            else return false
                        case _ => return false
                    }
                    else return false
                case 'U' => //US
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u001f'
                    }
                    else return false
                case 'V' => //VT
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'T')
                    {
                        ctx.offset += 2
                        ctx.col += 2
                        escapeChar = '\u000b'
                    }
                    else return false
                case _ => return false
            }
            true
        }
        else false
    }

    @tailrec private final def decimal(ctx: Context, x: Int = 0): Int =
    {
        if (ctx.moreInput)
        {
            val d = ctx.nextChar
            if (d >= '0' && d <= '9')
            {
                ctx.offset += 1
                ctx.col += 1
                decimal(ctx, x * 10 + d.asDigit)
            }
            else x
        }
        else x
    }

    @tailrec private final def hexadecimal(ctx: Context, x: Int = 0): Int =
    {
        if (ctx.moreInput)
        {
            (ctx.nextChar: @switch) match
            {
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                    ctx.offset += 1
                    ctx.col += 1
                    hexadecimal(ctx, x * 16 + d.asDigit)
                case _ => x
            }
        }
        else x
    }

    @tailrec private final def octal(ctx: Context, x: Int = 0): Int =
    {
        if (ctx.moreInput)
        {
            val d = ctx.nextChar
            if (d >= '0' && d <= '7')
            {
                ctx.offset += 1
                ctx.col += 1
                octal(ctx, x * 8 + d.asDigit)
            }
            else x
        }
        else x
    }

    override def toString: String = "TokenEscape"
    override def copy: TokenEscape = new TokenEscape(expected)
}

private [parsley] final class TokenString(ws: TokenSet, _expected: UnsafeOption[String]) extends TokenEscape(_expected)
{
    val expectedString = if (_expected == null) "string" else _expected
    val expectedEos = if (_expected == null) "end of string" else _expected
    val expectedEscape = if (_expected == null) "escape code" else _expected
    val expectedGap = if (_expected == null) "end of string gap" else _expected
    val expectedChar = if (_expected == null) "string character" else _expected

    override def apply(ctx: Context): Unit =
    {
        badCode = false
        if (ctx.moreInput && ctx.nextChar == '"')
        {
            ctx.offset += 1
            ctx.col += 1
            restOfString(ctx, new StringBuilder())
        }
        else ctx.fail(expectedString)
    }

    @tailrec def restOfString(ctx: Context, builder: StringBuilder): Unit =
    {
        if (ctx.moreInput) (ctx.nextChar: @switch) match
        {
            case '"' =>
                ctx.offset += 1
                ctx.col += 1
                ctx.stack.push(builder.toString)
                ctx.inc()
            case '\\' =>
                ctx.offset += 1
                ctx.col += 1
                if (spaces(ctx) != 0)
                {
                    if (ctx.moreInput && ctx.nextChar == '\\')
                    {
                        ctx.offset += 1
                        ctx.col += 1
                        restOfString(ctx, builder)
                    }
                    else ctx.fail(expectedGap)
                }
                else if (ctx.moreInput && ctx.nextChar == '&')
                {
                    ctx.offset += 1
                    ctx.col += 1
                    restOfString(ctx, builder)
                }
                else if (escape(ctx)) restOfString(ctx, builder += escapeChar)
                else
                {
                    ctx.fail(expectedEscape)
                    if (badCode) ctx.raw ::= "invalid escape sequence"
                }
            case c =>
                if (c > '\u0016')
                {
                    ctx.offset += 1
                    ctx.col += 1
                    restOfString(ctx, builder += c)
                }
                else ctx.fail(expectedChar)
        }
        else ctx.fail(expectedEos)
    }

    private def spaces(ctx: Context): Int =
    {
        var n = 0
        while (ctx.moreInput && ws(ctx.nextChar))
        {
            (ctx.nextChar: @switch) match
            {
                case '\n' => ctx.line += 1; ctx.col = 1
                case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                case _ => ctx.col += 1
            }
            ctx.offset += 1
            n += 1
        }
        n
    }

    override def toString: String = "TokenString"
    override def copy: TokenString = new TokenString(ws, _expected)
}

private [parsley] final class TokenRawString(_expected: UnsafeOption[String]) extends Instr
{
    val expectedString = if (_expected == null) "string" else _expected
    val expectedEos = if (_expected == null) "end of string" else _expected
    val expectedChar = if (_expected == null) "string character" else _expected

    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == '"')
        {
            ctx.offset += 1
            ctx.col += 1
            restOfString(ctx, new StringBuilder())
        }
        else ctx.fail(expectedString)
    }

    @tailrec def restOfString(ctx: Context, builder: StringBuilder): Unit =
    {
        if (ctx.moreInput) (ctx.nextChar: @switch) match
        {
            case '"' =>
                ctx.offset += 1
                ctx.col += 1
                ctx.stack.push(builder.toString)
                ctx.inc()
            case '\\' =>
                ctx.offset += 1
                ctx.col += 1
                builder += '\\'
                if (ctx.moreInput && ctx.nextChar > '\u0016')
                {
                    builder += ctx.nextChar
                    ctx.offset += 1
                    ctx.col += 1
                    restOfString(ctx, builder)
                }
                else ctx.fail(expectedChar)
            case c =>
                if (c > '\u0016')
                {
                    ctx.offset += 1
                    ctx.col += 1
                    restOfString(ctx, builder += c)
                }
                else ctx.fail(expectedChar)
        }
        else ctx.fail(expectedEos)
    }

    override def toString: String = "TokenRawString"
}

private [parsley] final class TokenIdentifier(start: TokenSet, letter: TokenSet, keywords: Set[String], _unexpected: UnsafeOption[String]) extends Instr
{
    val expected = if (_unexpected == null) "identifier" else _unexpected

    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && start(ctx.nextChar))
        {
            val name = new StringBuilder()
            name += ctx.nextChar
            ctx.offset += 1
            restOfIdentifier(ctx, name)
        }
        else ctx.fail(expected)
    }

    @tailrec def restOfIdentifier(ctx: Context, name: StringBuilder): Unit =
    {
        if (ctx.moreInput && letter(ctx.nextChar))
        {
            name += ctx.nextChar
            ctx.offset += 1
            restOfIdentifier(ctx, name)
        }
        else
        {
            val nameStr = name.toString
            if (keywords.contains(nameStr))
            {
                ctx.offset -= nameStr.length
                ctx.fail(expected)
                ctx.unexpected = "keyword " + nameStr
                ctx.unexpectAnyway = true
            }
            else
            {
                ctx.col += nameStr.length
                ctx.stack.push(nameStr)
                ctx.inc()
            }
        }
    }

    override def toString: String = "TokenIdentifier"
}

private [parsley] final class TokenUserOperator(start: TokenSet, letter: TokenSet, reservedOps: Set[String], _unexpected: UnsafeOption[String]) extends Instr
{
    val expected = if (_unexpected == null) "operator" else _unexpected

    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && start(ctx.nextChar))
        {
            val name = new StringBuilder()
            name += ctx.nextChar
            ctx.offset += 1
            restOfIdentifier(ctx, name)
        }
        else ctx.fail(expected)
    }

    @tailrec def restOfIdentifier(ctx: Context, name: StringBuilder): Unit =
    {
        if (ctx.moreInput && letter(ctx.nextChar))
        {
            name += ctx.nextChar
            ctx.offset += 1
            restOfIdentifier(ctx, name)
        }
        else
        {
            val nameStr = name.toString
            if (reservedOps.contains(nameStr))
            {
                ctx.offset -= nameStr.length
                ctx.fail(expected)
                ctx.unexpected = "reserved operator " + nameStr
                ctx.unexpectAnyway = true
            }
            else
            {
                ctx.col += nameStr.length
                ctx.stack.push(nameStr)
                ctx.inc()
            }
        }
    }

    override def toString: String = "TokenUserOperator"
}

private [parsley] final class TokenOperator(start: TokenSet, letter: TokenSet, reservedOps: Set[String], _unexpected: UnsafeOption[String]) extends Instr
{
    val expected = if (_unexpected == null) "operator" else _unexpected

    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && start(ctx.nextChar))
        {
            val name = new StringBuilder()
            name += ctx.nextChar
            ctx.offset += 1
            restOfIdentifier(ctx, name)
        }
        else ctx.fail(expected)
    }

    @tailrec def restOfIdentifier(ctx: Context, name: StringBuilder): Unit =
    {
        if (ctx.moreInput && letter(ctx.nextChar))
        {
            name += ctx.nextChar
            ctx.offset += 1
            restOfIdentifier(ctx, name)
        }
        else
        {
            val nameStr = name.toString
            if (!reservedOps.contains(nameStr))
            {
                ctx.offset -= nameStr.length
                ctx.fail(expected)
                ctx.unexpected = "non-reserved operator " + nameStr
                ctx.unexpectAnyway = true
            }
            else
            {
                ctx.col += nameStr.length
                ctx.stack.push(nameStr)
                ctx.inc()
            }
        }
    }

    override def toString: String = "TokenReservedOperator"
}

private [parsley] class TokenKeyword(_keyword: String, letter: TokenSet, caseSensitive: Boolean, _expected: UnsafeOption[String]) extends Instr with NoPush
{
    val expected = if (_expected == null) _keyword else _expected
    val expectedEnd = if (_expected == null) "end of " + _keyword else _expected
    val keyword = (if (caseSensitive) _keyword else _keyword.toLowerCase).toCharArray

    override def apply(ctx: Context): Unit =
    {
        val strsz = this.keyword.length
        val inputsz = ctx.inputsz
        val input = ctx.input
        var i = ctx.offset
        var j = 0
        val keyword = this.keyword
        if (inputsz >= i + strsz)
        {
            while (j < strsz)
            {
                val c = if (caseSensitive) input(i) else input(i).toLower
                if (c != keyword(j))
                {
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
            ctx.col += strsz
            ctx.offset = i
            if (i < inputsz && letter(input(i))) ctx.fail(expectedEnd)
            else ctx.inc()
        }
        else ctx.fail(expected)
    }

    override def toString: String = s"TokenKeyword(${_keyword})"
}

private [parsley] class TokenOperator_(_operator: String, letter: TokenSet, _expected: UnsafeOption[String]) extends Instr with NoPush
{
    val expected = if (_expected == null) _operator else _expected
    val expectedEnd = if (_expected == null) "end of " + _operator else _expected
    val operator = _operator.toCharArray

    override def apply(ctx: Context): Unit =
    {
        val strsz = this.operator.length
        val inputsz = ctx.inputsz
        val input = ctx.input
        var i = ctx.offset
        var j = 0
        val operator = this.operator
        if (inputsz >= i + strsz)
        {
            while (j < strsz)
            {
                if (input(i) != operator(j))
                {
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
            ctx.col += strsz
            ctx.offset = i
            if (i < inputsz && letter(input(i))) ctx.fail(expectedEnd)
            else ctx.inc()
        }
        else ctx.fail(expected)
    }

    override def toString: String = s"TokenOperator(${_operator})"
}

private [parsley] class TokenMaxOp(_operator: String, _ops: Set[String], _expected: UnsafeOption[String]) extends Instr with NoPush
{
    val expected: UnsafeOption[String] = if (_expected == null) _operator else _expected
    val expectedEnd: UnsafeOption[String] = if (_expected == null) "end of " + _operator else _expected
    val operator = _operator.toCharArray
    val ops = for (op <- _ops.toList if op.length > _operator.length && op.startsWith(_operator)) yield op.substring(_operator.length)

    override def apply(ctx: Context): Unit =
    {
        val inputsz: Int = ctx.inputsz
        val input = ctx.input
        var i = ctx.offset
        var j = 0
        val operator = this.operator
        val strsz: Int = operator.length
        if (inputsz >= i + strsz)
        {
            while (j < strsz)
            {
                if (input(i) != operator(j))
                {
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
            j = i
            if (i < inputsz)
            {
                var ops = this.ops
                while (ops.nonEmpty && i < inputsz)
                {
                    val c = input(i)
                    ops = for (op <- ops if op.charAt(0) == c) yield
                    {
                        val op_ = op.substring(1)
                        if (op_.isEmpty)
                        {
                            ctx.fail(expectedEnd)
                            return
                        }
                        op_
                    }
                    i += 1
                }
                ctx.col = ctx.col + strsz
                ctx.offset = j
                ctx.inc()
            }
            else
            {
                ctx.col = ctx.col + strsz
                ctx.offset = j
                ctx.inc()
            }
        }
        else ctx.fail(expected)
    }

    override def toString: String = s"TokenMaxOp(${_operator})"
}