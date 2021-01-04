package parsley.internal.instructions

import parsley.internal.deepembedding.Sign.{SignType, IntType, DoubleType}
import parsley.TokenParser.TokenSet
import parsley.internal.{Radix, UnsafeOption}

import scala.annotation.{switch, tailrec}

// TODO This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [internal] class TokenComment(start: String, end: String, line: String, nested: Boolean) extends Instr {
    protected final val noLine = line.isEmpty
    protected final val noMulti = start.isEmpty
    override def apply(ctx: Context): Unit = {
        if (!ctx.moreInput) ctx.fail("comment")
        else if (noLine && noMulti) ctx.fail("comment")
        else if (noLine) {
            if (!ctx.input.startsWith(start, ctx.offset)) ctx.fail("comment")
            else {
                if (!multiLineComment(ctx)) return
                ctx.pushAndContinue(())
            }
        }
        else if (noMulti) {
            if (!ctx.input.startsWith(line, ctx.offset)) ctx.fail("comment")
            else {
                singleLineComment(ctx)
                ctx.pushAndContinue(())
            }
        }
        else {
            val startsSingle = ctx.input.startsWith(line, ctx.offset)
            val startsMulti = ctx.input.startsWith(start, ctx.offset)
            if (!startsSingle && !startsMulti) ctx.fail("comment")
            else {
                if (startsMulti) {
                    if (!multiLineComment(ctx)) return
                }
                else singleLineComment(ctx)
                ctx.pushAndContinue(())
            }
        }
    }

    protected final def singleLineComment(ctx: Context): Unit = {
        ctx.fastUncheckedConsumeChars(line.length)
        while (ctx.moreInput && ctx.nextChar != '\n') ctx.consumeChar()
    }

    protected final def multiLineComment(ctx: Context): Boolean = {
        ctx.fastUncheckedConsumeChars(start.length)
        var n = 1
        while (n != 0) {
            if (ctx.input.startsWith(end, ctx.offset)) {
                ctx.fastUncheckedConsumeChars(end.length)
                n -= 1
            }
            else if (nested && ctx.input.startsWith(start, ctx.offset)) {
                ctx.fastUncheckedConsumeChars(start.length)
                n += 1
            }
            else if (ctx.moreInput) ctx.consumeChar()
            else {
                ctx.fail("end of comment")
                return false
            }
        }
        true
    }
    // $COVERAGE-OFF$
    override def toString: String = "TokenComment"
    // $COVERAGE-ON$
}

// TODO This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
private [internal] final class TokenSkipComments(start: String, end: String, line: String, nested: Boolean) extends TokenComment(start, end, line, nested) {
    override def apply(ctx: Context): Unit = {
        if (noLine && !noMulti) {
            while (ctx.moreInput && ctx.input.startsWith(start, ctx.offset)) if (!multiLineComment(ctx)) return
        }
        else if (noMulti && !noLine) {
            while (ctx.moreInput && ctx.input.startsWith(line, ctx.offset)) singleLineComment(ctx)
        }
        else if (!noLine && !noMulti) {
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
            while (ctx.moreInput && (startsSingle || startsMulti)) {
                if (startsMulti) {
                    if (!multiLineComment(ctx)) return
                }
                else singleLineComment(ctx)
                startsSingle = ctx.input.startsWith(line, ctx.offset)
                startsMulti = ctx.input.startsWith(start, ctx.offset)
            }
        }
        ctx.pushAndContinue(())
    }
    // $COVERAGE-OFF$
    override def toString: String = "TokenSkipComments"
    // $COVERAGE-ON$
}

private [internal] final class TokenWhiteSpace(ws: TokenSet, start: String, end: String, line: String, nested: Boolean)
    extends TokenComment(start, end, line, nested) {
    override def apply(ctx: Context): Unit = {
        if (noLine && noMulti) spaces(ctx)
        else if (noLine) {
            spaces(ctx)
            while (ctx.moreInput && ctx.input.startsWith(start, ctx.offset)) {
                if (!multiLineComment(ctx)) return
                spaces(ctx)
            }
        }
        else if (noMulti) {
            spaces(ctx)
            while (ctx.moreInput && ctx.input.startsWith(line, ctx.offset)) {
                singleLineComment(ctx)
                spaces(ctx)
            }
        }
        else {
            spaces(ctx)
            // TODO This is considered as a VERY rough implementation of the intrinsic, just to get it working, it will be optimised later
            var startsSingle = ctx.input.startsWith(line, ctx.offset)
            var startsMulti = ctx.input.startsWith(start, ctx.offset)
            while (ctx.moreInput && (startsSingle || startsMulti)) {
                if (startsMulti) {
                    if (!multiLineComment(ctx)) return
                }
                else singleLineComment(ctx)
                spaces(ctx)
                startsSingle = ctx.input.startsWith(line, ctx.offset)
                startsMulti = ctx.input.startsWith(start, ctx.offset)
            }
        }
        ctx.pushAndContinue(())
    }

    private def spaces(ctx: Context): Unit = while (ctx.moreInput && ws(ctx.nextChar)) ctx.consumeChar()
    // $COVERAGE-OFF$
    override def toString: String = "TokenWhiteSpace"
    // $COVERAGE-ON$
}

private [internal] final class TokenSign(ty: SignType, _expected: UnsafeOption[String]) extends Instr {
    val expected = if (_expected == null) "sign" else _expected
    val neg: Any => Any = ty match {
        case IntType => ((x: Int) => -x).asInstanceOf[Any => Any]
        case DoubleType => ((x: Double) => -x).asInstanceOf[Any => Any]
    }
    val pos: Any => Any = x => x

    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput) {
            if (ctx.nextChar == '-') {
                ctx.fastUncheckedConsumeChars(1)
                ctx.stack.push(neg)
            }
            else {
                if (ctx.nextChar == '+') ctx.fastUncheckedConsumeChars(1)
                ctx.stack.push(pos)
            }
        }
        ctx.inc()
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenSign"
    // $COVERAGE-ON$
}

private [instructions] sealed trait NumericReader {
    private final def subDecimal(base: Int, maxDigit: Char, ctx: Context): Int => Int = {
        @tailrec def go(x: Int): Int = {
            if (ctx.moreInput && ctx.nextChar >= '0' && ctx.nextChar <= maxDigit) {
                val d = ctx.nextChar.asDigit
                ctx.fastUncheckedConsumeChars(1)
                go(x * base + d)
            }
            else x
        }
        go
    }
    protected final def decimal(ctx: Context, firstDigit: Int = 0): Int = subDecimal(10, '9', ctx)(firstDigit)
    protected final def octal(ctx: Context, firstDigit: Int = 0): Int = subDecimal(8, '7', ctx)(firstDigit)

    @tailrec protected final def hexadecimal(ctx: Context, x: Int = 0): Int = {
        if (ctx.moreInput) {
            (ctx.nextChar: @switch) match {
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                    ctx.fastUncheckedConsumeChars(1)
                    hexadecimal(ctx, x * 16 + d.asDigit)
                case _ => x
            }
        }
        else x
    }
}

private [internal] final class TokenNatural(_expected: UnsafeOption[String]) extends Instr with NumericReader {
    val expected = if (_expected == null) "natural" else _expected
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput) (ctx.nextChar: @switch) match {
            case '0' =>
                ctx.fastUncheckedConsumeChars(1)
                if (!ctx.moreInput) ctx.pushAndContinue(0)
                else {
                    (ctx.nextChar: @switch) match {
                        case 'x' | 'X' =>
                            ctx.fastUncheckedConsumeChars(1)
                            if (ctx.moreInput) {
                                (ctx.nextChar: @switch) match {
                                    case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                                            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                                            | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                                        ctx.fastUncheckedConsumeChars(1)
                                        ctx.pushAndContinue(hexadecimal(ctx, d.asDigit))
                                    case _ => ctx.fail(expected)
                                }
                            }
                            else ctx.fail(expected)
                        case 'o' | 'O' =>
                            ctx.fastUncheckedConsumeChars(1)
                            if (ctx.moreInput) {
                                val d = ctx.nextChar
                                if (d >= '0' && d <= '7') {
                                    ctx.fastUncheckedConsumeChars(1)
                                    ctx.pushAndContinue(octal(ctx, d.asDigit))
                                }
                                else ctx.fail(expected)
                            }
                            else ctx.fail(expected)
                        case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                            ctx.fastUncheckedConsumeChars(1)
                            ctx.pushAndContinue(decimal(ctx, d.asDigit))
                        case _ => ctx.pushAndContinue(0)
                    }
                }
            case d@('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                ctx.fastUncheckedConsumeChars(1)
                ctx.pushAndContinue(decimal(ctx, d.asDigit))
            case _ => ctx.fail(expected)
        }
        else ctx.fail(expected)
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenNatural"
    // $COVERAGE-ON$
}

private [internal] final class TokenFloat(_expected: UnsafeOption[String]) extends Instr {
    val expected = if (_expected == null) "unsigned float" else _expected
    override def apply(ctx: Context): Unit = {
        var failed = false
        if (ctx.moreInput) (ctx.nextChar: @switch) match {
            case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                ctx.fastUncheckedConsumeChars(1)
                val builder = new StringBuilder()
                failed = decimal(ctx, builder += d, false)
                if (ctx.moreInput) (ctx.nextChar: @switch) match {
                    case '.' => // fraction
                        ctx.fastUncheckedConsumeChars(1)
                        failed = decimal(ctx, builder += '.')
                        if (!failed) {
                            if (ctx.moreInput && (ctx.nextChar == 'e' || ctx.nextChar == 'E')) {
                                ctx.fastUncheckedConsumeChars(1)
                                failed = exponent(ctx, builder += 'e')
                            }
                            if (!failed) try ctx.stack.push(builder.toString.toDouble)
                            catch { case _: NumberFormatException => failed = true }
                        }
                    case 'e' | 'E' => // exponent
                        ctx.fastUncheckedConsumeChars(1)
                        failed = exponent(ctx, builder += 'e')
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

    @tailrec private def decimal(ctx: Context, x: StringBuilder, first: Boolean = true): Boolean = {
        if (ctx.moreInput) {
            val d = ctx.nextChar
            if (d >= '0' && d <= '9') {
                ctx.fastUncheckedConsumeChars(1)
                decimal(ctx, x += d, false)
            }
            else first
        }
        else first
    }

    private def exponent(ctx: Context, x: StringBuilder): Boolean = {
        if (ctx.moreInput) {
            ctx.nextChar match {
                case '+' =>
                    ctx.fastUncheckedConsumeChars(1)
                    decimal(ctx, x)
                case '-' =>
                    ctx.fastUncheckedConsumeChars(1)
                    decimal(ctx, x += '-')
                case _ => decimal(ctx, x)
            }
        }
        else true
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenFloat"
    // $COVERAGE-ON$
}

object TokenEscape {
    private [instructions] sealed trait Escape
    private [instructions] case class EscapeChar(escapeChar: Char) extends Escape
    private [instructions] case object BadCode extends Escape
    private [instructions] case object NoParse extends Escape
}
private [internal] class TokenEscape(_expected: UnsafeOption[String]) extends Instr with Stateful with NumericReader {
    private [this] final val expected = if (_expected == null) "escape code" else _expected
    protected var escapeChar: Char = _
    protected var badCode: Boolean = _
    override def apply(ctx: Context): Unit = escape(ctx) match {
        case TokenEscape.EscapeChar(escapeChar) =>ctx.pushAndContinue(escapeChar)
        case TokenEscape.BadCode => ctx.failWithMessage(expected, msg = "invalid escape sequence")
        case TokenEscape.NoParse => ctx.fail(expected)
    }

    protected final def escape(ctx: Context): TokenEscape.Escape = {
        if (ctx.moreInput) {
            (ctx.nextChar: @switch) match {
                case 'a' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\u0007')
                case 'b' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\b')
                case 'f' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\u000c')
                case 'n' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\n')
                case 'r' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\r')
                case 't' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\t')
                case 'v' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\u000b')
                case '\\' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\\')
                case '\"' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\"')
                case '\'' => ctx.fastUncheckedConsumeChars(1); new TokenEscape.EscapeChar('\'')
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
                    ctx.fastUncheckedConsumeChars(1)
                    val escapeCode = decimal(ctx, d.asDigit)
                    if (escapeCode <= 0x10FFFF) new TokenEscape.EscapeChar(escapeCode.toChar)
                    else TokenEscape.BadCode
                case 'x' =>
                    ctx.fastUncheckedConsumeChars(1)
                    if (ctx.moreInput) {
                        (ctx.nextChar: @switch) match {
                            case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
                                    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
                                    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F') =>
                                ctx.fastUncheckedConsumeChars(1)
                                val escapeCode = hexadecimal(ctx, d.asDigit)
                                if (escapeCode <= 0x10FFFF) new TokenEscape.EscapeChar(escapeCode.toChar)
                                else TokenEscape.BadCode
                            case _ => TokenEscape.NoParse
                        }
                    }
                    else TokenEscape.NoParse
                case 'o' =>
                    ctx.fastUncheckedConsumeChars(1)
                    if (ctx.moreInput) {
                        val d = ctx.nextChar
                        if (d >= '0' && d <= '7') {
                            ctx.fastUncheckedConsumeChars(1)
                            val escapeCode = octal(ctx, d.asDigit)
                            if (escapeCode <= 0x10FFFF) new TokenEscape.EscapeChar(escapeCode.toChar)
                            else TokenEscape.BadCode
                        }
                        else TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case '^' =>
                    ctx.fastUncheckedConsumeChars(1)
                    if (ctx.moreInput) {
                        val c = ctx.nextChar
                        if (c >= 'A' && c <= 'Z') {
                            ctx.fastUncheckedConsumeChars(1)
                            new TokenEscape.EscapeChar((c - 'A' + 1).toChar)
                        }
                        else TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case 'A' => //ACK
                    if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'C' && ctx.input(ctx.offset + 2) == 'K') {
                        ctx.fastUncheckedConsumeChars(3)
                        new TokenEscape.EscapeChar('\u0006')
                    }
                    else TokenEscape.NoParse
                case 'B' => //BS BEL
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u0008')
                    }
                    else if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'E' && ctx.input(ctx.offset + 2) == 'L') {
                        ctx.fastUncheckedConsumeChars(3)
                        new TokenEscape.EscapeChar('\u0007')
                    }
                    else TokenEscape.NoParse
                case 'C' => //CR CAN
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'R') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u000d')
                    }
                    else if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'A' && ctx.input(ctx.offset + 2) == 'N') {
                        ctx.fastUncheckedConsumeChars(3)
                        new TokenEscape.EscapeChar('\u0018')
                    }
                    else TokenEscape.NoParse
                case 'D' => //DC1 DC2 DC3 DC4 DEL DLE
                    if (ctx.offset + 2 < ctx.inputsz) (ctx.input(ctx.offset + 1): @switch) match {
                        case 'C' => (ctx.input(ctx.offset + 2): @switch) match {
                            case '1' => ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0011')
                            case '2' => ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0012')
                            case '3' => ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0013')
                            case '4' => ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0014')
                            case _ => TokenEscape.NoParse
                        }
                        case 'E' =>
                            if (ctx.input(ctx.offset + 2) == 'L') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u001f') }
                            else TokenEscape.NoParse
                        case 'L' =>
                            if (ctx.input(ctx.offset + 2) == 'E') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0010') }
                            else TokenEscape.NoParse
                        case _ => TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case 'E' => //EM ETX ETB ESC EOT ENQ
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'M') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u0019')
                    }
                    else if (ctx.offset + 2 < ctx.inputsz) (ctx.input(ctx.offset + 1): @switch) match {
                        case 'N' =>
                            if (ctx.input(ctx.offset + 2) == 'Q') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0005') }
                            else TokenEscape.NoParse
                        case 'O' =>
                            if (ctx.input(ctx.offset + 2) == 'T') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0004') }
                            else TokenEscape.NoParse
                        case 'S' =>
                            if (ctx.input(ctx.offset + 2) == 'C') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u001b') }
                            else TokenEscape.NoParse
                        case 'T' =>
                            if (ctx.input(ctx.offset + 2) == 'X') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0003') }
                            else if (ctx.input(ctx.offset + 2) == 'B') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0017') }
                            else TokenEscape.NoParse
                        case _ => TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case 'F' => //FF FS
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'F') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u000c')
                    }
                    else if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u001c')
                    }
                    else TokenEscape.NoParse
                case 'G' => //GS
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u001d')
                    }
                    else TokenEscape.NoParse
                case 'H' => //HT
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'T') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u0009')
                    }
                    else TokenEscape.NoParse
                case 'L' => //LF
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'F') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\n')
                    }
                    else TokenEscape.NoParse
                case 'N' => //NUL NAK
                    if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'U' && ctx.input(ctx.offset + 2) == 'L') {
                        ctx.fastUncheckedConsumeChars(3)
                        new TokenEscape.EscapeChar('\u0000')
                    }
                    else if (ctx.offset + 2 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'A' && ctx.input(ctx.offset + 2) == 'K') {
                        ctx.fastUncheckedConsumeChars(3)
                        new TokenEscape.EscapeChar('\u0015')
                    }
                    else TokenEscape.NoParse
                case 'R' => //RS
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u001e')
                    }
                    else TokenEscape.NoParse
                case 'S' => //SO SI SP SOH STX SYN SUB
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'O') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u000e')
                    }
                    else if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'I') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u000f')
                    }
                    else if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'P') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u0020')
                    }
                    else if (ctx.offset + 2 < ctx.inputsz) (ctx.input(ctx.offset + 1): @switch) match {
                        case 'O' =>
                            if (ctx.input(ctx.offset + 2) == 'H') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0001') }
                            else TokenEscape.NoParse
                        case 'T' =>
                            if (ctx.input(ctx.offset + 2) == 'X') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0002') }
                            else TokenEscape.NoParse
                        case 'Y' =>
                            if (ctx.input(ctx.offset + 2) == 'N') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u0016') }
                            else TokenEscape.NoParse
                        case 'U' =>
                            if (ctx.input(ctx.offset + 2) == 'B') { ctx.fastUncheckedConsumeChars(3); new TokenEscape.EscapeChar('\u001a') }
                            else TokenEscape.NoParse
                        case _ => TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case 'U' => //US
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'S') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u001f')
                    }
                    else TokenEscape.NoParse
                case 'V' => //VT
                    if (ctx.offset + 1 < ctx.inputsz && ctx.input(ctx.offset + 1) == 'T') {
                        ctx.fastUncheckedConsumeChars(2)
                        new TokenEscape.EscapeChar('\u000b')
                    }
                    else TokenEscape.NoParse
                case _ => TokenEscape.NoParse
            }
        }
        else TokenEscape.NoParse
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenEscape"
    // $COVERAGE-ON$
    override def copy: TokenEscape = new TokenEscape(expected)
}

private [instructions] sealed trait TokenStringLike extends Instr {
    protected val expected: UnsafeOption[String]
    final protected lazy val expectedString = if (expected == null) "string" else expected
    final protected lazy val expectedEos = if (expected == null) "end of string" else expected
    final protected lazy val expectedChar = if (expected == null) "string character" else expected

    // All failures must be handled by this function
    protected def handleEscaped(ctx: Context, builder: StringBuilder): Boolean
    @tailrec private final def restOfString(ctx: Context, builder: StringBuilder): Unit = {
        if (ctx.moreInput) ctx.nextChar match {
                case '"' =>
                    ctx.fastUncheckedConsumeChars(1)
                    ctx.pushAndContinue(builder.toString)
                case '\\' =>
                    ctx.fastUncheckedConsumeChars(1)
                    if (handleEscaped(ctx, builder)) restOfString(ctx, builder)
                case c if c > '\u0016' =>
                    builder += c
                    ctx.fastUncheckedConsumeChars(1)
                    restOfString(ctx, builder)
                case _ => ctx.fail(expectedChar)
            }
            else ctx.fail(expectedEos)
    }
    final override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == '"') {
            ctx.fastUncheckedConsumeChars(1)
            restOfString(ctx, new StringBuilder())
        }
        else ctx.fail(expectedString)
    }
}

private [internal] final class TokenRawString(_expected: UnsafeOption[String]) extends TokenStringLike {
    override val expected = _expected
    override def handleEscaped(ctx: Context, builder: StringBuilder): Boolean = {
        builder += '\\'
        if (ctx.moreInput && ctx.nextChar > '\u0016') {
            builder += ctx.nextChar
            ctx.fastUncheckedConsumeChars(1)
            true
        }
        else {
            ctx.fail(expectedChar)
            false
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenRawString"
    // $COVERAGE-ON$
}

private [internal] final class TokenString(ws: TokenSet, _expected: UnsafeOption[String]) extends TokenEscape(_expected) with TokenStringLike {
    override val expected = _expected
    private val expectedEscape = if (_expected == null) "escape code" else _expected
    private val expectedGap = if (_expected == null) "end of string gap" else _expected

    private def readGap(ctx: Context): Boolean = {
        val completedGap = ctx.moreInput && ctx.nextChar == '\\'
        if (completedGap) ctx.fastUncheckedConsumeChars(1)
        else ctx.fail(expectedGap)
        completedGap
    }

    override def handleEscaped(ctx: Context, builder: StringBuilder): Boolean = {
        if (spaces(ctx) != 0) readGap(ctx)
        else if (ctx.moreInput && ctx.nextChar == '&') {
            ctx.fastUncheckedConsumeChars(1)
            true
        }
        else escape(ctx) match {
            case TokenEscape.EscapeChar(c) =>
                builder += c
                true
            case TokenEscape.BadCode =>
                ctx.failWithMessage(expectedEscape, "invalid escape sequence")
                false
            case TokenEscape.NoParse =>
                ctx.fail(expectedEscape)
                false
        }
    }

    @tailrec private def spaces(ctx: Context, n: Int = 0): Int = {
        if (ctx.moreInput && ws(ctx.nextChar)) {
            ctx.consumeChar()
            spaces(ctx, n + 1)
        }
        else n
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenString"
    // $COVERAGE-ON$
    override def copy: TokenString = new TokenString(ws, _expected)
}

private [instructions] abstract class TokenLexi(name: String, illegalName: String)
                                               (start: TokenSet, letter: TokenSet, illegal: String => Boolean, _expected: UnsafeOption[String]) extends Instr {
    private val expected = if (_expected == null) name else _expected

    final override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && start(ctx.nextChar)) {
            val name = new StringBuilder()
            name += ctx.nextChar
            ctx.offset += 1
            restOfToken(ctx, name)
        }
        else ctx.fail(expected)
    }

    private def ensureLegal(ctx: Context, tok: String) = {
        if (illegal(tok)) {
            ctx.offset -= tok.length
            ctx.unexpectedFail(expected = expected, unexpected = s"$illegalName $tok")
        }
        else {
            ctx.col += tok.length
            ctx.pushAndContinue(tok)
        }
    }

    @tailrec private final def restOfToken(ctx: Context, tok: StringBuilder): Unit = {
        if (ctx.moreInput && letter(ctx.nextChar)) {
            tok += ctx.nextChar
            ctx.offset += 1
            restOfToken(ctx, tok)
        }
        else ensureLegal(ctx, tok.toString)
    }

    // $COVERAGE-OFF$
    final override def toString: String = s"TokenLexi($name)"
    // $COVERAGE-ON$
}

private [internal] final class TokenIdentifier(start: TokenSet, letter: TokenSet, keywords: Set[String], _expected: UnsafeOption[String])
    extends TokenLexi("identifier", "keyword")(start, letter, keywords, _expected)

private [internal] final class TokenUserOperator(start: TokenSet, letter: TokenSet, reservedOps: Set[String], _expected: UnsafeOption[String])
    extends TokenLexi("operator", "reserved operator")(start, letter, reservedOps, _expected)

private [internal] final class TokenOperator(start: TokenSet, letter: TokenSet, reservedOps: Set[String], _expected: UnsafeOption[String])
    extends TokenLexi("operator", "non-reserved operator")(start, letter, reservedOps.andThen(!_), _expected)

private [instructions] abstract class TokenSpecific(_specific: String, caseSensitive: Boolean, _expected: UnsafeOption[String]) extends Instr {
    private final val expected = if (_expected == null) _specific else _expected
    protected final val expectedEnd = if (_expected == null) "end of " + _specific else _expected
    private final val specific = (if (caseSensitive) _specific else _specific.toLowerCase).toCharArray
    private final val strsz = specific.length
    protected def postprocess(ctx: Context, i: Int): Unit

    @tailrec final private def readSpecific(ctx: Context, i: Int, j: Int): Unit = {
        if (j < strsz) {
            val c = if (caseSensitive) ctx.input(i) else ctx.input(i).toLower
            if (c != specific(j)) ctx.fail(expected)
            else readSpecific(ctx, i + 1, j + 1)
        }
        else {
            ctx.saveState()
            ctx.fastUncheckedConsumeChars(strsz)
            postprocess(ctx, i)
        }
    }

    final override def apply(ctx: Context): Unit = {
        if (ctx.inputsz >= ctx.offset + strsz) readSpecific(ctx, ctx.offset, 0)
        else ctx.fail(expected)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"TokenSpecific(${_specific})"
    // $COVERAGE-ON$
}

private [internal] abstract class TokenSpecificNoTrailLetter(keyword: String, letter: TokenSet, caseSensitive: Boolean, expected: UnsafeOption[String])
    extends TokenSpecific(keyword, caseSensitive, expected) {
    final override def postprocess(ctx: Context, i: Int): Unit = {
        if (i < ctx.inputsz && letter(ctx.input(i))) {
            ctx.fail(expectedEnd)
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }
}

private [internal] final class TokenKeyword(keyword: String, letter: TokenSet, caseSensitive: Boolean, expected: UnsafeOption[String])
    extends TokenSpecificNoTrailLetter(keyword, letter, caseSensitive, expected)

private [internal] final class TokenOperator_(operator: String, letter: TokenSet, expected: UnsafeOption[String])
    extends TokenSpecificNoTrailLetter(operator, letter, true, expected)

private [internal] final class TokenMaxOp(operator: String, _ops: Set[String], expected: UnsafeOption[String])
    extends TokenSpecific(operator, true, expected) {
    private val ops = Radix(_ops.collect {
        case op if op.length > operator.length && op.startsWith(operator) => op.substring(operator.length)
    })

    @tailrec private def go(ctx: Context, i: Int, ops: Radix[Unit]): Unit = {
        lazy val ops_ = ops.suffixes(ctx.input(i))
        val possibleOpsRemain = i < ctx.inputsz && ops.nonEmpty
        if (possibleOpsRemain && ops_.contains("")) {
            ctx.fail(expectedEnd)
            ctx.restoreState()
        }
        else if (possibleOpsRemain) go(ctx, i + 1, ops_)
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    override def postprocess(ctx: Context, i: Int): Unit = go(ctx, i, ops)

    // $COVERAGE-OFF$
    override def toString: String = s"TokenMaxOp(${operator})"
    // $COVERAGE-ON$
}