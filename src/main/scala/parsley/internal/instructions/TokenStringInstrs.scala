package parsley.internal.instructions

import parsley.internal.UnsafeOption
import parsley.TokenParser.TokenSet

import scala.annotation.tailrec

private [internal] class TokenEscape(_expected: UnsafeOption[String]) extends Instr with NumericReader {
    private [this] final val expected = if (_expected == null) "escape code" else _expected
    override def apply(ctx: Context): Unit = escape(ctx) match {
        case TokenEscape.EscapeChar(escapeChar) =>ctx.pushAndContinue(escapeChar)
        case TokenEscape.BadCode => ctx.failWithMessage(expected, msg = "invalid escape sequence")
        case TokenEscape.NoParse => ctx.fail(expected)
    }

    private final def consumeAndReturn(ctx: Context, n: Int, c: Char) = {
        ctx.fastUncheckedConsumeChars(n)
        new TokenEscape.EscapeChar(c)
    }

    private final def lookAhead(ctx: Context, n: Int): Char = ctx.input(ctx.offset + n)
    private final def lookAhead(ctx: Context, n: Int, c: Char): Boolean = ctx.offset + n < ctx.inputsz && lookAhead(ctx, n) == c

    private final def numericEscape(ctx: Context, escapeCode: Int) = {
        if (escapeCode <= 0x10FFFF) new TokenEscape.EscapeChar(escapeCode.toChar)
        else TokenEscape.BadCode
    }

    private final def nonDecimalNumericEscape(ctx: Context, lexer: (Context, Int, Boolean) => Option[Int]) = {
        ctx.fastUncheckedConsumeChars(1)
        lexer(ctx, 0, true) match {
            case Some(x) => numericEscape(ctx, x)
            case None => TokenEscape.NoParse
        }
    }

    private final def decimalEscape(ctx: Context, d: Int) = {
        ctx.fastUncheckedConsumeChars(1)
        numericEscape(ctx, decimal(ctx, d, false).get)
    }
    private final def hexadecimalEscape(ctx: Context) = nonDecimalNumericEscape(ctx, hexadecimal)
    private final def octalEscape(ctx: Context) = nonDecimalNumericEscape(ctx, octal)
    private final def caretEscape(ctx: Context) = {
        ctx.fastUncheckedConsumeChars(1)
        if (ctx.moreInput && ctx.nextChar >= 'A' && ctx.nextChar <= 'Z') consumeAndReturn(ctx, 1, (ctx.nextChar - 'A' + 1).toChar)
        else TokenEscape.NoParse
    }

    protected final def escape(ctx: Context): TokenEscape.Escape = {
        val threeAvailable = ctx.offset + 2 < ctx.inputsz
        if (ctx.moreInput) {
            ctx.nextChar match {
                case 'a' => consumeAndReturn(ctx, 1, '\u0007')
                case 'b' => consumeAndReturn(ctx, 1, '\b')
                case 'f' => consumeAndReturn(ctx, 1, '\u000c')
                case 'n' => consumeAndReturn(ctx, 1, '\n')
                case 'r' => consumeAndReturn(ctx, 1, '\r')
                case 't' => consumeAndReturn(ctx, 1, '\t')
                case 'v' => consumeAndReturn(ctx, 1, '\u000b')
                case '\\' => consumeAndReturn(ctx, 1, '\\')
                case '\"' => consumeAndReturn(ctx, 1, '\"')
                case '\'' => consumeAndReturn(ctx, 1, '\'')
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => decimalEscape(ctx, d.asDigit)
                case 'x' => hexadecimalEscape(ctx)
                case 'o' => octalEscape(ctx)
                case '^' => caretEscape(ctx)
                case 'A' if threeAvailable && lookAhead(ctx, 1) == 'C' && lookAhead(ctx, 2) == 'K' => consumeAndReturn(ctx, 3, '\u0006') //ACK
                case 'B' => //BS BEL
                    if (lookAhead(ctx, 1, 'S')) consumeAndReturn(ctx, 2, '\u0008')
                    else if (lookAhead(ctx, 2, 'L') && lookAhead(ctx, 1) == 'E') {
                        consumeAndReturn(ctx, 3, '\u0007')
                    }
                    else TokenEscape.NoParse
                case 'C' => //CR CAN
                    if (lookAhead(ctx, 1, 'R')) consumeAndReturn(ctx, 2, '\u000d')
                    else if (lookAhead(ctx, 2, 'N') && lookAhead(ctx, 1) == 'A') consumeAndReturn(ctx, 3, '\u0018')
                    else TokenEscape.NoParse
                case 'D' if threeAvailable => //DC1 DC2 DC3 DC4 DEL DLE
                    val c = lookAhead(ctx, 2)
                    lookAhead(ctx, 1) match {
                        case 'C' if c == '1' => consumeAndReturn(ctx, 3, '\u0011')
                        case 'C' if c == '2' => consumeAndReturn(ctx, 3, '\u0012')
                        case 'C' if c == '3' => consumeAndReturn(ctx, 3, '\u0013')
                        case 'C' if c == '4' => consumeAndReturn(ctx, 3, '\u0014')
                        case 'E' if c == 'L' => consumeAndReturn(ctx, 3, '\u001f')
                        case 'L' if c == 'E' => consumeAndReturn(ctx, 3, '\u0010')
                        case _ => TokenEscape.NoParse
                    }
                case 'E' => //EM ETX ETB ESC EOT ENQ
                    if (lookAhead(ctx, 1, 'M')) consumeAndReturn(ctx, 2, '\u0019')
                    else if (threeAvailable) lookAhead(ctx, 1) match {
                        case 'N' if lookAhead(ctx, 2) == 'Q' => consumeAndReturn(ctx, 3, '\u0005')
                        case 'O' if lookAhead(ctx, 2) == 'T' => consumeAndReturn(ctx, 3, '\u0004')
                        case 'S' if lookAhead(ctx, 2) == 'C' => consumeAndReturn(ctx, 3, '\u001b')
                        case 'T' if lookAhead(ctx, 2) == 'X' => consumeAndReturn(ctx, 3, '\u0003')
                        case 'T' if lookAhead(ctx, 2) == 'B' => consumeAndReturn(ctx, 3, '\u0017')
                        case _ => TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case 'F' => //FF FS
                    if (lookAhead(ctx, 1, 'F')) consumeAndReturn(ctx, 2, '\u000c')
                    else if (lookAhead(ctx, 1, 'S')) consumeAndReturn(ctx, 2, '\u001c')
                    else TokenEscape.NoParse
                case 'G' if lookAhead(ctx, 1, 'S') => consumeAndReturn(ctx, 2, '\u001d') //GS
                case 'H' if lookAhead(ctx, 1, 'T') => consumeAndReturn(ctx, 2, '\u0009') //HT
                case 'L' if lookAhead(ctx, 1, 'F') => consumeAndReturn(ctx, 2, '\n')     //LF
                case 'N' => //NUL NAK
                    if (threeAvailable && lookAhead(ctx, 1) == 'U' && lookAhead(ctx, 2) == 'L') consumeAndReturn(ctx, 3, '\u0000')
                    else if (threeAvailable && lookAhead(ctx, 1) == 'A' && lookAhead(ctx, 2) == 'K') {
                        consumeAndReturn(ctx, 3, '\u0015')
                    }
                    else TokenEscape.NoParse
                case 'R' if lookAhead(ctx, 1, 'S') => consumeAndReturn(ctx, 2, '\u001e') //RS
                case 'S' => //SO SI SP SOH STX SYN SUB
                    if (lookAhead(ctx, 1, 'O')) consumeAndReturn(ctx, 2, '\u000e')
                    else if (lookAhead(ctx, 1, 'I')) consumeAndReturn(ctx, 2, '\u000f')
                    else if (lookAhead(ctx, 1, 'P')) consumeAndReturn(ctx, 2, '\u0020')
                    else if (threeAvailable) lookAhead(ctx, 1) match {
                        case 'O' if lookAhead(ctx, 2) == 'H' => consumeAndReturn(ctx, 3, '\u0001')
                        case 'T' if lookAhead(ctx, 2) == 'X' => consumeAndReturn(ctx, 3, '\u0002')
                        case 'Y' if lookAhead(ctx, 2) == 'N' => consumeAndReturn(ctx, 3, '\u0016')
                        case 'U' if lookAhead(ctx, 2) == 'B' => consumeAndReturn(ctx, 3, '\u001a')
                        case _ => TokenEscape.NoParse
                    }
                    else TokenEscape.NoParse
                case 'U' if lookAhead(ctx, 1, 'S') => consumeAndReturn(ctx, 2, '\u001f') //US
                case 'V' if lookAhead(ctx, 1, 'T') => consumeAndReturn(ctx, 2, '\u000b') //VT
                case _ => TokenEscape.NoParse
            }
        }
        else TokenEscape.NoParse
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenEscape"
    // $COVERAGE-ON$
}
private [instructions] object TokenEscape {
    private [instructions] sealed trait Escape
    private [instructions] case class EscapeChar(escapeChar: Char) extends Escape
    private [instructions] case object BadCode extends Escape
    private [instructions] case object NoParse extends Escape
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
}
