package parsley.internal.machine.instructions

import parsley.internal.errors.{ErrorItem, Desc}
import parsley.internal.machine.Context
import parsley.internal.Radix

import scala.annotation.tailrec

private [internal] class TokenEscape extends Instr with NumericReader {
    private [this] final val expected = Some(Desc("escape code"))
    override def apply(ctx: Context): Unit = escape(ctx) match {
        case TokenEscape.EscapeChar(escapeChar) =>ctx.pushAndContinue(escapeChar)
        case TokenEscape.BadCode => ctx.expectedFail(expected, reason = "invalid escape sequence")
        case TokenEscape.NoParse => ctx.expectedTokenFail(expected, 3)
    }

    private final def consumeAndReturn(ctx: Context, n: Int, c: Char) = {
        ctx.fastUncheckedConsumeChars(n)
        new TokenEscape.EscapeChar(c)
    }

    private final def lookAhead(ctx: Context, n: Int): Char = ctx.input.charAt(ctx.offset + n)
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

    private final def nonWordEscape(ctx: Context) = {
        if (ctx.moreInput) {
            ctx.nextChar match {
                case d@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => decimalEscape(ctx, d.asDigit)
                case 'x' => hexadecimalEscape(ctx)
                case 'o' => octalEscape(ctx)
                case '^' => caretEscape(ctx)
                case _ => TokenEscape.NoParse
            }
        }
        else TokenEscape.NoParse
    }

    protected final def escape(ctx: Context): TokenEscape.Escape = {
        // This is a bit dodgy, but oh well: works for now.
        // Basically, getMax needs to be able to stream in input from a source I guess?
        TokenEscape.escRadix.getMax(ctx.input.substring(ctx.offset).iterator.buffered) match {
            case Some((c, n)) => consumeAndReturn(ctx, n, c)
            case None => nonWordEscape(ctx)
        }
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

    private [TokenEscape] val escRadix = Radix[(Char, Int)](
        "a"   -> ('\u0007', 1),
        "b"   -> ('\b', 1),
        "f"   -> ('\u000c', 1),
        "n"   -> ('\n', 1),
        "r"   -> ('\r', 1),
        "t"   -> ('\t', 1),
        "v"   -> ('\u000b', 1),
        "\\"  -> ('\\', 1),
        "\""  -> ('\"', 1),
        "\'"  -> ('\'', 1),
        "ACK" -> ('\u0006', 3),
        "BS"  -> ('\u0008', 2),
        "BEL" -> ('\u0007', 3),
        "CR"  -> ('\u000d', 2),
        "CAN" -> ('\u0018', 3),
        "DC1" -> ('\u0011', 3),
        "DC2" -> ('\u0012', 3),
        "DC3" -> ('\u0013', 3),
        "DC4" -> ('\u0014', 3),
        "DEL" -> ('\u001f', 3),
        "DLE" -> ('\u0010', 3),
        "EM"  -> ('\u0019', 2),
        "ENQ" -> ('\u0005', 3),
        "EOT" -> ('\u0004', 3),
        "ESC" -> ('\u001b', 3),
        "ETX" -> ('\u0003', 3),
        "ETB" -> ('\u0017', 3),
        "FF"  -> ('\u000c', 2),
        "FS"  -> ('\u001c', 2),
        "GS"  -> ('\u001d', 2),
        "HT"  -> ('\u0009', 2),
        "LF"  -> ('\n', 2),
        "NUL" -> ('\u0000', 3),
        "NAK" -> ('\u0015', 3),
        "RS"  -> ('\u001e', 2),
        "SO"  -> ('\u000e', 2),
        "SI"  -> ('\u000f', 2),
        "SP"  -> ('\u0020', 2),
        "SOH" -> ('\u0001', 3),
        "STX" -> ('\u0002', 3),
        "SYN" -> ('\u0016', 3),
        "SUB" -> ('\u001a', 3),
        "US"  -> ('\u001f', 2),
        "VT"  -> ('\u000b', 2)
    )
}

private [instructions] sealed trait TokenStringLike extends Instr {
    final protected lazy val expectedString = Some(Desc("string"))
    final protected lazy val expectedEos = Some(Desc("end of string"))
    final protected lazy val expectedChar = Some(Desc("string character"))

    // All failures must be handled by this function
    protected def handleEscaped(ctx: Context, builder: StringBuilder): Boolean
    @tailrec private final def restOfString(ctx: Context, builder: StringBuilder): Unit = {
        if (ctx.moreInput) ctx.nextChar match {
                case '"' =>
                    ctx.fastUncheckedConsumeChars(1)
                    ctx.pushAndContinue(builder.result())
                case '\\' =>
                    ctx.fastUncheckedConsumeChars(1)
                    if (handleEscaped(ctx, builder)) restOfString(ctx, builder)
                case c if c > '\u0016' =>
                    builder += c
                    ctx.fastUncheckedConsumeChars(1)
                    restOfString(ctx, builder)
                case _ => ctx.expectedFail(expectedChar)
            }
            else ctx.expectedFail(expectedEos)
    }
    final override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == '"') {
            ctx.fastUncheckedConsumeChars(1)
            restOfString(ctx, new StringBuilder)
        }
        else ctx.expectedFail(expectedString)
    }
}

private [internal] object TokenRawString extends TokenStringLike {
    override def handleEscaped(ctx: Context, builder: StringBuilder): Boolean = {
        builder += '\\'
        if (ctx.moreInput && ctx.nextChar > '\u0016') {
            builder += ctx.nextChar
            ctx.fastUncheckedConsumeChars(1)
            true
        }
        else {
            ctx.expectedFail(expectedChar)
            false
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenRawString"
    // $COVERAGE-ON$
}

private [internal] final class TokenString(ws: Char => Boolean) extends TokenEscape with TokenStringLike {
    private [this] final val expectedEscape = Some(Desc("escape code"))
    private [this] final val expectedGap = Some(Desc("end of string gap"))

    private def readGap(ctx: Context): Boolean = {
        val completedGap = ctx.moreInput && ctx.nextChar == '\\'
        if (completedGap) ctx.fastUncheckedConsumeChars(1)
        else ctx.expectedFail(expectedGap)
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
                ctx.expectedFail(expectedEscape, reason = "invalid escape sequence")
                false
            case TokenEscape.NoParse =>
                ctx.expectedFail(expectedEscape)
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
