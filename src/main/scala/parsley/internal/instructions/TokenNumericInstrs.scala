package parsley.internal.instructions

import parsley.internal.deepembedding.Sign.{SignType, IntType, DoubleType}
import parsley.internal.UnsafeOption

import scala.annotation.tailrec

private [internal] final class TokenSign(ty: SignType, _expected: UnsafeOption[String]) extends Instr {
    val expected = if (_expected == null) "sign" else _expected
    val neg: Any => Any = ty match {
        case IntType => ((x: Int) => -x).asInstanceOf[Any => Any]
        case DoubleType => ((x: Double) => -x).asInstanceOf[Any => Any]
    }
    val pos = (x: Any) => x

    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == '-') {
            ctx.fastUncheckedConsumeChars(1)
            ctx.stack.push(neg)
        }
        else if (ctx.moreInput && ctx.nextChar == '+') {
            ctx.fastUncheckedConsumeChars(1)
            ctx.stack.push(pos)
        }
        else ctx.stack.push(pos)
        ctx.inc()
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenSign"
    // $COVERAGE-ON$
}

private [instructions] trait NumericReader {
    private final def subDecimal(base: Int, isDigit: Char => Boolean): (Context, Int, Boolean) => Option[Int] = {
        @tailrec def go(ctx: Context, x: Int, first: Boolean): Option[Int] = {
            if (ctx.moreInput && isDigit(ctx.nextChar)) {
                val d = ctx.nextChar.asDigit
                ctx.fastUncheckedConsumeChars(1)
                go(ctx, x * base + d, false)
            }
            else if (first) None
            else Some(x)
        }
        go
    }
    protected final val decimal = subDecimal(10, _.isDigit)
    protected final val octal = subDecimal(8, parsley.Char.isOctDigit)
    protected final val hexadecimal = subDecimal(16, parsley.Char.isHexDigit)
}

private [internal] final class TokenNatural(_expected: UnsafeOption[String]) extends Instr with NumericReader {
    val expected = if (_expected == null) "natural" else _expected
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == '0') {
            ctx.fastUncheckedConsumeChars(1)
            lazy val hexa = ctx.nextChar == 'x' || ctx.nextChar == 'X'
            lazy val octa = ctx.nextChar == 'o' || ctx.nextChar == 'O'
            if (ctx.moreInput && (hexa || octa)) {
                ctx.fastUncheckedConsumeChars(1)
                (if (hexa) hexadecimal else octal)(ctx, 0, true) match {
                    case Some(x) => ctx.pushAndContinue(x)
                    case None => ctx.fail(expected)
                }
            }
            else ctx.pushAndContinue(decimal(ctx, 0, true).getOrElse(0))
        }
        else decimal(ctx, 0, true) match {
            case Some(x) => ctx.pushAndContinue(x)
            case None => ctx.fail(expected)
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenNatural"
    // $COVERAGE-ON$
}

private [internal] final class TokenFloat(_expected: UnsafeOption[String]) extends Instr {
    val expected = if (_expected == null) "unsigned float" else _expected
    override def apply(ctx: Context): Unit = {
        val builder = new StringBuilder()
        if (decimal(ctx, builder)) {
            lexFraction(ctx, builder)
        }
        else ctx.fail(expected)
    }

    @tailrec private final def decimal(ctx: Context, builder: StringBuilder, first: Boolean = true): Boolean = {
        if (ctx.moreInput && ctx.nextChar >= '0' && ctx.nextChar <= '9') {
            builder += ctx.nextChar
            ctx.fastUncheckedConsumeChars(1)
            decimal(ctx, builder, false)
        }
        else !first
    }

    private final def exponent(ctx: Context, builder: StringBuilder): Boolean = {
        ctx.fastUncheckedConsumeChars(1)
        if (ctx.moreInput && ctx.nextChar == '+') ctx.fastUncheckedConsumeChars(1)
        else if (ctx.moreInput && ctx.nextChar == '-') {
            ctx.fastUncheckedConsumeChars(1)
            builder += '-'
        }
        decimal(ctx, builder)
    }

    private final def attemptCastAndContinue(ctx: Context, builder: StringBuilder): Unit = {
        try ctx.pushAndContinue(builder.toString.toDouble)
        catch {
            case _: NumberFormatException => ctx.fail(expected)
        }
    }

    private final def lexExponent(ctx: Context, builder: StringBuilder, missingOk: Boolean): Unit = {
        val requireExponent = ctx.moreInput && (ctx.nextChar == 'e' || ctx.nextChar == 'E')
        if (requireExponent && exponent(ctx, builder += 'e')) attemptCastAndContinue(ctx, builder)
        else if (requireExponent) ctx.fail(expected)
        else if (missingOk) attemptCastAndContinue(ctx, builder)
        else ctx.fail(expected)
    }

    private final def lexFraction(ctx: Context, builder: StringBuilder) = {
        if (ctx.moreInput && ctx.nextChar == '.') {
            ctx.fastUncheckedConsumeChars(1)
            if (decimal(ctx, builder += '.')) lexExponent(ctx, builder, missingOk = true)
            else ctx.fail(expected)
        }
        else lexExponent(ctx, builder, missingOk = false)
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenFloat"
    // $COVERAGE-ON$
}