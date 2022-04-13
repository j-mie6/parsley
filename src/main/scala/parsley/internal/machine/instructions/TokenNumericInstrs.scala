package parsley.internal.machine.instructions

import scala.annotation.tailrec

import parsley.character

import parsley.internal.deepembedding.Sign.{DoubleType, IntType, SignType}
import parsley.internal.errors.{Desc, ErrorItem}
import parsley.internal.machine.Context

private [internal] final class TokenSign(ty: SignType) extends Instr {
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
    protected final val octal = subDecimal(8, character.isOctDigit)
    protected final val hexadecimal = subDecimal(16, character.isHexDigit)
}

private [internal] object TokenNatural extends Instr with NumericReader {
    private [this] final val expected = Some(Desc("natural"))
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == '0') {
            ctx.fastUncheckedConsumeChars(1)
            lazy val hexa = ctx.nextChar == 'x' || ctx.nextChar == 'X'
            lazy val octa = ctx.nextChar == 'o' || ctx.nextChar == 'O'
            if (ctx.moreInput && (hexa || octa)) {
                ctx.fastUncheckedConsumeChars(1)
                (if (hexa) hexadecimal else octal)(ctx, 0, true) match {
                    case Some(x) => ctx.pushAndContinue(x)
                    case None => ctx.expectedFail(expected)
                }
            }
            else ctx.pushAndContinue(decimal(ctx, 0, true).getOrElse(0))
        }
        else decimal(ctx, 0, true) match {
            case Some(x) => ctx.pushAndContinue(x)
            case None => ctx.expectedFail(expected)
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenNatural"
    // $COVERAGE-ON$
}

private [internal] object TokenFloat extends Instr {
    private [this] final val expected = Some(Desc("unsigned float"))
    override def apply(ctx: Context): Unit = {
        val initialOffset = ctx.offset
        if (decimal(ctx)) {
            lexFraction(ctx, initialOffset)
        }
        else ctx.expectedFail(expected)
    }

    @tailrec private final def decimal(ctx: Context, first: Boolean = true): Boolean = {
        if (ctx.moreInput && ctx.nextChar >= '0' && ctx.nextChar <= '9') {
            ctx.fastUncheckedConsumeChars(1)
            decimal(ctx, false)
        }
        else !first
    }

    private final def exponent(ctx: Context): Boolean = {
        ctx.fastUncheckedConsumeChars(1)
        if (ctx.moreInput && ctx.nextChar == '+') ctx.fastUncheckedConsumeChars(1)
        else if (ctx.moreInput && ctx.nextChar == '-') {
            ctx.fastUncheckedConsumeChars(1)
        }
        decimal(ctx)
    }

    private final def attemptCastAndContinue(ctx: Context, initialOffset: Int): Unit = {
        try ctx.pushAndContinue(ctx.input.substring(initialOffset, ctx.offset).toDouble)
        catch {
            case _: NumberFormatException => ctx.expectedFail(expected)
        }
    }

    private final def lexExponent(ctx: Context, initialOffset: Int, missingOk: Boolean): Unit = {
        val requireExponent = ctx.moreInput && (ctx.nextChar == 'e' || ctx.nextChar == 'E')
        if (requireExponent && exponent(ctx)) attemptCastAndContinue(ctx, initialOffset)
        else if (requireExponent) ctx.expectedFail(expected)
        else if (missingOk) attemptCastAndContinue(ctx, initialOffset)
        else ctx.expectedFail(expected)
    }

    private final def lexFraction(ctx: Context, initialOffset: Int) = {
        if (ctx.moreInput && ctx.nextChar == '.') {
            ctx.fastUncheckedConsumeChars(1)
            if (decimal(ctx)) lexExponent(ctx, initialOffset, missingOk = true)
            else ctx.expectedFail(expected)
        }
        else lexExponent(ctx, initialOffset, missingOk = false)
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenFloat"
    // $COVERAGE-ON$
}
