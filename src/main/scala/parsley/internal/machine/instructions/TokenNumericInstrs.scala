/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec

import parsley.character
import parsley.token.descriptions.Presence

import parsley.internal.deepembedding.Sign.{CombinedType, DoubleType, IntType, SignType}
import parsley.internal.errors.{Desc, ExpectItem, ExpectRaw}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.MultiExpectedError

private [internal] final class TokenSign(ty: SignType, plusPresence: Presence) extends Instr {
    val neg: Any => Any = ty match {
        case IntType => ((x: IntType.resultType) => -x).asInstanceOf[Any => Any]
        case DoubleType => ((x: DoubleType.resultType) => -x).asInstanceOf[Any => Any]
        case CombinedType => ((x: CombinedType.resultType) => x.fold(n => Left(-n), n => Right(-n))).asInstanceOf[Any => Any]
    }
    val pos = (x: Any) => x

    private [this] val expecteds: Set[ExpectItem] =
        if (plusPresence ne Presence.Illegal) Set(new ExpectRaw("+"), new ExpectRaw("-"))
        else                                  Set(new ExpectRaw("-"))

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // This could be simplified, but the "fail" branches need to be duplicated...
        if (ctx.moreInput && ctx.nextChar == '-') {
            ctx.fastUncheckedConsumeChars(1)
            ctx.pushAndContinue(neg)
        }
        else if ((plusPresence ne Presence.Illegal) && ctx.moreInput && ctx.nextChar == '+') {
            ctx.fastUncheckedConsumeChars(1)
            ctx.pushAndContinue(pos)
        }
        else if (plusPresence eq Presence.Required) {
            ctx.fail(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, 1))
        }
        else {
            ctx.pushError(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, 1))
            ctx.addErrorToHintsAndPop()
            ctx.pushAndContinue(pos)
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenSign"
    // $COVERAGE-ON$
}

private [instructions] trait NumericReader {
    private final def subDecimal(base: Int, isDigit: Char => Boolean): (Context, BigInt, Boolean) => Option[BigInt] = {
        @tailrec def go(ctx: Context, x: BigInt, first: Boolean): Option[BigInt] = {
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
        ensureRegularInstruction(ctx)
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
        ensureRegularInstruction(ctx)
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
        try ctx.pushAndContinue(BigDecimal(ctx.input.substring(initialOffset, ctx.offset)))
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
