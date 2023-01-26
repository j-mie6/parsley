/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import parsley.token.errors.LabelConfig
import parsley.token.predicate

import parsley.internal.errors.ExpectDesc
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.instructions.Instr

private [internal] final class SoftKeyword(
        specific: String, letter: CharPredicate, caseSensitive: Boolean, expected: Option[ExpectDesc], expectedEnd: Option[ExpectDesc]) extends Instr {
    def this(specific: String, letter: predicate.CharPredicate, caseSensitive: Boolean, expected: LabelConfig, expectedEnd: String) = {
        this(if (caseSensitive) specific else specific.toLowerCase,
             letter.asInternalPredicate,
             caseSensitive,
             expected.asExpectDesc, Some(new ExpectDesc(expectedEnd)))
    }

    private [this] final val strsz = specific.length
    private [this] final val numCodePoints = specific.codePointCount(0, strsz)

    final override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput(strsz)) {
            ctx.saveState()
            readSpecific(ctx, 0)
        }
        else ctx.expectedFail(expected, numCodePoints)
    }

    private def postprocess(ctx: Context): Unit = {
        if (letter.peek(ctx)) {
            ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    val readCharCaseHandledBMP = {
        if (caseSensitive) (ctx: Context) => ctx.peekChar
        else (ctx: Context) => ctx.peekChar.toLower
    }

    val readCharCaseHandledSupplementary = {
        if (caseSensitive) (ctx: Context) => Character.toCodePoint(ctx.peekChar(0), ctx.peekChar(1))
        else (ctx: Context) => Character.toLowerCase(Character.toCodePoint(ctx.peekChar(0), ctx.peekChar(1)))
    }

    final private def readSpecific(ctx: Context, j: Int): Unit = {
        if (j < strsz) {
            val c = specific.codePointAt(j)
            if (Character.isSupplementaryCodePoint(c) && ctx.moreInput(2) && readCharCaseHandledSupplementary(ctx) == c) {
                ctx.fastConsumeSupplementaryChar()
                readSpecific(ctx, j + 2)
            }
            else if (ctx.moreInput && readCharCaseHandledBMP(ctx) == c.toChar) {
                ctx.consumeChar()
                readSpecific(ctx, j + 1)
            }
            else {
                ctx.restoreState()
                ctx.expectedFail(expected, numCodePoints)
            }
        }
        else postprocess(ctx)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SoftKeyword($specific)"
    // $COVERAGE-ON$
}
