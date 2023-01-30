/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import scala.annotation.tailrec

import parsley.token.errors.LabelConfig
import parsley.token.predicate

import parsley.internal.errors.ExpectDesc
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.instructions.Instr
import parsley.internal.collection.immutable.Trie

private [token] abstract class Specific extends Instr {
    protected val specific: String
    protected val caseSensitive: Boolean
    protected val expected: Option[ExpectDesc]
    private [this] final val strsz = specific.length
    private [this] final val numCodePoints = specific.codePointCount(0, strsz)

    protected def postprocess(ctx: Context): Unit

    final override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput(strsz)) {
            ctx.saveState()
            readSpecific(ctx, 0)
        }
        else ctx.expectedFail(expected, numCodePoints)
    }

    private val readCharCaseHandledBMP = {
        if (caseSensitive) (ctx: Context) => ctx.peekChar
        else (ctx: Context) => ctx.peekChar.toLower
    }

    private val readCharCaseHandledSupplementary = {
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
}

private [internal] final class SoftKeyword(protected val specific: String, letter: CharPredicate, protected val caseSensitive: Boolean,
                                           protected val expected: Option[ExpectDesc], expectedEnd: Option[ExpectDesc]) extends Specific {
    def this(specific: String, letter: predicate.CharPredicate, caseSensitive: Boolean, expected: LabelConfig, expectedEnd: String) = {
        this(if (caseSensitive) specific else specific.toLowerCase,
             letter.asInternalPredicate,
             caseSensitive,
             expected.asExpectDesc, Some(new ExpectDesc(expectedEnd)))
    }

    protected def postprocess(ctx: Context): Unit = {
        if (letter.peek(ctx)) {
            ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SoftKeyword($specific)"
    // $COVERAGE-ON$
}

private [internal] final class SoftOperator(protected val specific: String, letter: CharPredicate, ops: Trie, protected val expected: Option[ExpectDesc], expectedEnd: Option[ExpectDesc]) extends Specific {
    def this(specific: String, letter: predicate.CharPredicate, ops: Trie, expected: LabelConfig, expectedEnd: String) = {
        this(specific, letter.asInternalPredicate, ops, expected.asExpectDesc, Some(new ExpectDesc(expectedEnd)))
    }
    protected val caseSensitive = true
    private val ends = ops.suffixes(specific)

    // returns true if an end could be parsed from this point
    @tailrec private def checkEnds(ctx: Context, ends: Trie, off: Int): Boolean = {
        if (ends.nonEmpty && ctx.moreInput(off + 1)) {
            val endsOfNext = ends.suffixes(ctx.peekChar(off))
            endsOfNext.contains("") || checkEnds(ctx, endsOfNext, off + 1)
        }
        else false
    }

    protected def postprocess(ctx: Context): Unit = {
        if (letter.peek(ctx) || checkEnds(ctx, ends, off = 0)) {
            ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"SoftOperator($specific)"
    // $COVERAGE-ON$
}
