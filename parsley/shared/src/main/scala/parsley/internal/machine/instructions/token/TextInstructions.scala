/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import scala.annotation.tailrec

import parsley.character.{isHexDigit, isOctDigit}

import parsley.internal.collection.immutable.Trie
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.{EmptyError, MultiExpectedError}
import parsley.internal.machine.instructions.Instr
import parsley.token.errors.SpecialisedFilterConfig

private [internal] final class EscapeMapped(escTrie: Trie[Int], caretWidth: Int, expecteds: Set[ExpectItem]) extends Instr {
    def this(escTrie: Trie[Int], escs: Set[String]) = this(escTrie, escs.view.map(_.length).max, escs.map(ExpectRaw(_)))
    // Do not consume input on failure, it's possible another escape sequence might share a lead
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        findFirst(ctx, 0, escTrie)
    }

    @tailrec private def findLongest(ctx: Context, off: Int, escs: Trie[Int], longestChar: Int, longestSz: Int): Unit = {
        val (nextLongestChar, nextLongestSz) = escs.get("") match {
            case Some(x) => (x, off)
            case None => (longestChar, longestSz)
        }
        lazy val escsNew = escs.suffixes(ctx.peekChar(off))
        if (ctx.moreInput(off + 1) && escsNew.nonEmpty) findLongest(ctx, off + 1, escsNew, nextLongestChar, nextLongestSz)
        else {
            ctx.fastUncheckedConsumeChars(nextLongestSz)
            ctx.pushAndContinue(nextLongestChar)
        }
    }

    @tailrec private def findFirst(ctx: Context, off: Int, escs: Trie[Int]): Unit = {
        lazy val escsNew = escs.suffixes(ctx.peekChar(off))
        val couldTryMore = ctx.moreInput(off + 1) && escsNew.nonEmpty
        escs.get("") match {
            case Some(x) if couldTryMore => findLongest(ctx, off + 1, escsNew, x, off)
            case Some(x) =>
                ctx.fastUncheckedConsumeChars(off)
                ctx.pushAndContinue(x)
            case None if couldTryMore => findFirst(ctx, off + 1, escsNew)
            case None => ctx.fail(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, caretWidth))
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeMapped"
    // $COVERAGE-ON$
}

// TODO: clean up!
private [machine] abstract class EscapeSomeNumber(n: Int, radix: Int) extends Instr {

    def noMoreDigits(ctx: Context, n: Int, num: BigInt, origOff: Int, origLine: Int, origCol: Int): Unit

    override def apply(ctx: Context): Unit = {
        assume(n > 0, "n cannot be zero for EscapeAtMost or EscapeExactly")
        val origOff = ctx.offset
        val origLine = ctx.line
        val origCol = ctx.col
        if (ctx.moreInput && pred(ctx.peekChar)) go(ctx, n - 1, ctx.consumeChar().asDigit, origOff, origLine, origCol)
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }

    private def go(ctx: Context, n: Int, num: BigInt, origOff: Int, origLine: Int, origCol: Int): Unit = {
        if (n > 0) {
            if (ctx.moreInput && pred(ctx.peekChar)) {
                go(ctx, n - 1, num * radix + ctx.consumeChar().asDigit, origOff, origLine, origCol)
            }
            else noMoreDigits(ctx, n, num, origOff, origLine, origCol)
        }
        else {
            assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
            ctx.pushAndContinue(num)
        }
    }

    protected val expected: Some[ExpectDesc] = radix match {
        case 10 => Some(ExpectDesc("digit"))
        case 16 => Some(ExpectDesc("hexadecimal digit"))
        case 8 => Some(ExpectDesc("octal digit"))
        case 2 => Some(ExpectDesc("bit"))
    }

    protected val pred: Char => Boolean = radix match {
        case 10 => _.isDigit
        case 16 => isHexDigit(_)
        case 8 => isOctDigit(_)
        case 2 => c => c == '0' || c == '1'
    }
}

private [internal] final class EscapeAtMost(n: Int, radix: Int) extends EscapeSomeNumber(n, radix) {
    override def noMoreDigits(ctx: Context, n: Int, num: BigInt, origOff: Int, origLine: Int, origCol: Int): Unit = {
        ctx.addHints(expected.toSet, unexpectedWidth = 1)
        ctx.pushAndContinue(num)
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeAtMost"
    // $COVERAGE-ON$
}

private [internal] final class EscapeExactly(n: Int, full: Int, radix: Int, inexactErr: SpecialisedFilterConfig[Int]) extends EscapeSomeNumber(n, radix) {
    override def noMoreDigits(ctx: Context, n: Int, num: BigInt, origOff: Int, origLine: Int, origCol: Int): Unit = {
        if (n == 0) {
            ctx.addHints(expected.toSet, unexpectedWidth = 1)
            ctx.pushAndContinue(num)
        } else {
            // will need the original state
            ctx.fail(inexactErr.mkError(origOff, origLine, origCol, ctx.offset - origOff, full - n))
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeAtMost"
    // $COVERAGE-ON$
}
