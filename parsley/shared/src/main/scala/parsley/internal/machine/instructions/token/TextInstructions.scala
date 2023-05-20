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
private [machine] abstract class EscapeSomeNumber(radix: Int) extends Instr {
    final def someNumber(ctx: Context, n: Int): EscapeSomeNumber.Result = {
        assume(n > 0, "n cannot be zero for EscapeAtMost or EscapeExactly")
        if (ctx.moreInput && pred(ctx.peekChar)) go(ctx, n - 1, ctx.consumeChar().asDigit) else EscapeSomeNumber.NoDigits
    }

    private def go(ctx: Context, n: Int, num: BigInt): EscapeSomeNumber.Result = {
        if (n > 0) {
            if (ctx.moreInput && pred(ctx.peekChar)) go(ctx, n - 1, num * radix + ctx.consumeChar().asDigit)
            else EscapeSomeNumber.NoMoreDigits(n, num)
        }
        else EscapeSomeNumber.Good(num)
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
private [token] object EscapeSomeNumber {
    sealed abstract class Result
    case class Good(num: BigInt) extends Result
    case class NoMoreDigits(remaining: Int, num: BigInt) extends Result
    case object NoDigits extends Result
}

private [internal] final class EscapeAtMost(n: Int, radix: Int) extends EscapeSomeNumber(radix) {
    override def apply(ctx: Context): Unit = someNumber(ctx, n) match {
        case EscapeSomeNumber.Good(num) =>
            assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
            ctx.pushAndContinue(num)
        case EscapeSomeNumber.NoDigits => ctx.expectedFail(expected, unexpectedWidth = 1)
        case EscapeSomeNumber.NoMoreDigits(_, num) =>
            ctx.addHints(expected.toSet, unexpectedWidth = 1)
            ctx.pushAndContinue(num)
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeAtMost"
    // $COVERAGE-ON$
}

private [internal] final class EscapeOneOfExactly(radix: Int, ns: List[Int], inexactErr: SpecialisedFilterConfig[Int]) extends EscapeSomeNumber(radix) {
    private val (m :: ms) = ns
    def apply(ctx: Context): Unit = {
        val origOff = ctx.offset
        val origLine = ctx.line
        val origCol = ctx.col
        someNumber(ctx, m) match {
            case EscapeSomeNumber.Good(num) =>
                assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
                ctx.stack.push(num)
                go(ctx, m, m, ms)
                ctx.inc()
            case EscapeSomeNumber.NoDigits => ctx.expectedFail(expected, unexpectedWidth = 1)
            case EscapeSomeNumber.NoMoreDigits(remaining, _) =>
                assume(remaining != 0, "cannot be left with 0 remaining digits and failed")
                ctx.fail(inexactErr.mkError(origOff, origLine, origCol, ctx.offset - origOff, m - remaining))
        }
    }

    private def fail(ctx: Context, digits: Int, origOff: Int, origLine: Int, origCol: Int) = {
        // To Cosmin: this can use the save point mechanism you have
        ctx.offset = origOff
        ctx.line = origLine
        ctx.col = origCol
        digits
    }

    def go(ctx: Context, digits: Int, m: Int, ns: List[Int]): Int = ns match {
        case Nil => digits
        case n :: ns =>
            val origOff = ctx.offset
            val origLine = ctx.line
            val origCol = ctx.col
            someNumber(ctx, n-m) match { // this is the only place where the failure can actually happen: go never fails
                case EscapeSomeNumber.Good(num) =>
                    assume(new EmptyError(ctx.offset, ctx.line, ctx.col, 0).isExpectedEmpty, "empty errors don't have expecteds, so don't effect hints")
                    ctx.stack.push(num)
                    val exp = go(ctx, n-m, n, ns)
                    val y = ctx.stack.pop[BigInt]()
                    val x = ctx.stack.peek[BigInt]
                    ctx.stack.exchange(x * BigInt(radix).pow(exp) + y) // digits is removed here, because it's been added before the get
                    exp + digits
                case EscapeSomeNumber.NoDigits =>
                    ctx.addHints(expected.toSet, unexpectedWidth = 1)
                    fail(ctx, digits, origOff, origLine, origCol)
                case EscapeSomeNumber.NoMoreDigits(remaining, _) =>
                    assume(remaining != 0, "cannot be left with 0 remaining digits and failed")
                    assume(inexactErr.mkError(origOff, origLine, origCol, ctx.offset - origOff, n - remaining).isExpectedEmpty,
                           "filter errors don't have expecteds, so don't effect hints")
                    fail(ctx, digits, origOff, origLine, origCol)
            }
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeOneOfExactly"
    // $COVERAGE-ON$
}
