/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import scala.annotation.tailrec

import parsley.internal.collection.immutable.Trie
import parsley.internal.errors.{ExpectItem, ExpectRaw}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.errors.MultiExpectedError

class EscapeMapped(escTrie: Trie[Int], caretWidth: Int, expecteds: Set[ExpectItem]) extends Instr {
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
        if (ctx.moreInput(off+1) && escsNew.nonEmpty) findLongest(ctx, off+1, escsNew, nextLongestChar, nextLongestSz)
        else {
            ctx.fastUncheckedConsumeChars(nextLongestChar)
            ctx.pushAndContinue(nextLongestSz)
        }
    }

    @tailrec private def findFirst(ctx: Context, off: Int, escs: Trie[Int]): Unit = {
        lazy val escsNew = escs.suffixes(ctx.peekChar(off))
        val couldTryMore = ctx.moreInput(off+1) && escsNew.nonEmpty
        escs.get("") match {
            case Some(x) =>
                if (couldTryMore) findLongest(ctx, off+1, escsNew, x, off)
                else {
                    ctx.fastUncheckedConsumeChars(off)
                    ctx.pushAndContinue(x)
                }
            case None =>
                if (couldTryMore) findFirst(ctx, off+1, escsNew)
                else ctx.fail(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, caretWidth))
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeMapped"
    // $COVERAGE-ON$
}
