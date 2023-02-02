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
    // We do need to backtrack out of this if things go wrong, it's possible another escape sequence might share a lead
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        findFirst(ctx, 0, escTrie)
    }

    @tailrec private def findLongest(ctx: Context, off: Int, escs: Trie[Int], bestGuess: Int, bestGuessSz: Int): Unit = {
        escs.get("") match {
            case Some(x) =>
                lazy val escsNew = escs.suffixes(ctx.peekChar(off))
                if (ctx.moreInput(off+1) && escsNew.nonEmpty) {
                    findLongest(ctx, off+1, escsNew, x, off)
                }
                else {
                    ctx.fastUncheckedConsumeChars(off)
                    ctx.pushAndContinue(x)
                }
            case None =>
                lazy val escsNew = escs.suffixes(ctx.peekChar(off))
                if (ctx.moreInput(off+1) && escsNew.nonEmpty) {
                    findLongest(ctx, off+1, escsNew, bestGuess, bestGuessSz)
                }
                else {
                    ctx.fastUncheckedConsumeChars(bestGuessSz)
                    ctx.pushAndContinue(bestGuess)
                }
        }
    }

    @tailrec private def findFirst(ctx: Context, off: Int, escs: Trie[Int]): Unit = {
        escs.get("") match {
            case Some(x) =>
                lazy val escsNew = escs.suffixes(ctx.peekChar(off))
                if (ctx.moreInput(off+1) && escsNew.nonEmpty) {
                    findLongest(ctx, off+1, escsNew, x, off)
                }
                else {
                    ctx.fastUncheckedConsumeChars(off)
                    ctx.pushAndContinue(x)
                }
            case None =>
                lazy val escsNew = escs.suffixes(ctx.peekChar(off))
                if (ctx.moreInput(off+1) && escsNew.nonEmpty) {
                    findFirst(ctx, off+1, escsNew)
                }
                else ctx.fail(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, caretWidth))
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "EscapeMapped"
    // $COVERAGE-ON$
}
