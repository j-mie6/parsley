/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import scala.annotation.tailrec

import parsley.internal.collection.immutable.Trie
import parsley.internal.errors.ExpectItem
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.errors.MultiExpectedError

class EscapeMapped(escTrie: Trie[Int]) extends Instr {
    private val caretWidth: Int = ???
    private val expecteds: Set[ExpectItem] = ???
    // We do need to backtrack out of this if things go wrong, it's possible another escape sequence might share a lead
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        go(ctx, 0, escTrie)
    }

    @tailrec private def go(ctx: Context, off: Int, escs: Trie[Int]): Unit = {
        escs.get("") match {
            case Some(x) =>
                ctx.fastUncheckedConsumeChars(off)
                ctx.pushAndContinue(x)
            case None =>
                lazy val escsNew = escs.suffixes(ctx.peekChar(off))
                if (ctx.moreInput(off+1) && escsNew.nonEmpty) {
                    go(ctx, off+1, escsNew)
                }
                else ctx.fail(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, expecteds, caretWidth))
        }
    }
}
