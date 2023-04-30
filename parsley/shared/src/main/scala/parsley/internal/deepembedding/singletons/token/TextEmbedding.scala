/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons.token

import parsley.registers.Reg

import parsley.internal.collection.immutable.Trie
import parsley.internal.deepembedding.singletons.Singleton
import parsley.internal.machine.instructions
import parsley.internal.deepembedding.frontend.UsesRegister

private [parsley] final class EscapeMapped(escTrie: Trie[Int], escs: Set[String]) extends Singleton[Int] {
    // $COVERAGE-OFF$
    override def pretty: String = "escapeMapped"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.token.EscapeMapped(escTrie, escs)
}

private [parsley] final class EscapeAtMost(n: Int, radix: Int, val reg: Reg[Int]) extends Singleton[BigInt] with UsesRegister {
    override def instr: instructions.Instr = new instructions.token.EscapeAtMost(n, radix, reg.addr)
    // $COVERAGE-OFF$
    override def pretty: String = "escapeAtMost"
    // $COVERAGE-ON$
}
