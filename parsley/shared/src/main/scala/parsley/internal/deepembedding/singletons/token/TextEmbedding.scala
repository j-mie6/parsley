/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons.token

import parsley.internal.collection.immutable.Trie
import parsley.internal.deepembedding.singletons.Singleton
import parsley.internal.machine.instructions
import parsley.token.errors.SpecialisedFilterConfig

private [parsley] final class EscapeMapped(escTrie: Trie[Int], escs: Set[String]) extends Singleton[Int] {
    // $COVERAGE-OFF$
    override def pretty: String = "escapeMapped"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.token.EscapeMapped(escTrie, escs)
}

private [parsley] final class EscapeAtMost(n: Int, radix: Int) extends Singleton[BigInt] {
    override def instr: instructions.Instr = new instructions.token.EscapeAtMost(n, radix)
    // $COVERAGE-OFF$
    override def pretty: String = "escapeAtMost"
    // $COVERAGE-ON$
}

private [parsley] final class EscapeExactly(n: Int, full: Int, radix: Int, inexactErr: SpecialisedFilterConfig[Int]) extends Singleton[BigInt] {
    override def instr: instructions.Instr = new instructions.token.EscapeExactly(n, full, radix, inexactErr)
    // $COVERAGE-OFF$
    override def pretty: String = "escapeExactly"
    // $COVERAGE-ON$
}

private [parsley] final class EscapeOneOfExactly(radix: Int, ns: List[Int], inexactErr: SpecialisedFilterConfig[Int]) extends Singleton[BigInt] {
    override def instr: instructions.Instr = new instructions.token.EscapeOneOfExactly(radix, ns, inexactErr)
    // $COVERAGE-OFF$
    override def pretty: String = "escapeOneOfExactly"
    // $COVERAGE-ON$
}
