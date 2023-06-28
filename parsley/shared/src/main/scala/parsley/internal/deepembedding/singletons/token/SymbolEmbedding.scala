/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons.token

import parsley.token.errors.LabelConfig
import parsley.token.predicate.CharPredicate
import parsley.internal.collection.immutable.Trie
import parsley.internal.deepembedding.frontend.LazyParsleyIVisitor
import parsley.internal.deepembedding.singletons.Singleton
import parsley.internal.machine.instructions

private [parsley] final class SoftKeyword(private [SoftKeyword] val specific: String, letter: CharPredicate, val caseSensitive: Boolean,
                                          val expected: LabelConfig, expectedEnd: String) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"softKeyword($specific)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.token.SoftKeyword(specific, letter, caseSensitive, expected, expectedEnd)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)(specific, letter, caseSensitive, expected, expectedEnd)
}

private [parsley] final class SoftOperator(private [SoftOperator] val specific: String, letter: CharPredicate, ops: Trie[Unit],
                                           val expected: LabelConfig, expectedEnd: String) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"softOperator($specific)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.token.SoftOperator(specific, letter, ops, expected, expectedEnd)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)(specific, letter, ops, expected, expectedEnd)
}

// $COVERAGE-OFF$
private [deepembedding] object SoftKeyword {
    def unapply(self: SoftKeyword): Some[String] = Some(self.specific)
}
private [deepembedding] object SoftOperator {
    def unapply(self: SoftOperator): Some[String] = Some(self.specific)
}
// $COVERAGE-ON$
