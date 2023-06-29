/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons.token

import parsley.token.errors.SpecialisedFilterConfig

import parsley.internal.collection.immutable.Trie
import parsley.internal.deepembedding.backend.StrictParsley.InstrBuffer
import parsley.internal.deepembedding.frontend.LazyParsleyIVisitor
import parsley.internal.deepembedding.singletons.Singleton
import parsley.internal.machine.instructions

private [parsley] final class EscapeMapped(escTrie: Trie[Int], escs: Set[String]) extends Singleton[Int] {
    // $COVERAGE-OFF$
    override def pretty: String = "escapeMapped"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.token.EscapeMapped(escTrie, escs)
        if (!producesResults) instrs += instructions.Pop
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Int] = visitor.visit(this, context)(escTrie, escs)

    override private [parsley] def prettyName = pretty
}

private [parsley] final class EscapeAtMost(n: Int, radix: Int) extends Singleton[BigInt] {
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.token.EscapeAtMost(n, radix)
        if (!producesResults) instrs += instructions.Pop
    }
    // $COVERAGE-OFF$
    override def pretty: String = "escapeAtMost"
    // $COVERAGE-ON$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[BigInt] = visitor.visit(this, context)(n, radix)

    override private [parsley] def prettyName = pretty
}

private [parsley] final class EscapeOneOfExactly(radix: Int, ns: List[Int], inexactErr: SpecialisedFilterConfig[Int]) extends Singleton[BigInt] {
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.token.EscapeOneOfExactly(radix, ns, inexactErr)
        if (!producesResults) instrs += instructions.Pop
    }
    // $COVERAGE-OFF$
    override def pretty: String = "escapeOneOfExactly"
    // $COVERAGE-ON$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[BigInt] = visitor.visit(this, context)(radix, ns, inexactErr)

    override private [parsley] def prettyName = pretty
}
