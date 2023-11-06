/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley

// Sadly, no reflective capabilities exist in Scala 3 yet, so these
// methods don't do anything yet.
// TODO: Find a substitute for reflection for Scala 3.
private [parsley] object XCollector extends CollectorImpl {
    override def collectNames(obj: Any): Map[LazyParsley[_], String] = Map.empty

    override def collectLexer(lexer: Lexer): Map[LazyParsley[_], String] = Map.empty

    // This current implementation of a collector does nothing for Scala 3, as no automatic
    // substitute for reflection has been found.
    override val supported: Boolean = false
}
