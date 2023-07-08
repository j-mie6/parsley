/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import parsley.Parsley
import parsley.errors.combinator.markAsToken

private [token] object LexemeImpl {
    implicit def fromSpace(spaces: Parsley[_]): Lexeme = new Lexeme {
        def apply[A](p: Parsley[A]): Parsley[A] = markAsToken(p) <* spaces
    }
    val empty = new Lexeme {
        def apply[A](p: Parsley[A]): Parsley[A] = markAsToken(p)
    }
}
