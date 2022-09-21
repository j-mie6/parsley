/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

import parsley.Parsley

private [token] class LexemeSymbol(symbol: Symbol, ws: Parsley[_]) extends Symbol {

    override def apply(name: String): Parsley[Unit] = lexeme(symbol.apply(name))

    override def apply(name: Char): Parsley[Unit] = lexeme(symbol.apply(name))

    override def softKeyword(name: String): Parsley[Unit] = lexeme(symbol.softKeyword(name))

    override def operator(name: String): Parsley[Unit] = lexeme(symbol.operator(name))

    override def maxOp(name: String): Parsley[Unit] = lexeme(symbol.maxOp(name))

    private def lexeme[A](p: Parsley[A]) = p <* ws
}
