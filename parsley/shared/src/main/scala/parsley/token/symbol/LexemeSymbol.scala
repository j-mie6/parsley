/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

import parsley.Parsley
import parsley.token.Lexeme
import parsley.token.errors.ErrorConfig

private [token] class LexemeSymbol(symbol: Symbol, lexeme: Lexeme, err: ErrorConfig) extends Symbol(err) {
    override def apply(name: String): Parsley[Unit] = lexeme(symbol.apply(name))
    override def apply(name: Char): Parsley[Unit] = lexeme(symbol.apply(name))
    override def softKeyword(name: String): Parsley[Unit] = lexeme(symbol.softKeyword(name))
    override def softOperator(name: String): Parsley[Unit] = lexeme(symbol.softOperator(name))
}
