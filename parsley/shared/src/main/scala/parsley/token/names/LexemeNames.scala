/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley
import parsley.token.Lexeme
import parsley.token.predicate.CharPredicate

private [token] class LexemeNames(names: Names, lexeme: Lexeme) extends Names {
    override lazy val identifier: Parsley[String] = lexeme(names.identifier)
    override def identifier(startChar: CharPredicate): Parsley[String] = lexeme(names.identifier(startChar))
    override lazy val userDefinedOperator: Parsley[String] = lexeme(names.userDefinedOperator)
    override def userDefinedOperator(startChar: CharPredicate, endChar: CharPredicate): Parsley[String] = lexeme(names.userDefinedOperator(startChar, endChar))
}
