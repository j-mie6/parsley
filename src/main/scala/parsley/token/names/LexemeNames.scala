/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley

private [token] class LexemeNames(names: Names, ws: Parsley[_]) extends Names {
    override lazy val identifier: Parsley[String] = lexeme(names.identifier)
    override lazy val userDefinedOperator: Parsley[String] = lexeme(names.userDefinedOperator)
    override def userDefinedOperator(startChar: Option[Char], endChar: Option[Char]): Parsley[String] = lexeme(names.userDefinedOperator(startChar, endChar))

    private def lexeme[A](p: Parsley[A]) = p <* ws
}
