/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley

private [token] final class LexemeString(string: String , ws: Parsley[_]) extends String {
    override lazy val unicode: Parsley[ScalaString] = lexeme(string.unicode)
    override lazy val ascii: Parsley[ScalaString] = lexeme(string.ascii)
    override lazy val extendedAscii: Parsley[ScalaString] = lexeme(string.extendedAscii)

    private def lexeme[A](p: Parsley[A]) = p <* ws
}
