/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley
import parsley.token.Lexeme

private [token] final class LexemeString(string: String, lexeme: Lexeme) extends String {
    override lazy val fullUtf16: Parsley[ScalaString] = lexeme(string.fullUtf16)
    override lazy val ascii: Parsley[ScalaString] = lexeme(string.ascii)
    override lazy val latin1: Parsley[ScalaString] = lexeme(string.latin1)
}
