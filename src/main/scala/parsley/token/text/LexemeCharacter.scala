/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley

private [token] final class LexemeCharacter(character: Character, ws: Parsley[_]) extends Character {
    override lazy val unicode: Parsley[Int] = lexeme(character.unicode)
    override lazy val basicMultilingualPlane: Parsley[Char] = lexeme(character.basicMultilingualPlane)
    override lazy val ascii: Parsley[Char] = lexeme(character.ascii)
    override lazy val extendedAscii: Parsley[Char] = lexeme(character.extendedAscii)

    private def lexeme[A](p: Parsley[A]) = p <* ws
}
