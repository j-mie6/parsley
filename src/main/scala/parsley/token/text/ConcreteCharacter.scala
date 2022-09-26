/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley
import parsley.character.satisfy
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.text.TextDesc

private [token] final class ConcreteCharacter(desc: TextDesc, escapes: Escape) extends Character {
    private val quote = desc.characterLiteralEnd
    private lazy val charLetter = Character.letter(quote, desc.escapeSequences.escBegin, allowsAllSpace = false, desc.graphicCharacter)

    override lazy val unicode: Parsley[Int] = {
        quote *> (escapes.escapeChar <|> charLetter.toUnicode) <* quote
    }

    override lazy val basicMultilingualPlane: Parsley[Char] = quote *> (escapes.escapeChar.collectMsg("non-BMP character") {
        case n if Character.isBmpCodePoint(n) => n.toChar
    } <|> charLetter.toBmp) <* quote

    // FIXME: These are going to be a dodgy because of the double check here, may reference BMP
    override lazy val ascii: Parsley[Char] = basicMultilingualPlane.filterOut {
        case n if n > Character.MaxAscii => "non-ascii character"
    }
    override lazy val extendedAscii: Parsley[Char] = basicMultilingualPlane.filterOut {
        case n if n > Character.MaxExtendedAscii => "non-ascii character (extended)"
    }
}
