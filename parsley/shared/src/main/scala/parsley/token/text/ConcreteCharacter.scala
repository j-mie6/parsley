/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley
import parsley.character.char
import parsley.errors.combinator.ErrorMethods
import parsley.token.descriptions.text.TextDesc
import parsley.token.errors.ErrorConfig

private [token] final class ConcreteCharacter(desc: TextDesc, escapes: Escape, err: ErrorConfig) extends Character {
    private val quote = char(desc.characterLiteralEnd)
    private lazy val charLetter = Character.letter(desc.characterLiteralEnd, desc.escapeSequences.escBegin, allowsAllSpace = false, desc.graphicCharacter)

    override lazy val fullUtf16: Parsley[Int] = {
        quote *> (escapes.escapeChar <|> ErrorConfig.label(err.labelGraphicCharacter)(charLetter.toUnicode)) <* quote
    }

    override lazy val basicMultilingualPlane: Parsley[Char] = {
        quote *> (escapes.escapeChar.collectMsg(err.messageCharEscapeNonBasicMultilingualPlane(_)) {
            case n if Character.isBmpCodePoint(n) => n.toChar
        } <|> ErrorConfig.label(err.labelGraphicCharacter)(charLetter.toBmp)) <* quote
    }

    // FIXME: These are going to be a dodgy because of the double check here, may reference BMP
    override lazy val ascii: Parsley[Char] = basicMultilingualPlane.filterOut {
        case n if n > Character.MaxAscii => err.explainCharNonAscii(n.toInt)
    }
    override lazy val latin1: Parsley[Char] = basicMultilingualPlane.filterOut {
        case n if n > Character.MaxLatin1 => err.explainCharNonLatin1(n.toInt)
    }
}
