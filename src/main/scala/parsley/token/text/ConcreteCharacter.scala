/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley
import parsley.character.satisfy
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.TextDesc

private [token] final class ConcreteCharacter(desc: TextDesc, escapes: Escape) extends Character {
    private lazy val charLetter = Character.letter('\'', desc.escapeChars.escBegin, allowsAllSpace = false, desc.graphicCharacter)

    override lazy val unicode: Parsley[Int] = {
        assume(!'\''.isLowSurrogate, "quotes are not low surrogates")
        '\'' *> ((escapes.escapeChar <* '\'') <|> (charLetter <~> (charLetter <* '\'' <|> '\'')).collect {
            case (c, '\'') => c.toInt
            case (high, low) if Character.isSurrogatePair(high, low) => Character.toCodePoint(high, low)
        })
    }

    override lazy val basicMultilingualPlane: Parsley[Char] = unicode.collectMsg("non-BMP character") {
        case n if Character.isBmpCodePoint(n) => n.toChar
    }

    override lazy val ascii: Parsley[Char] = unicode.collectMsg("non-ascii character") {
            case n if n <= Character.MaxAscii => n.toChar
    }

    override lazy val extendedAscii: Parsley[Char] = unicode.collectMsg("non-ascii character (extended)") {
            case n if n <= Character.MaxExtendedAscii => n.toChar
    }
}
