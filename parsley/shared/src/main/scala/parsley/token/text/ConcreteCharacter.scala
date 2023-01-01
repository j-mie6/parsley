/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley
import parsley.character.char
import parsley.errors.combinator.ErrorMethods
import parsley.token.descriptions.text.TextDesc
import parsley.token.errors.ErrorConfig

private [token] final class ConcreteCharacter(desc: TextDesc, escapes: Escape, err: ErrorConfig) extends Character {
    private val quote = char(desc.characterLiteralEnd)
    private lazy val graphic = Character.letter(desc.characterLiteralEnd, desc.escapeSequences.escBegin, allowsAllSpace = false, desc.graphicCharacter)

    private def charLetter(graphicLetter: Parsley[Int]) = escapes.escapeChar <|> ErrorConfig.label(err.labelGraphicCharacter)(graphicLetter)
    private def charLiteral[A](letter: Parsley[A]) = quote *> letter <* quote

    override lazy val fullUtf16: Parsley[Int] = charLiteral(charLetter(graphic.toUnicode))
    // this is a bit inefficient, converting to int and then back to char, but it makes it consistent, and can be optimised anyway
    private lazy val uncheckedBmpLetter = charLetter(graphic.toBmp.map(_.toInt))

    private def constrainedBmp(illegal: PartialFunction[Int, ScalaString]) = charLiteral(uncheckedBmpLetter.filterOut(illegal).map(_.toChar))

    override lazy val basicMultilingualPlane: Parsley[Char] = constrainedBmp {
        case n if !Character.isBmpCodePoint(n) => err.explainCharNonBasicMultilingualPlane(n)
    }
    override lazy val ascii: Parsley[Char] = constrainedBmp {
        case n if n > Character.MaxAscii => err.explainCharNonAscii(n)
    }
    override lazy val latin1: Parsley[Char] = constrainedBmp {
        case n if n > Character.MaxLatin1 => err.explainCharNonLatin1(n)
    }
}
