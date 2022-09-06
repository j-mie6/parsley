/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.empty
import parsley.character.{satisfy, char}
import parsley.combinator.skipSome
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.text.EscapeDesc

private [token] abstract class StringCharacter {
    def apply(isLetter: Char => Boolean): Parsley[Option[Int]]
}

private [token] object RawCharacter extends StringCharacter {
    override def apply(isLetter: Char => Boolean): Parsley[Option[Int]] = satisfy(isLetter).map(c => Some(c.toInt)).label("string character")
}

private [token] class EscapableCharacter(desc: EscapeDesc, escapes: Escape, space: Parsley[_]) extends StringCharacter {
    private lazy val escapeEmpty = desc.emptyEscape.fold[Parsley[Char]](empty)(char)
    private lazy val escapeGap = {
        if (desc.gapsSupported) skipSome(space.label("string gap")) *> desc.escBegin.label("end of string gap")
        else empty
    }
    private lazy val stringEscape: Parsley[Option[Int]] = {
        desc.escBegin *> (escapeGap #> None
                      <|> escapeEmpty #> None
                      <|> escapes.escapeCode.map(Some(_)).explain("invalid escape sequence"))
    }

    override def apply(isLetter: Char => Boolean): Parsley[Option[Int]] =
        (satisfy(c => isLetter(c) && c != desc.escBegin).map(c => Some(c.toInt)) <|> stringEscape).label("string character")
}
