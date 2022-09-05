/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.empty
import parsley.character.{satisfy, char}
import parsley.combinator.skipSome
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.text.{EscapeDesc, StringDesc}

// TODO: this should probably take the isGraphic predicate in instead
// this way, the rawness of the character can be handled at the application site and not need a RawString class.
private [token] abstract class StringCharacter {
    def apply(isLetter: Char => Boolean): Parsley[Option[Int]]
}

// TODO: This needs logic to check the desc.rawEscape
private [token] class RawCharacter(desc: StringDesc) extends StringCharacter {
    override def apply(isLetter: Char => Boolean): Parsley[Option[Int]] = satisfy(isLetter).map(c => Some(c.toInt)).label("string character")
}

private [token] class EscapableCharacter(desc: EscapeDesc, escapes: Escape, space: Parsley[_]) extends StringCharacter {
    private val escapeEmpty = desc.emptyEscape.fold[Parsley[Char]](empty)(char)
    private lazy val escapeGap = {
        if (desc.gapsSupported) skipSome(space.label("string gap")) *> '\\'.label("end of string gap")
        else empty
    }
    private lazy val stringEscape: Parsley[Option[Int]] = {
        '\\' *> (escapeGap #> None
             <|> escapeEmpty #> None
             <|> escapes.escapeCode.map(Some(_)).explain("invalid escape sequence"))
    }

    override def apply(isLetter: Char => Boolean): Parsley[Option[Int]] =
        (satisfy(c => isLetter(c) && c != desc.escBegin).map(c => Some(c.toInt)) <|> stringEscape).label("string character")
}
