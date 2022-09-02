/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.empty
import parsley.character.char
import parsley.combinator.skipSome
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.TextDesc

// TODO: this should probably take the isGraphic predicate in instead
// this way, the rawness of the character can be handled at the application site and not need a RawString class.
private [token] abstract class StringCharacter {
    def apply(letter: Parsley[Char]): Parsley[Option[Int]]
}

private [token] object RawCharacter extends StringCharacter {
    override def apply(letter: Parsley[Char]): Parsley[Option[Int]] = letter.map(c => Some(c.toInt))
}

private [token] class EscapableCharacter(desc: TextDesc, escapes: Escape, space: Parsley[_]) extends StringCharacter {
    private val escapeEmpty = desc.escapeChars.emptyEscape.fold[Parsley[Char]](empty)(char)
    private lazy val escapeGap = {
        if (desc.escapeChars.gapsSupported) skipSome(space.label("string gap")) *> '\\'.label("end of string gap")
        else empty
    }
    private lazy val stringEscape: Parsley[Option[Int]] = {
        '\\' *> (escapeGap #> None
             <|> escapeEmpty #> None
             <|> escapes.escapeCode.map(Some(_)).explain("invalid escape sequence"))
    }

    override def apply(letter: Parsley[Char]): Parsley[Option[Int]] =
        (letter.map(c => Some(c.toInt)) <|> stringEscape).label("string character")
}
