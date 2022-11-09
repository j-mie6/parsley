/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => SString, _}
import parsley.{Parsley, ParsleyTest, Success, Failure}
import parsley.token.Lexeme

import parsley.token.descriptions.text._
import parsley.token.predicate._

class CharacterTests extends ParsleyTest {
    def makeChar(desc: TextDesc): Character = new LexemeCharacter(new ConcreteCharacter(desc, new Escape(desc.escapeSequences)), Lexeme.empty)

    def unicodeCases(char: Character)(tests: (SString, Option[Int])*): Unit = cases(char.unicode)(tests: _*)
    def bmpCases(char: Character)(tests: (SString, Option[Char])*): Unit = cases(char.basicMultilingualPlane)(tests: _*)
    def asciiCases(char: Character)(tests: (SString, Option[Char])*): Unit = cases(char.ascii)(tests: _*)
    def extAsciiCases(char: Character)(tests: (SString, Option[Char])*): Unit = cases(char.extendedAscii)(tests: _*)

    def unicodeCases(desc: TextDesc)(tests: (SString, Option[Int])*): Unit = unicodeCases(makeChar(desc))(tests: _*)
    def bmpCases(desc: TextDesc)(tests: (SString, Option[Char])*): Unit = bmpCases(makeChar(desc))(tests: _*)
    def asciiCases(desc: TextDesc)(tests: (SString, Option[Char])*): Unit = asciiCases(makeChar(desc))(tests: _*)
    def extAsciiCases(desc: TextDesc)(tests: (SString, Option[Char])*): Unit = extAsciiCases(makeChar(desc))(tests: _*)

    val plain = TextDesc.plain.copy(
        graphicCharacter = Unicode(_ >= ' '),
        escapeSequences = EscapeDesc.plain.copy(multiMap = Map("lf" -> '\n', "lam" -> 'λ', "pound" -> '£', "smile" -> 0x1F642 /*🙂*/)),
    )
    val plainChar = makeChar(plain)

    "character literals" should "require end char" in bmpCases(plainChar)(
        "a" -> None,
        "'a'" -> Some('a'),
    )

    they should "parse any defined \"graphic character\"" in bmpCases(plain.copy(graphicCharacter = Basic(Set('a', 'b', 'c'))))(
        "'a'" -> Some('a'),
        "'b'" -> Some('b'),
        "'c'" -> Some('c'),
        "'d'" -> None,
    )

    they should "be able to parse escape sequences too" in bmpCases(plainChar)(
        "'a'" -> Some('a'),
        "'\\lf'" -> Some('\n'),
    )

    they should "not allow for string gaps or empty characters" in bmpCases(plain.copy(escapeSequences = EscapeDesc.plain.copy(gapsSupported = true, emptyEscape = Some('&'))))(
        "'\\ \\'" -> None,
        "'\\&'" -> None,
    )

    they should "be able to set the literal end char" in bmpCases(plain.copy(characterLiteralEnd = '@'))(
        "@a@" -> Some('a'),
    )

    they should "handle empty graphic chars, by parsing escape sequences" in bmpCases(plain.copy(graphicCharacter = NotRequired))(
        "'a'" -> None,
        "'\\lf'" -> Some('\n'),
    )

    "full utf-16 literals" should "parse any valid single code-point" in unicodeCases(plainChar)(
        "'🙂'" -> Some(0x1F642 /*🙂*/),
        "'a'" -> Some('a'),
        "'\\smile'" -> Some(0x1F642 /*🙂*/),
    )

    they should "not parse multi-code point characters" in unicodeCases(plainChar)(
        "'🇬🇧'" -> None, // flags are more than one code point big!
        "'aa'" -> None,
    )

    "basic multi-lingual plane literals" should "parse any valid bmp code-point" in bmpCases(plainChar)(
        "'λ'" -> Some('λ'),
        "' '" -> Some(' '),
        "'a'" -> Some('a'),
        "'\\lf'" -> Some('\n'),
        "'\\lam'" -> Some('λ'),
    )

    they should "not parse wider unicode, including from escape characters" in bmpCases(plainChar)(
        "'\\oops'" -> None,
        "'🙂'" -> None,
        "'🇬🇧'" -> None,
    )

    "extended-ascii literals" should "parse any valid extended ascii code-point" in extAsciiCases(plainChar)(
        "'a'" -> Some('a'),
        "'£'" -> Some('£'),
        "'\\pound'" -> Some('£'),
        "'\\lf'" -> Some('\n')
    )

    they should "not parse other utf-16 characters, including from escape sequences" in extAsciiCases(plainChar)(
        "'\\oops'" -> None,
        "'\\lam'" -> None,
        "'λ'" -> None,
        "'🙂'" -> None,
    )

    "ascii literals" should "parse any valid ascii code-point" in asciiCases(plainChar)(
        "'a'" -> Some('a'),
        "'\\lf'" -> Some('\n')
    )

    they should "not parse other utf-16 characters, including from escape sequences" in asciiCases(plainChar)(
        "'\\oops'" -> None,
        "'\\lam'" -> None,
        "'£'" -> None,
        "'\\pound'" -> None,
        "'λ'" -> None,
        "'🙂'" -> None,
    )
}
