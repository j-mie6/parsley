/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => SString, _}
import parsley.{Parsley, ParsleyTest, Success, Failure}
import parsley.token.Lexeme

import parsley.token.descriptions.text._
import parsley.token.predicate._
import parsley.character.space

class StringTests extends ParsleyTest {
    private def makeString(desc: TextDesc, char: StringCharacter, spaceAllowed: Boolean) =
        new LexemeString(new ConcreteString(desc.stringEnds, char, desc.graphicCharacter, spaceAllowed),Lexeme.empty)
    private def makeString(desc: TextDesc): String =
        makeString(desc, new EscapableCharacter(desc.escapeSequences, new Escape(desc.escapeSequences), space), false)
    private def makeMultiString(desc: TextDesc): String =
        makeString(desc, new EscapableCharacter(desc.escapeSequences, new Escape(desc.escapeSequences), space), true)
    private def makeRawString(desc: TextDesc): String =
        makeString(desc, RawCharacter, false)
    private def makeRawMultiString(desc: TextDesc): String =
        makeString(desc, RawCharacter, true)

    def unicodeCases(str: String)(tests: (SString, Option[SString])*): Unit = cases(str.unicode)(tests: _*)
    def asciiCases(str: String)(tests: (SString, Option[SString])*): Unit = cases(str.ascii)(tests: _*)
    def extAsciiCases(str: String)(tests: (SString, Option[SString])*): Unit = cases(str.extendedAscii)(tests: _*)

    val plain = TextDesc.plain.copy(
        graphicCharacter = Unicode(_ >= ' '),
        escapeSequences = EscapeDesc.plain.copy(multiMap = Map("lf" -> '\n', "lam" -> 'λ', "pound" -> '£', "smile" -> 0x1F642 /*🙂*/)),
    )
    val plainStr = makeString(plain)
    val plainMultiStr = makeMultiString(plain)
    val plainRawStr = makeRawString(plain)
    val plainRawMultiStr = makeRawMultiString(plain)

    "string literals" should "be allowed to be empty" in unicodeCases(plainStr)(
        "\"\"" -> Some(""),
    )

    they should "be allowed to be non-empty" in unicodeCases(plainStr)(
        "\"hello world!\"" -> Some("hello world!"),
        "hello" -> None,
        "\"" -> None,
        "\"🙂🙂\"" -> Some("🙂🙂"),
        "\"🇬🇧\"" -> Some("🇬🇧"),
    )

    they should "allow for escape sequences" in unicodeCases(plainStr)(
        "\"\\smilea\"" -> Some("🙂a"),
    )

    /*they should "allow for different graphic characters" in asciiCases(makeString(plain.copy(graphicCharacter = Basic(Set('a', 'b')))))(
        "\"aab\"" -> Some("aab"),
        "\"abc\"" -> None,
    )*/

    they should "allow for change in literal end" in unicodeCases(makeString(plain.copy(stringEnds = Set("@@"))))(
        "@@@@" -> Some(""),
        "@@abc@@" -> Some("abc"),
        "@@" -> None,
        "\"\"" -> None,
    )

    they should "allow for string gaps when configured" in unicodeCases(makeString(plain.copy(escapeSequences = plain.escapeSequences.copy(gapsSupported = true))))(
        "\"hello \\          \\world!\"" -> Some("hello world!"),
        "\"hello \\ world!\"" -> None,
    )

    they should "allow for empty characters when configured" in unicodeCases(makeString(plain.copy(escapeSequences = plain.escapeSequences.copy(emptyEscape = Some('&')))))(
        "\"hello\\& world!\"" -> Some("hello world!"),
    )

    they should "allow for no graphical character" in unicodeCases(makeString(plain.copy(graphicCharacter = NotRequired)))(
        "\"a\"" -> None,
        "\"\\smile\\lf\"" -> Some("🙂\n"),
    )
}
