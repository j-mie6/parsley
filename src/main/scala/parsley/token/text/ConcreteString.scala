/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.{empty, fresh, pure}
import parsley.character.{satisfy, char}
import parsley.combinator.{between, skipSome}
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.token.descriptions.TextDesc

private [token] final class ConcreteString(desc: TextDesc, escapes: Escape, space: Parsley[_]) extends String {
    override lazy val unicode: Parsley[ScalaString] = {
        val pf = pure[(StringBuilder, Option[Int]) => StringBuilder] { (sb, cpo) =>
            for (cp <- cpo) sb ++= Character.toChars(cp)
            sb
        }
        val content = parsley.expr.infix.secretLeft1(fresh(new StringBuilder), stringChar, pf).map(_.toString)
        between('"'.label("string"), '"'.label("end of string"), content)
    }
    override lazy val ascii: Parsley[ScalaString] = amend {
        entrench(unicode).guardAgainst {
            case str if !String.isAscii(str) => Seq("non-ascii characters in string literal, this is not allowed")
       }
    }
    override lazy val extendedAscii: Parsley[ScalaString] = amend {
        entrench(unicode).guardAgainst {
            case str if !String.isExtendedAscii(str) => Seq("non-extended-ascii characters in string literal, this is not allowed")
       }
    }

    private def letter(terminal: Char): Parsley[Char] = satisfy(c => c != terminal && c != '\\' && c > '\u0016') // 0x16 is arbitrary, configure

    private val escapeEmpty = desc.escapeChars.emptyEscape.fold[Parsley[Char]](empty)(char)
    private lazy val escapeGap = {
        if (desc.escapeChars.gapsSupported) skipSome(space.label("string gap")) *> '\\'.label("end of string gap")
        else empty
    }
    private lazy val stringLetter = letter('"')
    private lazy val stringEscape: Parsley[Option[Int]] = {
        '\\' *> (escapeGap #> None
                <|> escapeEmpty #> None
                <|> escapes.escapeCode.map(Some(_)).explain("invalid escape sequence"))
    }
    private lazy val stringChar: Parsley[Option[Int]] = ((stringLetter.map(c => Some(c.toInt))) <|> stringEscape).label("string character")
}
