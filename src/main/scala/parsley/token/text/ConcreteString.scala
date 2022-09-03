/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.{fresh, pure}
import parsley.combinator.between
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.token.descriptions.text.TextDesc

private [token] final class ConcreteString(desc: TextDesc, stringChar: StringCharacter) extends String {
    override lazy val unicode: Parsley[ScalaString] = {
        val pf = pure { (sb: StringBuilder, cpo: Option[Int]) =>
            for (cp <- cpo) sb ++= Character.toChars(cp)
            sb
        }
        val content = parsley.expr.infix.secretLeft1(fresh(new StringBuilder), strChar, pf).map(_.toString)
        between('"'.label("string"), '"'.label("end of string"), content)
    }
    override lazy val ascii: Parsley[ScalaString] = String.ensureAscii(unicode)
    override lazy val extendedAscii: Parsley[ScalaString] = String.ensureExtendedAscii(unicode)

    private lazy val strChar: Parsley[Option[Int]] =
        stringChar(Character.letter('"', desc.escapeChars.escBegin, allowsAllSpace = false, desc.graphicCharacter))
}
