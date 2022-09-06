/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.{fresh, pure, notFollowedBy}
import parsley.combinator.{choice, between, skipMany, skipManyUntil}
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.{charLift, stringLift}

private [token] final class ConcreteString(ends: Set[ScalaString], stringChar: StringCharacter, isGraphic: Char => Boolean, allowsAllSpace: Boolean) extends String {
    override lazy val unicode: Parsley[ScalaString] = choice(ends.view.map(makeStringParser).toSeq: _*) *> sbReg.gets(_.toString)
    override lazy val ascii: Parsley[ScalaString] = String.ensureAscii(unicode)
    override lazy val extendedAscii: Parsley[ScalaString] = String.ensureExtendedAscii(unicode)

    val sbReg = parsley.registers.Reg.make[StringBuilder]

    def makeStringParser(terminal: ScalaString): Parsley[_] = {
        val terminalInit = terminal.charAt(0)
        val strChar = stringChar(Character.letter(terminalInit, allowsAllSpace, isGraphic))
        val pf = pure { (sb: StringBuilder, cpo: Option[Int]) =>
            for (cp <- cpo) sb ++= Character.toChars(cp)
            sb
        }
        val content = parsley.expr.infix.secretLeft1(sbReg.get, strChar, pf)
        // terminal should be first, to allow for a jump table on the main choice
        terminal *>
        // then only one string builder needs allocation
        sbReg.put(fresh(new StringBuilder)) *>
        skipManyUntil(sbReg.modify(terminalInit #> ((sb: StringBuilder) => sb += terminalInit)) <|> content, terminal)
    }
}
