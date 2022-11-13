/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.{fresh, pure, notFollowedBy}
import parsley.combinator.{between, choice, skipMany, skipManyUntil}
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.{charLift, stringLift}
import parsley.implicits.zipped.Zipped2
import parsley.token.predicate.CharPredicate

private [token] final class ConcreteString(ends: Set[ScalaString], stringChar: StringCharacter, isGraphic: CharPredicate, allowsAllSpace: Boolean)
    extends String {
    override lazy val fullUtf16: Parsley[ScalaString] = choice(ends.view.map(makeStringParser).toSeq: _*) *> sbReg.gets(_.toString)
    override lazy val ascii: Parsley[ScalaString] = String.ensureAscii(fullUtf16)
    override lazy val latin1: Parsley[ScalaString] = String.ensureExtendedAscii(fullUtf16)

    private val sbReg = parsley.registers.Reg.make[StringBuilder]

    private def makeStringParser(terminal: ScalaString): Parsley[_] = {
        val terminalInit = terminal.charAt(0)
        val strChar = stringChar(Character.letter(terminalInit, allowsAllSpace, isGraphic))
        val pf = (sb: StringBuilder, cpo: Option[Int]) => {
            for (cp <- cpo) parsley.character.addCodepoint(sb, cp)
            sb
        }
        val content = parsley.expr.infix.secretLeft1((sbReg.get, strChar).zipped(pf), strChar, pure(pf))
        // terminal should be first, to allow for a jump table on the main choice
        terminal *>
        // then only one string builder needs allocation
        sbReg.put(fresh(new StringBuilder)) *>
        skipManyUntil(sbReg.modify(terminalInit #> ((sb: StringBuilder) => sb += terminalInit)) <|> content, terminal)
    }
}
