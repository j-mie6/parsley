/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.{attempt, fresh, pure}
import parsley.character.{char, string}
import parsley.combinator.{choice, skipManyUntil}
import parsley.implicits.zipped.Zipped2
import parsley.token.errors.{ErrorConfig, LabelConfig, LabelWithExplainConfig}
import parsley.token.predicate.CharPredicate

private [token] final class ConcreteString(ends: Set[ScalaString], stringChar: StringCharacter, isGraphic: CharPredicate,
                                           allowsAllSpace: Boolean, err: ErrorConfig) extends String {

    private def stringLiteral(valid: Parsley[StringBuilder] => Parsley[StringBuilder],
                              openLabel: (Boolean, Boolean) => LabelWithExplainConfig, closeLabel: (Boolean, Boolean) => LabelConfig) = {
        choice(ends.view.map(makeStringParser(valid, openLabel, closeLabel)).toSeq: _*) *> sbReg.gets(_.toString)
    }
    override lazy val fullUtf16: Parsley[ScalaString] = stringLiteral(identity, err.labelStringUtf16, err.labelStringUtf16End)
    override lazy val ascii: Parsley[ScalaString] = stringLiteral(String.ensureAscii(err), err.labelStringAscii, err.labelStringAsciiEnd)
    override lazy val latin1: Parsley[ScalaString] = stringLiteral(String.ensureExtendedAscii(err), err.labelStringLatin1, err.labelStringLatin1End)

    private val sbReg = parsley.registers.Reg.make[StringBuilder]

    private def makeStringParser(valid: Parsley[StringBuilder] => Parsley[StringBuilder],
                                 openLabel: (Boolean, Boolean) => LabelWithExplainConfig, closeLabel: (Boolean, Boolean) => LabelConfig)
                                (terminalStr: ScalaString) = {
        val terminalInit = terminalStr.charAt(0)
        val strChar = stringChar(Character.letter(terminalInit, allowsAllSpace, isGraphic))
        val pf = (sb: StringBuilder, cpo: Option[Int]) => {
            for (cp <- cpo) parsley.character.addCodepoint(sb, cp)
            sb
        }
        val content = valid(parsley.expr.infix.secretLeft1((sbReg.get, strChar).zipped(pf), strChar, pure(pf)))
        val terminal = string(terminalStr)
        // terminal should be first, to allow for a jump table on the main choice
        openLabel(allowsAllSpace, stringChar.isRaw)(terminal) *>
        // then only one string builder needs allocation
        sbReg.put(fresh(new StringBuilder)) *>
        skipManyUntil(sbReg.modify(char(terminalInit) #> ((sb: StringBuilder) => sb += terminalInit)) <|> content,
                      closeLabel(allowsAllSpace, stringChar.isRaw)(attempt(terminal))) //is the attempt needed here? not sure
    }
}
