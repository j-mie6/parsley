/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.{atomic, empty, fresh, pure}
import parsley.character.{char, string, strings}
import parsley.combinator.skipManyUntil
import parsley.errors.combinator.ErrorMethods
import parsley.state.Ref
import parsley.syntax.zipped.*
import parsley.token.errors.{ErrorConfig, LabelConfig, LabelWithExplainConfig}
import parsley.token.CharPred

private [token] final class ConcreteString(ends: Set[(String, String)], stringChar: StringCharacter, isGraphic: CharPred,
                                           allowsAllSpace: Boolean, err: ErrorConfig) extends StringParsers {
    private lazy val sbRef = Ref.make[StringBuilder]
    private final def finalStr = sbRef.gets { sb =>
        val s = sb.toString
        sb.clear()
        s
    }

    private def stringLiteral(valid: Parsley[StringBuilder] => Parsley[StringBuilder],
                              openLabel: (Boolean, Boolean) => LabelWithExplainConfig, closeLabel: (Boolean, Boolean) => LabelConfig) = {
        ends.view.map(makeStringParser(sbRef, valid, closeLabel)).toList match {
            case Nil => empty
            case str0 :: strs => strings(stringStart(openLabel, _), str0, strs*) ~> finalStr
        }
    }
    override lazy val fullUtf16: Parsley[String] = stringLiteral(identity, err.labelStringUtf16, err.labelStringUtf16End)
    override lazy val ascii: Parsley[String] = stringLiteral(StringParsers.ensureAscii(err), err.labelStringAscii, err.labelStringAsciiEnd)
    override lazy val latin1: Parsley[String] = stringLiteral(StringParsers.ensureExtendedAscii(err), err.labelStringLatin1, err.labelStringLatin1End)

    private def stringStart(openLabel: (Boolean, Boolean) => LabelWithExplainConfig, end: String) =
        openLabel(allowsAllSpace, stringChar.isRaw)(string(end)).ut()

    private def makeStringParser(sbRef: Ref[StringBuilder], valid: Parsley[StringBuilder] => Parsley[StringBuilder], closeLabel: (Boolean, Boolean) => LabelConfig)
                                (terminalStr: (String, String)) = {
        // NOTE: begin is consumed by the caller of this function
        val (begin, end) = terminalStr
        val terminalInit = end.charAt(0)
        val strChar = stringChar(CharacterParsers.letter(terminalInit, allowsAllSpace, isGraphic))
        val pf = (sb: StringBuilder, cpo: Option[Int]) => {
            for (cp <- cpo) parsley.unicode.addCodepoint(sb, cp)
            sb
        }
        // `content` is in a dropped position, so needs the unsafe to avoid the mutation
        // TODO: this could be fixed better with registers and skipMany?
        val content = valid(parsley.expr.infix.secretLeft1((sbRef.get, strChar).zipped(pf), strChar, pure(pf), name = null).impure)
        val p =
            // only one string builder needs allocation
            sbRef.set(fresh(new StringBuilder)) ~>
            // FIXME: could this remove the atomic by factoring the start of the string out?
            skipManyUntil(sbRef.update(char(terminalInit).hide.as((sb: StringBuilder) => sb += terminalInit)) | content,
                          closeLabel(allowsAllSpace, stringChar.isRaw)(atomic(string(end)))) // atomic needed because ambiguity with init
        (begin, p)
    }
}
