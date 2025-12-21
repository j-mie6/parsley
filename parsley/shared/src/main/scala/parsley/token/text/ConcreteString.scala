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
import parsley.token.text.ConcreteStringTemplate.*

private [text] sealed abstract class ConcreteStringTemplate(err: ErrorConfig) extends StringParsers {
    override final lazy val fullUtf16: Parsley[String] = stringLiteral(identity, err.labelStringUtf16, err.labelStringUtf16End)
    override final lazy val ascii: Parsley[String] = stringLiteral(StringParsers.ensureAscii(err), err.labelStringAscii, err.labelStringAsciiEnd)
    override final lazy val latin1: Parsley[String] = stringLiteral(StringParsers.ensureExtendedAscii(err), err.labelStringLatin1, err.labelStringLatin1End)

    protected def stringLiteral(valid: Parsley[StringBuilder] => Parsley[StringBuilder],
                                openLabel: (Boolean, Boolean) => LabelWithExplainConfig, closeLabel: (Boolean, Boolean) => LabelConfig): Parsley[String]
}

private [token] final class ConcreteString(ends: Set[(String, String)], stringChar: StringCharacter, isGraphic: CharPred, allowsAllSpace: Boolean, err: ErrorConfig)
    extends ConcreteStringTemplate(err) {
    private lazy val sbRef = Ref.make[StringBuilder]

    protected def stringLiteral(valid: Parsley[StringBuilder] => Parsley[StringBuilder],
                                openLabel: (Boolean, Boolean) => LabelWithExplainConfig, closeLabel: (Boolean, Boolean) => LabelConfig) = {
        literals(endParsers(ends, this, sbRef, valid, closeLabel), sbRef, stringStart(openLabel, stringChar, allowsAllSpace, _))
    }

    private [text] def makeStringParser(sbRef: Ref[StringBuilder], valid: Parsley[StringBuilder] => Parsley[StringBuilder], closeLabel: (Boolean, Boolean) => LabelConfig)
                                       (terminalStr: (String, String)) = {
        // NOTE: begin is consumed by the caller of this function
        val (begin, end) = terminalStr
        val terminalInit = end.charAt(0)
        val strChar = stringChar(CharacterParsers.letter(terminalInit, allowsAllSpace, isGraphic))
        // `content` is in a dropped position, so needs the impure to avoid losing the mutation
        // TODO: this could be fixed better with references and skipMany?
        val content = valid(parsley.expr.infix.secretLeft1((sbRef.get, strChar).zipped(addCodepoint), strChar, pure(addCodepoint), name = null).impure)
        val p =
            // only one string builder needs allocation
            sbRef.set(fresh(new StringBuilder)) ~>
            // FIXME: could this remove the atomic by factoring the start of the string out?
            skipManyUntil(sbRef.update(char(terminalInit).hide.as((sb: StringBuilder) => sb += terminalInit)) | content,
                          closeLabel(allowsAllSpace, stringChar.isRaw)(atomic(string(end)))) // atomic needed because ambiguity with init
        (begin, p)
    }
}

private [token] final class CombinedStrings(singleEnds: Set[(String, String)], multiEnds: Set[(String, String)],
                                            single: ConcreteString, multi: ConcreteString,
                                            stringChar: StringCharacter, err: ErrorConfig) extends ConcreteStringTemplate(err) {
    private lazy val sbRef = Ref.make[StringBuilder]

    protected def stringLiteral(valid: Parsley[StringBuilder] => Parsley[StringBuilder],
                                openLabel: (Boolean, Boolean) => LabelWithExplainConfig, closeLabel: (Boolean, Boolean) => LabelConfig) = {
        val parsers = endParsers(singleEnds, single, sbRef, valid, closeLabel) ::: endParsers(multiEnds, multi, sbRef, valid, closeLabel)
        literals(parsers, sbRef, stringStart(openLabel, stringChar, allowsAllSpace = false, _))
    }
}

private [text] object ConcreteStringTemplate {
    def endParsers(ends: Set[(String, String)], impl: ConcreteString, sbRef: Ref[StringBuilder], valid: Parsley[StringBuilder] => Parsley[StringBuilder], closeLabel: (Boolean, Boolean) => LabelConfig) = {
        ends.view.map(impl.makeStringParser(sbRef, valid, closeLabel)).toList
    }
    val addCodepoint = (sb: StringBuilder, cpo: Option[Int]) => {
        for (cp <- cpo) parsley.unicode.addCodepoint(sb, cp)
        sb
    }

    def stringStart(openLabel: (Boolean, Boolean) => LabelWithExplainConfig, stringChar: StringCharacter, allowsAllSpace: Boolean, end: String) =
        openLabel(allowsAllSpace, stringChar.isRaw)(string(end)).ut()

    def literals(parsers: List[(String, Parsley[Unit])], sbRef: Ref[StringBuilder], string: String => Parsley[String]) = parsers match {
        case Nil => empty
        case str0 :: strs => strings(string, str0, strs*) ~> finalStr(sbRef)
    }

    private def finalStr(sbRef: Ref[StringBuilder]) = sbRef.gets { sb =>
        val s = sb.toString
        sb.clear()
        s
    }
}
