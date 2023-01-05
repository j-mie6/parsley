/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.empty
import parsley.character.{char, satisfy, charUtf16, satisfyUtf16}
import parsley.combinator.skipSome
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.errors.ErrorConfig
import parsley.token.predicate.{Basic, CharPredicate, NotRequired, Unicode}

private [token] abstract class StringCharacter {
    def apply(isLetter: CharPredicate): Parsley[Option[Int]]
    def isRaw: Boolean

    protected def _checkBadChar(err: ErrorConfig) = err.verifiedStringBadCharsUsedInLiteral.foldLeft(empty) {
        case (w, (c, reason)) => w <|> charUtf16(c).unexpected(reason)
    }
}

private [token] class RawCharacter(err: ErrorConfig) extends StringCharacter {
    override def isRaw = true
    override def apply(isLetter: CharPredicate): Parsley[Option[Int]] = isLetter match {
        case Basic(isLetter) => ErrorConfig.label(err.labelStringCharacter)(satisfy(isLetter).map(c => Some(c.toInt))) <|> _checkBadChar(err)
        case Unicode(isLetter) => ErrorConfig.label(err.labelStringCharacter)(satisfyUtf16(isLetter).map(Some(_))) <|> _checkBadChar(err)
        case NotRequired => empty
    }
}

private [token] class EscapableCharacter(desc: EscapeDesc, escapes: Escape, space: Parsley[_], err: ErrorConfig) extends StringCharacter {
    override def isRaw = false
    private lazy val escapeEmpty = ErrorConfig.label(err.labelStringEscapeEmpty)(desc.emptyEscape.fold[Parsley[Char]](empty)(char))
    private lazy val escapeGap = {
        if (desc.gapsSupported) skipSome(ErrorConfig.label(err.labelStringEscapeGap)(space)) *> ErrorConfig.label(err.labelStringEscapeGapEnd)(desc.escBegin)
        else empty
    }
    private lazy val stringEscape: Parsley[Option[Int]] =
        escapes.escapeBegin *> (escapeGap #> None
                            <|> escapeEmpty #> None
                            <|> escapes.escapeCode.map(Some(_)))

    override def apply(isLetter: CharPredicate): Parsley[Option[Int]] = {
        isLetter match {
            case Basic(isLetter) => ErrorConfig.label(err.labelStringCharacter)(
                stringEscape <|> ErrorConfig.explain(err.labelStringCharacter.flatMap(_ => err.explainGraphicCharacter))(
                                     ErrorConfig.label(err.labelGraphicCharacter)(satisfy(c => isLetter(c) && c != desc.escBegin).map(c => Some(c.toInt))))
                             <|> _checkBadChar(err)
            )
            case Unicode(isLetter) => ErrorConfig.label(err.labelStringCharacter)(
                stringEscape <|> ErrorConfig.explain(err.labelStringCharacter.flatMap(_ => err.explainGraphicCharacter))(
                                     ErrorConfig.label(err.labelGraphicCharacter)(satisfyUtf16(c => isLetter(c) && c != desc.escBegin.toInt).map(Some(_))))
                             <|> _checkBadChar(err)
            )
            case NotRequired => stringEscape
        }
    }
}
