/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.{empty, some}
import parsley.character.{char, satisfyMap}
import parsley.syntax.character.charLift
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.errors.ErrorConfig
import parsley.token.predicate.{Basic, CharPredicate, NotRequired, Unicode}
import parsley.unicode.{satisfyMap => satisfyMapUtf16}

private [token] abstract class StringCharacter {
    def apply(isLetter: CharPredicate): Parsley[Option[Int]]
    def isRaw: Boolean

    protected def _checkBadChar(err: ErrorConfig) = err.verifiedStringBadCharsUsedInLiteral.checkBadChar
}

private [token] class RawCharacter(err: ErrorConfig) extends StringCharacter {
    override def isRaw: Boolean = true
    override def apply(isLetter: CharPredicate): Parsley[Option[Int]] = isLetter match {
        case Basic(isLetter) => err.labelStringCharacter(satisfyMap { case c if isLetter(c) => Some(c.toInt) }) <|> _checkBadChar(err)
        case Unicode(isLetter) => err.labelStringCharacter(satisfyMapUtf16 { case c if isLetter(c) => Some(c) }) <|> _checkBadChar(err)
        case NotRequired => empty
    }
}

private [token] class EscapableCharacter(desc: EscapeDesc, escapes: Escape, space: Parsley[_], err: ErrorConfig) extends StringCharacter {
    override def isRaw: Boolean = false
    private lazy val escapeEmpty = err.labelStringEscapeEmpty(desc.emptyEscape.fold[Parsley[Char]](empty)(char))
    private lazy val escapeGap = {
        if (desc.gapsSupported) some(err.labelStringEscapeGap(space)) ~> err.labelStringEscapeGapEnd(desc.escBegin)
        else empty
    }
    private lazy val stringEscape: Parsley[Option[Int]] =
        escapes.escapeBegin *> (escapeGap.as(None)
                            <|> escapeEmpty.as(None)
                            <|> escapes.escapeCode.map(Some(_)))

    override def apply(isLetter: CharPredicate): Parsley[Option[Int]] = {
        isLetter match {
            case Basic(isLetter) => err.labelStringCharacter(
                stringEscape <|> err.labelGraphicCharacter(satisfyMap { case c if isLetter(c) && c != desc.escBegin => Some(c.toInt) })
                             <|> _checkBadChar(err)
            )
            case Unicode(isLetter) => err.labelStringCharacter(
                stringEscape <|> err.labelGraphicCharacter(satisfyMapUtf16 { case c if isLetter(c) && c != desc.escBegin.toInt => Some(c) })
                             <|> _checkBadChar(err)
            )
            case NotRequired => stringEscape
        }
    }
}
