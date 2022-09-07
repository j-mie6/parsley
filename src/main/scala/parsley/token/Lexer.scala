/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import scala.language.implicitConversions

import parsley.Parsley, Parsley.{attempt, empty, fresh, notFollowedBy, pure, unit}
import parsley.character.{char, digit, hexDigit, octDigit, satisfy, string, stringOfMany, oneOf}
import parsley.combinator.{between, many, sepBy, sepBy1, skipMany, skipSome}
import parsley.errors.combinator.{amend, entrench, unexpected, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.lift.{lift2, lift3}
import parsley.token.numeric._
import parsley.token.text.{String => _, _}

import parsley.XAssert._

import parsley.internal.deepembedding.Sign.{DoubleType, IntType, SignType}
import parsley.internal.deepembedding.singletons
import scala.annotation.implicitNotFound

/** TODO:
  *
  * @param lang TODO:
  * @since 4.0.0
  */
abstract class ImplicitLexeme private [token] {
    /** TODO:
      *
      * @since 4.0.0
      */
    implicit def implicitLexeme(s: String): Parsley[Unit]
}

/** This class provides a large selection of functionality concerned
  * with lexing.
  *
  * This class provides lexing functionality to `parsley`, however
  * it is guaranteed that nothing in this class is not implementable
  * purely using `parsley`'s pre-existing functionality. These are
  * regular parsers, but constructed in such a way that they create
  * a clear and logical separation from the rest of the parser.
  *
  * The class is broken up into several internal "modules" that group
  * together similar kinds of functionality. Importantly, the `lexemes`
  * and `nonlexemes` objects separate the underlying token implementations
  * based on whether or not they consume whitespace or not. Functionality
  * is broadly duplicated across both of these modules: `lexemes` should
  * be used by a wider parser, to ensure whitespace is handled uniformly;
  * and `nonlexemes` should be used to define further composite tokens or
  * in special circumstances where whitespace should not be consumed.
  *
  * It is possible that some of the implementations of
  * parsers found within this class may have been hand-optimised for
  * performance: care '''will''' have been taken to ensure these
  * implementations precisely match the semantics of the originals.
  *
  * @param lang the configuration for the lexer, specifying the lexing
  *             rules of the grammar/language being parsed.
  * @since 4.0.0
  */
class Lexer private [parsley] (lang: descriptions.LanguageDesc) { lexer =>
    def this(lang: LanguageDef) = this(lang.toDesc)

    // public API
    /** TODO:
      *
      * @since 4.0.0
      */
    object lexemes {
        lazy val identifier: Parsley[String] = lexeme(nonlexemes.identifier)
        def keyword(name: String): Parsley[Unit] = lexeme(nonlexemes.keyword(name))

        lazy val userOp: Parsley[String] = lexeme(nonlexemes.userOp)
        lazy val reservedOp: Parsley[String] = lexeme(nonlexemes.reservedOp)
        def operator(name: String): Parsley[Unit] = lexeme(nonlexemes.operator(name))
        def maxOp(name: String): Parsley[Unit] = lexeme(nonlexemes.maxOp(name))

        /** TODO:
          *
          * @since 4.0.0
          */
        object numeric {
            def unsigned: parsley.token.numeric.Integer = natural
            val natural: parsley.token.numeric.Integer = new LexemeInteger(nonlexemes.numeric.natural, whiteSpace)

            def signed: parsley.token.numeric.Integer = integer
            val integer: parsley.token.numeric.Integer = new LexemeInteger(nonlexemes.numeric.integer, whiteSpace)

            def floating: parsley.token.numeric.Real = real
            private [Lexer] val positiveReal = new LexemeReal(nonlexemes.numeric.positiveReal, whiteSpace)
            val real: parsley.token.numeric.Real = new LexemeReal(nonlexemes.numeric.real, whiteSpace)

            val unsignedCombined: parsley.token.numeric.Combined = new LexemeCombined(nonlexemes.numeric.unsignedCombined, whiteSpace)
            val signedCombined: parsley.token.numeric.Combined = new LexemeCombined(nonlexemes.numeric.signedCombined, whiteSpace)
        }

        /** TODO:
          *
          * @since 4.0.0
          */
        object text {
            val character: parsley.token.text.Character = new LexemeCharacter(nonlexemes.text.character, whiteSpace)
            val string: parsley.token.text.String = new LexemeString(nonlexemes.text.string, whiteSpace)
            val rawString: parsley.token.text.String = new LexemeString(nonlexemes.text.rawString, whiteSpace)
            val multiString: parsley.token.text.String = new LexemeString(nonlexemes.text.multiString, whiteSpace)
            val rawMultiString: parsley.token.text.String = new LexemeString(nonlexemes.text.rawMultiString, whiteSpace)
        }

        def symbol(name: String): Parsley[String] = lexeme(string(name))
        def symbol(name: Char): Parsley[Char] = lexeme(char(name))
        def symbol_(name: String): Parsley[String] = lexeme(attempt(string(name)))

        lazy val semi: Parsley[Char] = symbol(';').label("semicolon")
        lazy val comma: Parsley[Char] = symbol(',').label("comma")
        lazy val colon: Parsley[Char] = symbol(':').label("colon")
        lazy val dot: Parsley[Char] = symbol('.').label("dot")
        def semiSep[A](p: Parsley[A]): Parsley[List[A]] = sepBy(p, semi)
        def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = sepBy1(p, semi)
        def commaSep[A](p: Parsley[A]): Parsley[List[A]] = sepBy(p, comma)
        def commaSep1[A](p: Parsley[A]): Parsley[List[A]] = sepBy1(p, comma)

        def parens[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '(', ')', "parenthesis", "parentheses")
        def braces[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '{', '}', "brace", "braces")
        def angles[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '<', '>', "angle bracket", "angle brackets")
        def brackets[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '[', ']', "square bracket", "square brackets")

        private def enclosing[A](p: =>Parsley[A], open: Char, close: Char, singular: String, plural: String) =
            between(symbol(open).label(s"open $singular"),
                    symbol(close).label(s"matching closing $singular").explain(s"unclosed $plural"),
                    p)

    }

    /** TODO:
      *
      * @since 4.0.0
      */
    object nonlexemes {
        lazy val identifier: Parsley[String] = keyOrOp(lang.identDesc.identStart, lang.identDesc.identLetter, ident, lang.identDesc.isReservedName(_),  "identifier", "identifier", "keyword")
        def keyword(name: String): Parsley[Unit] = lang.identDesc.identLetter match {
            case Static(letter) => new Parsley(new singletons.Specific("keyword", name, letter, lang.identDesc.caseSensitive))
            case _ => attempt(caseString(name) *> notFollowedBy(identLetter).label(s"end of $name"))
        }

        lazy val userOp: Parsley[String] = keyOrOp(lang.opStart, lang.opLetter, oper, lang.isReservedOp(_), "userOp", "operator", "reserved operator")
        lazy val reservedOp: Parsley[String] = keyOrOp(lang.opStart, lang.opLetter, oper, !lang.isReservedOp(_), "reservedOp", "operator", "non-reserved operator")
        def operator(name: String): Parsley[Unit] = lang.opLetter match {
            case Static(letter) => new Parsley(new singletons.Specific("operator", name, letter, true))
            case _ => attempt(string(name) *> notFollowedBy(opLetter).label(s"end of $name"))
        }
        def maxOp(name: String): Parsley[Unit] = new Parsley(new singletons.MaxOp(name, lang.operators))

        /** TODO:
          *
          * @since 4.0.0
          */
        object numeric {
            def unsigned: parsley.token.numeric.Integer = natural
            val natural: parsley.token.numeric.Integer = new UnsignedInteger(lang.numericDesc)

            def signed: parsley.token.numeric.Integer = integer
            val integer: parsley.token.numeric.Integer = new SignedInteger(lang.numericDesc, natural)

            def floating: parsley.token.numeric.Real = real
            private [Lexer] val positiveReal = new UnsignedReal(lang.numericDesc, natural)
            val real: parsley.token.numeric.Real = new SignedReal(lang.numericDesc, positiveReal)

            val unsignedCombined: parsley.token.numeric.Combined = new UnsignedCombined(lang.numericDesc, integer, positiveReal)
            val signedCombined: parsley.token.numeric.Combined = new SignedCombined(lang.numericDesc, unsignedCombined)
        }

        /** TODO:
          *
          * @since 4.0.0
          */
        object text {
            private val escapes = new Escape(lang.textDesc.escapeChars)
            private val escapeChar = new EscapableCharacter(lang.textDesc.escapeChars, escapes, space)

            val character: parsley.token.text.Character = new ConcreteCharacter(lang.textDesc, escapes)
            val string: parsley.token.text.String =
                new ConcreteString(lang.textDesc.stringEnds, escapeChar, lang.textDesc.graphicCharacter, false)
            val rawString: parsley.token.text.String =
                new ConcreteString(lang.textDesc.stringEnds, RawCharacter, lang.textDesc.graphicCharacter, false)
            val multiString: parsley.token.text.String =
                new ConcreteString(lang.textDesc.multiStringEnds, escapeChar, lang.textDesc.graphicCharacter, true)
            val rawMultiString: parsley.token.text.String =
                new ConcreteString(lang.textDesc.multiStringEnds, RawCharacter, lang.textDesc.graphicCharacter, true)
        }
    }

    /** TODO:
      *
      * @since 4.0.0
      */
    val implicits: ImplicitLexeme = new ImplicitLexeme {
        /** @inheritdoc */
        implicit def implicitLexeme(s: String): Parsley[Unit] = {
            if (lang.identDesc.keywords(s)) lexemes.keyword(s)
            else if (lang.operators(s))     lexemes.maxOp(s)
            else                            lexemes.symbol_(s).void
        }
    }

    /** TODO:
      *
      * @since 4.0.0
      */
    def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* whiteSpace

    /** TODO:
      *
      * @since 4.0.0
      */
    lazy val whiteSpace: Parsley[Unit] = whiteSpace_(lang.whitespaceDesc.space).hide

    /** TODO:
      *
      * @since 4.0.0
      */
    val whiteSpace_ : Impl => Parsley[Unit] = {
        case NotRequired => skipComments
        case Static(ws) => new Parsley(new singletons.WhiteSpace(ws, lang.whitespaceDesc.commentStart, lang.whitespaceDesc.commentEnd,
                                                                     lang.whitespaceDesc.commentLine, lang.whitespaceDesc.nestedComments))
        case Parser(space_) if lang.whitespaceDesc.supportsComments =>
            skipMany(attempt(new Parsley(new singletons.Comment(lang.whitespaceDesc.commentStart,
                                                                lang.whitespaceDesc.commentEnd,
                                                                lang.whitespaceDesc.commentLine,
                                                                lang.whitespaceDesc.nestedComments))) <|> space_)
        case Parser(space_) => skipMany(space_)
        // $COVERAGE-OFF$
        case _ => ??? // scalastyle:ignore not.implemented.error.usage
        // $COVERAGE-ON$
    }

    /** TODO:
      *
      * @since 4.0.0
      */
    lazy val skipComments: Parsley[Unit] = {
        if (!lang.whitespaceDesc.supportsComments) unit
        else {
            new Parsley(new singletons.SkipComments(lang.whitespaceDesc.commentStart, lang.whitespaceDesc.commentEnd,
                                                    lang.whitespaceDesc.commentLine,  lang.whitespaceDesc.nestedComments))
        }
    }

    // legacy API
    @deprecated def identifier: Parsley[String] = lexemes.identifier
    @deprecated def keyword(name: String): Parsley[Unit] = lexemes.keyword(name)
    @deprecated def userOp: Parsley[String] = lexemes.userOp
    @deprecated def reservedOp_ : Parsley[String] = nonlexemes.reservedOp
    @deprecated def reservedOp: Parsley[String] = lexemes.reservedOp
    @deprecated def operator(name: String): Parsley[Unit] = lexemes.operator(name)
    @deprecated def operator_(name: String): Parsley[Unit] = nonlexemes.operator(name)
    @deprecated def maxOp(name: String): Parsley[Unit] = lexemes.maxOp(name)
    @deprecated def maxOp_(name: String): Parsley[Unit] = nonlexemes.maxOp(name)
    @deprecated def charLiteral: Parsley[Char] = lexemes.text.character.basicMultilingualPlane
    @deprecated def stringLiteral: Parsley[String] = lexemes.text.string.unicode
    @deprecated def stringLiteral_ : Parsley[String] = nonlexemes.text.string.unicode
    @deprecated def rawStringLiteral: Parsley[String] = nonlexemes.text.rawString.unicode
    @deprecated def natural: Parsley[Int] = lexemes.numeric.natural.number.map(_.toInt)
    @deprecated def integer: Parsley[Int] = lexemes.numeric.integer.number.map(_.toInt)
    @deprecated def unsignedFloat: Parsley[Double] = lexemes.numeric.positiveReal.decimal.map(_.toDouble)
    @deprecated def float: Parsley[Double] = lexemes.numeric.real.decimal.map(_.toDouble)
    @deprecated def number: Parsley[Either[Int, Double]] = lexemes.numeric.signedCombined.number.map(_.fold(x => Left(x.toInt), y => Right(y.toDouble)))
    @deprecated def naturalOrFloat: Parsley[Either[Int, Double]] =
        lexemes.numeric.unsignedCombined.number.map(_.fold(x => Left(x.toInt), y => Right(y.toDouble)))
    @deprecated def decimal: Parsley[Int] = lexemes.numeric.natural.decimal.map(_.toInt)
    @deprecated def hexadecimal: Parsley[Int] = lexemes.numeric.natural.hexadecimal.map(_.toInt)
    @deprecated def octal: Parsley[Int] = lexemes.numeric.natural.octal.map(_.toInt)
    @deprecated def symbol(name: String): Parsley[String] = lexemes.symbol(name)
    @deprecated def symbol(name: Char): Parsley[Char] = lexemes.symbol(name)
    @deprecated def symbol_(name: String): Parsley[String] = lexemes.symbol_(name)
    @deprecated def parens[A](p: =>Parsley[A]): Parsley[A] = lexemes.parens(p)
    @deprecated def braces[A](p: =>Parsley[A]): Parsley[A] = lexemes.braces(p)
    @deprecated def angles[A](p: =>Parsley[A]): Parsley[A] = lexemes.angles(p)
    @deprecated def brackets[A](p: =>Parsley[A]): Parsley[A] = lexemes.brackets(p)
    @deprecated def semi: Parsley[Char] = lexemes.semi
    @deprecated def comma: Parsley[Char] = lexemes.comma
    @deprecated def colon: Parsley[Char] = lexemes.colon
    @deprecated def dot: Parsley[Char] = lexemes.dot
    @deprecated def semiSep[A](p: Parsley[A]): Parsley[List[A]] = lexemes.semiSep(p)
    @deprecated def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = lexemes.semiSep1(p)
    @deprecated def commaSep[A](p: Parsley[A]): Parsley[List[A]] = lexemes.commaSep(p)
    @deprecated def commaSep1[A](p: Parsley[A]): Parsley[List[A]] = lexemes.commaSep1(p)

    // private API
    private def keyOrOp(startImpl: Impl, letterImpl: Impl, parser: Parsley[String], illegal: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        val builder = (start: Char => Boolean, letter: Char => Boolean) =>
            new Parsley(new singletons.NonSpecific(combinatorName, name, illegalName, start, letter, illegal))
        (startImpl, letterImpl) match {
            case (Static(start), Static(letter)) => builder(start, letter)
            case _ =>
                attempt {
                    amend {
                        // TODO: Ideally, we'd make a combinator that eliminates this flatMap
                        entrench(parser).flatMap {
                            case x if illegal(x) => unexpected(s"$illegalName $x")
                            case x => pure(x)
                        }
                    }
                }.label(name)
        }
    }

    // Identifiers & Reserved words
    private def caseString(name: String): Parsley[String] = {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) c.toLower <|> c.toUpper else c
        if (lang.identDesc.caseSensitive) string(name)
        else name.foldLeft(pure(name))((p, c) => p <* caseChar(c)).label(name)
    }
    private lazy val identStart = lang.identDesc.identStart.toParser
    private lazy val identLetter = lang.identDesc.identLetter.toParser
    private lazy val ident = lift2((c: Char, cs: String) => s"$c$cs", identStart, stringOfMany(identLetter))

    // Operators & Reserved ops
    private lazy val opStart = lang.opStart.toParser
    private lazy val opLetter = lang.opLetter.toParser
    private lazy val oper = lift2((c: Char, cs: String) => s"$c$cs", opStart, stringOfMany(opLetter))

    // White space & symbols
    private lazy val space = lang.whitespaceDesc.space.toParser
}
