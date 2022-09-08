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

/** This class provides implicit functionality to promote string
  * literals into tokens.
  *
  * @since 4.0.0
  */
abstract class ImplicitLexeme private [token] {
    /** This method takes the given string and turns it
      * into a parser for that token.
      *
      * This method can be brought into scope in a parser to
      * allow string literals to naturally serve as tokens.
      * In particular, it will correctly deal with known keywords
      * and operators, and otherwise handle other strings at
      * face-value.
      *
      * @note it is assumed that
      * the token's content is irrelevant, since it is
      * already known what it is, so `Unit` is returned.
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
  * @define numeric
  *     This object contains lexing functionality relevant to the parsing
  *     of numbers. This is sub-divided into different categories:
  *
  *       - integers (both signed and unsigned)
  *       - reals    (signed only)
  *       - a combination of the two (signed and unsigned)
  *
  *     These contain relevant functionality for the processing of
  *     decimal, hexadecimal, octal, and binary literals; or some
  *     mixed combination thereof (as specified by `desc.numericDesc`).
  *     Additionally, it is possible to ensure literals represent known
  *     sizes or precisions.
  *
  * @define text
  *     This object contains lexing functionality relevant to the parsing
  *     of text. This is sub-divided into different categories:
  *
  *       - string literals (both with escapes and raw)
  *       - multi-line string literals (both with escapes and raw)
  *       - character literals
  *
  *     These contain the relevant functionality required to specify the
  *     degree of unicode support for the underlying language, from
  *     ASCII to full UTF-16.
  *
  * @define natural
  *     This is a collection of parsers concerned with handling unsigned (positive) integer literals.
  *
  *     Natural numbers are described generally as follows:
  *       - '''`desc.numericDesc.literalBreakChar`''': determines whether or not it
  *         is legal to "break up" the digits within a literal, for example: is `1_000_000` allowed?
  *         If this is legal, describes what the break character is, and whether it can appear after
  *         a hexadecimal/octal/binary prefix
  *       - '''`desc.numericDesc.leadingZerosAllowed`''': determines whether or not it is
  *         possible to add extraneous zero digits onto the front of a number or not. In some languages,
  *         like C, this is disallowed, as numbers starting with `0` are octal numbers.
  *       - '''`desc.numericDesc.integerNumbersCanBe{Hexadecimal/Octal/Binary}`''': these flags
  *         control what kind of literals can appear within the `number` parser. Each type of literal
  *         can be individually parsed with its corresponding parser, regardless of the value of the
  *         flag
  *       - '''`desc.numericDesc.{hexadecimal/octal/binary}Leads`''': controls what character must
  *         follow a `0` when starting a number to change it from decimal into another base. This
  *         set may be empty, in which case the literal is described purely with leading zero (C style
  *         octals would set `octalLeads` to `Set.empty`)
  *
  *     Additional to the parsing of decimal, hexadecimal, octal, and binary literals, each parser can
  *     be given a bit-width from 8- to 64-bit: this will check the parsed literal to ensure it is
  *     a legal literal of that size.
  * @define integer
  *     This is a collection of parsers concerned with handling signed integer literals.
  *
  *     Signed integer literals are an extension of unsigned integer literals with the following
  *     extra configuration:
  *       - '''`desc.numericDesc.positiveSign`''': describes whether or not literals are
  *         allowed to omit `+` for positive literals, must write a `+`, or can never write a `+`.
  * @define real
  *     This is a collection of parsers concerned with handling signed real numbers (like floats and doubles).
  *
  *     These literals consist of a (possibly optional) integer prefix, with at least one of a fractional component (with `.`)
  *     or an exponential component.
  *
  *     Real numbers are an extension of signed integers with the following additional configuration:
  *       - '''`desc.numericDesc.leadingDotAllowed`''': determines whether a literal like `.0` would be considered legal
  *       - '''`desc.numericDesc.trailingDotAllowed`''': determines whether a literal like `0.` would be considered legal
  *       - '''`desc.numericDesc.realNumbersCanBe{Hexadecimal/Octal/Binary}`''': these flags control
  *         what kind of literals can appear within the `number` parser. Each type of literal
  *         may still be individually parsed with its corresponding parser, regardless of the value of
  *         the flag
  *       - '''`desc.numericDesc.{decimal/hexadecimal/octal/binary}ExponentDesc`''': describes how the
  *         exponential syntax works for each kind of base. If the syntax is legal, then this describes:
  *         which characters start it (classically, this would be `e` or `E` for decimals); whether or
  *         not it is compulsory for the literal (in Java and C, hexadecimal floats are ''only'' valid
  *         when they have an exponent attached); and whether or not a `+` sign is mandatory, optional,
  *         or illegal for positive exponents
  *
  *   Additional to the parsing of decimal, hexadecimal, octal, and binary floating literals, each
  *   parser can be given a precision of IEEE 754 float or double. This can either be achieved by
  *   rounding to the nearest representable value, or by ensuring that the literal must be precisely
  *   representable as one of these numbers (which is defined as being one of binary, decimal
  *   or exact `float` and `double` values as described by Java)
  *
  * @define unsignedCombined
  *     This is a collection of parsers concerned with handling numeric literals that may either be
  *     unsigned integers ''or'' unsigned reals.
  *
  *     There is no additional configuration offered over that found in `natural` or `real`.
  *
  *     the bit-bounds and precision of the integer or real parts of the result can be specified
  *     in any pairing.
  * @define signedCombined
  *     This is a collection of parsers concerned with handling numeric literals that may either be
  *     signed integers ''or'' signed reals.
  *
  *     There is no additional configuration offered over that found in `integer` or `real`.
  *
  *     the bit-bounds and precision of the integer or real parts of the result can be specified
  *     in any pairing.
  *
  * @define character
  *     This is a collection of parsers concerned with handling character literals.
  *
  *     Character literals are described generally as follows:
  *       - '''`desc.textDesc.characterLiteralEnd`''': the character that starts and ends
  *         the literal (for example in many languages this is `'`)
  *       - '''`desc.textDesc.graphicCharacter`''': describes the legal characters that may appear
  *         in the literal directly. Usually, this excludes control characters and newlines,
  *         but permits most other things. Escape sequences can represent non-graphic
  *         characters
  *       - '''`desc.textDesc.escapeSequences`''': describes the legal escape sequences that
  *         that can appear in a character literal (for example `\n` or `\u000a`)
  *
  *     Aside from the generic configuration, characters can be parsed in accordance with
  *     varying levels of unicode support, from ASCII-only to full UTF-16 characters. Parsers
  *     for each of four different vareties are exposed by this object.
  * @define string
  *     This is a collection of parsers concerned with handling single-line string literals.
  *
  *     String literals are described generally as follows:
  *       - '''`desc.textDesc.stringEnds`''':  the sequence of characters that can begin or
  *         end a string literal. Regardless of which of these is used for a specific literal,
  *         the end of the literal ''must'' use the same sequence
  *       - '''`desc.textDesc.graphicCharacter`''': describes the legal characters that may appear
  *         in the literal directly. Usually, this excludes control characters and newlines,
  *         but permits most other things. Escape sequences can represent non-graphic
  *         characters for non-raw strings
  *       - '''`desc.textDesc.escapeSequences`''': describes the legal escape sequences that
  *         that can appear in a string literal (for example `\n` or `\u000a`)
  * @define multiString
  *    This is a collection of parsers concerned with handling multi-line string literals.
  *
  *     String literals are described generally as follows:
  *       - '''`desc.textDesc.multiStringEnds`''':  the sequence of characters that can begin or
  *         end a multi-line string literal. Regardless of which of these is used for a specific literal,
  *         the end of the literal ''must'' use the same sequence
  *       - '''`desc.textDesc.graphicCharacter`''': describes the legal characters that may appear
  *         in the literal directly. Usually, this excludes control characters and newlines,
  *         but permits most other things. Escape sequences can represent non-graphic
  *         characters for non-raw strings
  *       - '''`desc.textDesc.escapeSequences`''': describes the legal escape sequences that
  *         that can appear in a string literal (for example `\n` or `\u000a`)
  * @define raw this will be parsed without handling any escape sequences,
  *         this includes literal-end characters and the escape prefix
  *         (often `"` and `\` respectively)
  *
  * @constructor TODO:
  * @param desc the configuration for the lexer, specifying the lexical
  *             rules of the grammar being parsed.
  * @param errConfig the configuration for error messages generated within
  *                  the lexer.
  * @since 4.0.0
  */
class Lexer private [parsley] (desc: descriptions.LexicalDesc, errConfig: errors.ErrorConfig) { lexer =>
    /** TODO:
      *
      * @param desc the configuration for the lexer, specifying the lexical
      *             rules of the grammar/language being parsed.
      * @since 4.0.0
      */
    private [parsley] def this(desc: descriptions.LexicalDesc) = this(desc, errors.ErrorConfig.default)
    def this(lang: LanguageDef) = this(lang.toDesc)

    /** This object is concerned with ''lexemes'': these are tokens that are
      * treated as "words", such that whitespace will be consumed after each
      * has been parsed.
      *
      * Ideally, a wider parser should not be concerned with
      * handling whitespace, as it is responsible for dealing with a stream
      * of tokens. With parser combinators, however, it is usually not the
      * case that there is a separate distinction between the parsing phase
      * and the lexing phase. That said, it is good practice to establish
      * a logical separation between the two worlds. As such, this object
      * contains parsers that parse tokens, and these are whitespace-aware.
      * This means that whitespace will be consumed '''after''' any of these
      * parsers are parsed. It is not, however, required that whitespace be
      * present.
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

        /** $numeric
          *
          * @since 4.0.0
          */
        object numeric {
            /** $natural
              *
              * @since 4.0.0
              * @note alias for [[natural `natural`]].
              */
            def unsigned: parsley.token.numeric.Integer = natural
            /** $natural
              *
              * @since 4.0.0
              */
            val natural: parsley.token.numeric.Integer = new LexemeInteger(nonlexemes.numeric.natural, whiteSpace)

            /** $integer
              *
              * @since 4.0.0
              * @note alias for [[integer `integer`]]
              * @see [[unsigned `unsigned`]] for a full description of signed integer configuration
              */
            def signed: parsley.token.numeric.Integer = integer
            /** $integer
              *
              * @since 4.0.0
              * @see [[natural `natural`]] for a full description of integer configuration
              */
            val integer: parsley.token.numeric.Integer = new LexemeInteger(nonlexemes.numeric.integer, whiteSpace)

            /** $real
              *
              * @since 4.0.0
              * @note alias for [[real `real`]]
              * @see [[natural `natural`]] and [[integer `integer`]] for a full description of the configuration for the start of a real number
              */
            def floating: parsley.token.numeric.Real = real
            private [Lexer] val positiveReal = new LexemeReal(nonlexemes.numeric.positiveReal, whiteSpace)
            /** $real
              *
              * @since 4.0.0
              * @see [[natural `natural`]] and [[integer `integer`]] for a full description of the configuration for the start of a real number
              */
            val real: parsley.token.numeric.Real = new LexemeReal(nonlexemes.numeric.real, whiteSpace)

            /** $unsignedCombined
              *
              * @since 4.0.0
              */
            val unsignedCombined: parsley.token.numeric.Combined = new LexemeCombined(nonlexemes.numeric.unsignedCombined, whiteSpace)
            /** $signedCombined
              *
              * @since 4.0.0
              */
            val signedCombined: parsley.token.numeric.Combined = new LexemeCombined(nonlexemes.numeric.signedCombined, whiteSpace)
        }

        /** $text
          *
          * @since 4.0.0
          */
        object text {
            /** $character
              *
              * @since 4.0.0
              */
            val character: parsley.token.text.Character = new LexemeCharacter(nonlexemes.text.character, whiteSpace)
            /** $string
              *
              * @since 4.0.0
              */
            val string: parsley.token.text.String = new LexemeString(nonlexemes.text.string, whiteSpace)
            /** $string
              *
              * @note $raw
              * @since 4.0.0
              */
            val rawString: parsley.token.text.String = new LexemeString(nonlexemes.text.rawString, whiteSpace)
            /** $multiString
              *
              * @since 4.0.0
              */
            val multiString: parsley.token.text.String = new LexemeString(nonlexemes.text.multiString, whiteSpace)
            /** $multiString
              *
              * @note $raw
              * @since 4.0.0
              */
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

    /** This object is concerned with ''non-lexemes'': these are tokens that
      * do not give any special treatment to whitespace.
      *
      * Whilst the functionality in `lexeme` is ''strongly'' recommended for
      * wider use in a parser, the functionality here may be useful for more
      * specialised use-cases. In particular, these may for the building blocks
      * for more complex tokens (where whitespace is not allowed between them, say),
      * in which case these compound tokens can be turned into lexemes manually.
      * For example, the lexer does not have configuration for trailing specifiers
      * on numeric literals (like, `1024L` in Scala, say): the desired numeric
      * literal parser could be extended with this functionality ''before'' whitespace
      * is consumed by using the variant found in this object.
      *
      * Alternatively, these tokens can be used for ''lexical extraction'', which
      * can be performed by the [[parsley.errors.ErrorBuilder `ErrorBuilder`]]
      * typeclass: this can be used to try and extract tokens from the input stream
      * when an error happens, to provide a more informative error. In this case,
      * it is desirable to ''not'' consume whitespace after the token to keep the
      * error tight and precise.
      *
      * @since 4.0.0
      */
    object nonlexemes {
        lazy val identifier: Parsley[String] = keyOrOp(desc.identDesc.identStart, desc.identDesc.identLetter, ident, desc.identDesc.isReservedName(_),  "identifier", "identifier", "keyword")
        def keyword(name: String): Parsley[Unit] = desc.identDesc.identLetter match {
            case Static(letter) => new Parsley(new singletons.Specific("keyword", name, letter, desc.identDesc.caseSensitive))
            case _ => attempt(caseString(name) *> notFollowedBy(identLetter).label(s"end of $name"))
        }

        lazy val userOp: Parsley[String] = keyOrOp(desc.opStart, desc.opLetter, oper, desc.isReservedOp(_), "userOp", "operator", "reserved operator")
        lazy val reservedOp: Parsley[String] = keyOrOp(desc.opStart, desc.opLetter, oper, !desc.isReservedOp(_), "reservedOp", "operator", "non-reserved operator")
        def operator(name: String): Parsley[Unit] = desc.opLetter match {
            case Static(letter) => new Parsley(new singletons.Specific("operator", name, letter, true))
            case _ => attempt(string(name) *> notFollowedBy(opLetter).label(s"end of $name"))
        }
        def maxOp(name: String): Parsley[Unit] = new Parsley(new singletons.MaxOp(name, desc.operators))

        /** $numeric
          *
          * @since 4.0.0
          */
        object numeric {
            /** $natural
              *
              * @since 4.0.0
              * @note alias for [[natural `natural`]].
              */
            def unsigned: parsley.token.numeric.Integer = natural
            /** $natural
              *
              * @since 4.0.0
              */
            val natural: parsley.token.numeric.Integer = new UnsignedInteger(desc.numericDesc)

            /** $integer
              *
              * @since 4.0.0
              * @note alias for [[integer `integer`]]
              * @see [[unsigned `unsigned`]] for a full description of signed integer configuration
              */
            def signed: parsley.token.numeric.Integer = integer
            /** $integer
              *
              * @since 4.0.0
              * @see [[natural `natural`]] for a full description of integer configuration
              */
            val integer: parsley.token.numeric.Integer = new SignedInteger(desc.numericDesc, natural)

            /** $real
              *
              * @since 4.0.0
              * @note alias for [[real `real`]]
              * @see [[natural `natural`]] and [[integer `integer`]] for a full description of the configuration for the start of a real number
              */
            def floating: parsley.token.numeric.Real = real
            private [Lexer] val positiveReal = new UnsignedReal(desc.numericDesc, natural)
            /** $real
              *
              * @since 4.0.0
              * @see [[natural `natural`]] and [[integer `integer`]] for a full description of the configuration for the start of a real number
              */
            val real: parsley.token.numeric.Real = new SignedReal(desc.numericDesc, positiveReal)

            /** $unsignedCombined
              *
              * @since 4.0.0
              */
            val unsignedCombined: parsley.token.numeric.Combined = new UnsignedCombined(desc.numericDesc, integer, positiveReal)
            /** $signedCombined
              *
              * @since 4.0.0
              */
            val signedCombined: parsley.token.numeric.Combined = new SignedCombined(desc.numericDesc, unsignedCombined)
        }

        /** $text
          *
          * @since 4.0.0
          */
        object text {
            private val escapes = new Escape(desc.textDesc.escapeSequences)
            private val escapeChar = new EscapableCharacter(desc.textDesc.escapeSequences, escapes, space)

            /** $character
              *
              * @since 4.0.0
              */
            val character: parsley.token.text.Character = new ConcreteCharacter(desc.textDesc, escapes)
            /** $string
              *
              * @since 4.0.0
              */
            val string: parsley.token.text.String =
                new ConcreteString(desc.textDesc.stringEnds, escapeChar, desc.textDesc.graphicCharacter, false)
            /** $string
              *
              * @note $raw
              * @since 4.0.0
              */
            val rawString: parsley.token.text.String =
                new ConcreteString(desc.textDesc.stringEnds, RawCharacter, desc.textDesc.graphicCharacter, false)
            /** $multiString
              *
              * @since 4.0.0
              */
            val multiString: parsley.token.text.String =
                new ConcreteString(desc.textDesc.multiStringEnds, escapeChar, desc.textDesc.graphicCharacter, true)
            /** $multiString
              *
              * @note $raw
              * @since 4.0.0
              */
            val rawMultiString: parsley.token.text.String =
                new ConcreteString(desc.textDesc.multiStringEnds, RawCharacter, desc.textDesc.graphicCharacter, true)
        }
    }

    /** This object can be imported from to expose a way of converting raw Scala string literals
      * into a parser for that specific token.
      *
      * @since 4.0.0
      */
    val implicits: ImplicitLexeme = new ImplicitLexeme {
        /** @inheritdoc */
        implicit def implicitLexeme(s: String): Parsley[Unit] = {
            if (desc.identDesc.keywords(s)) lexemes.keyword(s)
            else if (desc.operators(s))     lexemes.maxOp(s)
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
    lazy val whiteSpace: Parsley[Unit] = whiteSpace_(desc.whitespaceDesc.space).hide

    /** TODO:
      *
      * @since 4.0.0
      */
    val whiteSpace_ : Impl => Parsley[Unit] = {
        case NotRequired => skipComments
        case Static(ws) => new Parsley(new singletons.WhiteSpace(ws, desc.whitespaceDesc.commentStart, desc.whitespaceDesc.commentEnd,
                                                                     desc.whitespaceDesc.commentLine, desc.whitespaceDesc.nestedComments))
        case Parser(space_) if desc.whitespaceDesc.supportsComments =>
            skipMany(attempt(new Parsley(new singletons.Comment(desc.whitespaceDesc.commentStart,
                                                                desc.whitespaceDesc.commentEnd,
                                                                desc.whitespaceDesc.commentLine,
                                                                desc.whitespaceDesc.nestedComments))) <|> space_)
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
        if (!desc.whitespaceDesc.supportsComments) unit
        else {
            new Parsley(new singletons.SkipComments(desc.whitespaceDesc.commentStart, desc.whitespaceDesc.commentEnd,
                                                    desc.whitespaceDesc.commentLine,  desc.whitespaceDesc.nestedComments))
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
        if (desc.identDesc.caseSensitive) string(name)
        else name.foldLeft(pure(name))((p, c) => p <* caseChar(c)).label(name)
    }
    private lazy val identStart = desc.identDesc.identStart.toParser
    private lazy val identLetter = desc.identDesc.identLetter.toParser
    private lazy val ident = lift2((c: Char, cs: String) => s"$c$cs", identStart, stringOfMany(identLetter))

    // Operators & Reserved ops
    private lazy val opStart = desc.opStart.toParser
    private lazy val opLetter = desc.opLetter.toParser
    private lazy val oper = lift2((c: Char, cs: String) => s"$c$cs", opStart, stringOfMany(opLetter))

    // White space & symbols
    private lazy val space = desc.whitespaceDesc.space.toParser
}
