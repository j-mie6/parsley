/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import parsley.Parsley, Parsley.{attempt, unit}
import parsley.character.satisfyUtf16
import parsley.combinator.{between, eof, sepBy, sepBy1, skipMany}
import parsley.errors.combinator.ErrorMethods
import parsley.registers.Reg
import parsley.token.names.{ConcreteNames, LexemeNames, Names}
import parsley.token.numeric.{Combined, Integer, LexemeCombined, LexemeInteger, LexemeReal, Real,
                              SignedCombined, SignedInteger, SignedReal, UnsignedCombined, UnsignedInteger, UnsignedReal}
import parsley.token.predicate.{Basic, CharPredicate, NotRequired, Unicode}
import parsley.token.symbol.{ConcreteSymbol, LexemeSymbol, Symbol}
import parsley.token.text.{Character, ConcreteCharacter, ConcreteString, EscapableCharacter, Escape, LexemeCharacter, LexemeString, RawCharacter}

import parsley.internal.deepembedding.singletons

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
  * @define symbol
  *     This object contains lexing functionality relevant to the parsing
  *     of atomic symbols.
  *
  *     Symbols are characterised by their "unitness", that is, every parser
  *     inside returns `Unit`. This is because they all parse a specific
  *     known entity, and, as such, the result of the parse is irrelevant.
  *     These can be things such as reserved names, or small symbols like
  *     parentheses. This object also contains a means of creating new symbols
  *     as well as implicit conversions to allow for Scala's string literals to serve
  *     as symbols within a parser.
  *
  * @define names
  *     This object contains lexing functionality relevant to the parsing
  *     of names, which include operators or identifiers.
  *
  *     The parsing of names is mostly concerned with finding the longest
  *     valid name that is not a reserved name, such as a hard keyword or
  *     a special operator.
  *
  *    TODO: describe the configuration effects.
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
// TODO: remove
class Lexer private[parsley] (desc: descriptions.LexicalDesc, errConfig: errors.ErrorConfig) {
    /** TODO:
      *
      * @param desc the configuration for the lexer, specifying the lexical
      *             rules of the grammar/language being parsed.
      * @since 4.0.0
      */
    def this(desc: descriptions.LexicalDesc) = this(desc, errors.ErrorConfig.default)
    // $COVERAGE-OFF$
    @deprecated def this(lang: LanguageDef) = this(lang.toDesc)
    // $COVERAGE-ON$

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
    object lexeme {
        /** This combinator turns a non-lexeme parser into a lexeme one by
          * ensuring whitespace is consumed after the parser.
          *
          * When using parser combinators, it is important to establish a
          * consistent whitespace consumption scheme: ideally, there is no
          * wasteful parsing, and whitespace consumption should not impact
          * backtracking. This leads to a convention that whitespace must
          * only be consumed ''after'' a token, and only once at the very
          * start of the parser (see [[fully `fully`]]). When manually
          * constructing tokens that are not supported by this lexer, use
          * this combinator to ensure it also follows the whitespace convention.
          *
          * @param p the token parser to ensure consumes trailing whitespace.
          * @since 4.0.0
          */
        def apply[A](p: Parsley[A]): Parsley[A] = p <* space.whiteSpace

        /** $names
          *
          * @since 4.0.0
          */
        val names: parsley.token.names.Names = new LexemeNames(nonlexeme.names, space.whiteSpace)

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
            // $COVERAGE-OFF$
            def unsigned: parsley.token.numeric.Integer = natural
            // $COVERAGE-ON$
            /** $natural
              *
              * @since 4.0.0
              */
            val natural: parsley.token.numeric.Integer = new LexemeInteger(nonlexeme.numeric.natural, space.whiteSpace)

            /** $integer
              *
              * @since 4.0.0
              * @note alias for [[integer `integer`]]
              * @see [[unsigned `unsigned`]] for a full description of signed integer configuration
              */
            // $COVERAGE-OFF$
            def signed: parsley.token.numeric.Integer = integer
            // $COVERAGE-ON$
            /** $integer
              *
              * @since 4.0.0
              * @see [[natural `natural`]] for a full description of integer configuration
              */
            val integer: parsley.token.numeric.Integer = new LexemeInteger(nonlexeme.numeric.integer, space.whiteSpace)

            /** $real
              *
              * @since 4.0.0
              * @note alias for [[real `real`]]
              * @see [[natural `natural`]] and [[integer `integer`]] for a full description of the configuration for the start of a real number
              */
            // $COVERAGE-OFF$
            def floating: parsley.token.numeric.Real = real
            // $COVERAGE-ON$
            private [Lexer] val positiveReal = new LexemeReal(nonlexeme.numeric.positiveReal, space.whiteSpace)
            /** $real
              *
              * @since 4.0.0
              * @see [[natural `natural`]] and [[integer `integer`]] for a full description of the configuration for the start of a real number
              */
            val real: parsley.token.numeric.Real = new LexemeReal(nonlexeme.numeric.real, space.whiteSpace)

            /** $unsignedCombined
              *
              * @since 4.0.0
              */
            val unsignedCombined: parsley.token.numeric.Combined = new LexemeCombined(nonlexeme.numeric.unsignedCombined, space.whiteSpace)
            /** $signedCombined
              *
              * @since 4.0.0
              */
            val signedCombined: parsley.token.numeric.Combined = new LexemeCombined(nonlexeme.numeric.signedCombined, space.whiteSpace)
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
            val character: parsley.token.text.Character = new LexemeCharacter(nonlexeme.text.character, space.whiteSpace)
            /** $string
              *
              * @since 4.0.0
              */
            val string: parsley.token.text.String = new LexemeString(nonlexeme.text.string, space.whiteSpace)
            /** $string
              *
              * @note $raw
              * @since 4.0.0
              */
            val rawString: parsley.token.text.String = new LexemeString(nonlexeme.text.rawString, space.whiteSpace)
            /** $multiString
              *
              * @since 4.0.0
              */
            val multiString: parsley.token.text.String = new LexemeString(nonlexeme.text.multiString, space.whiteSpace)
            /** $multiString
              *
              * @note $raw
              * @since 4.0.0
              */
            val rawMultiString: parsley.token.text.String = new LexemeString(nonlexeme.text.rawMultiString, space.whiteSpace)
        }

        /** $symbol
          *
          * @since 4.0.0
          */
        val symbol: parsley.token.symbol.Symbol = new LexemeSymbol(nonlexeme.symbol, space.whiteSpace)

        /** This object contains helper combinators for parsing terms separated by
          * common symbols.
          *
          * @since 4.0.0
          */
        object separators {
            /** TODO:
              *
              * @since 4.0.0
              */
            def semiSep[A](p: Parsley[A]): Parsley[List[A]] = sepBy(p, symbol.semi)
            /** TODO:
              *
              * @since 4.0.0
              */
            def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = sepBy1(p, symbol.semi)
            /** TODO:
              *
              * @since 4.0.0
              */
            def commaSep[A](p: Parsley[A]): Parsley[List[A]] = sepBy(p, symbol.comma)
            /** TODO:
              *
              * @since 4.0.0
              */
            def commaSep1[A](p: Parsley[A]): Parsley[List[A]] = sepBy1(p, symbol.comma)
        }

        /** This object contains helper combinators for parsing terms enclosed by
          * common symbols.
          *
          * @since 4.0.0
          */
        object enclosing {
            /** TODO:
              *
              * @since 4.0.0
              */
            def parens[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, symbol.openParen, symbol.closingParen, "parentheses")
            /** TODO:
              *
              * @since 4.0.0
              */
            def braces[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, symbol.openBrace, symbol.closingBrace, "braces")
            /** TODO:
              *
              * @since 4.0.0
              */
            def angles[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, symbol.openAngle, symbol.closingAngle, "angle brackets")
            /** TODO:
              *
              * @since 4.0.0
              */
            def brackets[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, symbol.openSquare, symbol.closingSquare, "square brackets")

            private def enclosing[A](p: =>Parsley[A], open: Parsley[Unit], close: Parsley[Unit], plural: String) =
                between(open, close.explain(s"unclosed $plural"), p)
        }
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
    object nonlexeme {
        /** $names
          *
          * @since 4.0.0
          */
        val names: parsley.token.names.Names = new ConcreteNames(desc.nameDesc, desc.symbolDesc)

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
            // $COVERAGE-OFF$
            def unsigned: parsley.token.numeric.Integer = natural
            // $COVERAGE-ON$
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
            // $COVERAGE-OFF$
            def signed: parsley.token.numeric.Integer = integer
            // $COVERAGE-ON$
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
            // $COVERAGE-OFF$
            def floating: parsley.token.numeric.Real = real
            // $COVERAGE-ON$
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
            private val escapeChar = new EscapableCharacter(desc.textDesc.escapeSequences, escapes, space.space)

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

        /** $symbol
          *
          * @since 4.0.0
          */
        val symbol: parsley.token.symbol.Symbol = new ConcreteSymbol(desc.nameDesc, desc.symbolDesc)
    }

    /** This combinator ensures a parser fully parses all available input, and consumes whitespace
      * at the start.
      *
      * This combinator should be used ''once'' as the outermost combinator in a parser. It is the
      * only combinator that should consume ''leading'' whitespace, and this must be the first
      * thing a parser does. It will ensure that, after the parser is complete, the end of the
      * input stream has been reached.
      *
      * @since 4.0.0
      */
    def fully[A](p: =>Parsley[A]): Parsley[A] = {
        val init = if (desc.spaceDesc.whitespaceIsContextDependent) space.init else unit
        init *> space.whiteSpace *> p <* eof
    }

    /** This object is concerned with special treatment of whitespace.
      *
      * For the vast majority of cases, the functionality within this
      * object shouldn't be needed, as whitespace is consistently handled
      * by `lexeme` and `fully`. However, for grammars where whitespace
      * is significant (like indentation-sensitive languages), this object
      * provides some more fine-grained control over how whitespace is
      * consumed by the parsers within `lexeme`.
      *
      * @since 4.0.0
      */
    object space {
        private [Lexer] lazy val space = desc.spaceDesc.space.toNative
        private lazy val wsImpl = Reg.make[Parsley[Unit]]

        /** This parser initialises the whitespace used by the lexer when
          * `spaceDesc.whiteSpaceIsContextDependent` is set to `true`.
          *
          * The whitespace is set to the implementation given by the lexical description.
          * This parser '''must''' be used, by `fully` or otherwise, as the first thing
          * the global parser does or an `UnfilledRegisterException` will occur.
          *
          * @note this parser is automatically invoked by the [[fully `fully`]] combinator when applicable.
          * @see [[alter `alter`]] for how to change whitespace during a parse.
          * @since 4.0.0
          */
        def init: Parsley[Unit] = {
            if (!desc.spaceDesc.whitespaceIsContextDependent) {
                // $COVERAGE-OFF$
                throw new UnsupportedOperationException( // scalastyle:ignore throw
                    "Whitespace cannot be initialised unless `spaceDesc.whitespaceIsContextDependent` is true"
                )
                // $COVERAGE-ON$
            }
            wsImpl.put(_whiteSpace)
        }

        /** This combinator changes how whitespace is parsed by lexemes for the duration of
          * a given parser.
          *
          * So long as `spaceDesc.whiteSpaceIsContextDependent` is set to `true`, this combinator
          * will be able to locally change the definition of whitespace during the given parser.
          *
          * @example
          *  In indentation sensitive languages, the indentation sensitivity is often ignored
          *  within parentheses or braces. In these cases `lexeme.enclosing.parens(space.alter(withNewline)(p))`
          *  would allow unrestricted newlines within parentheses.
          *
          * @param newSpace the new implementation of whitespace to be used during the execution of `within`.
          * @param within the parser that should be parsed using the updated whitespace.
          * @note the whitespace will not be restored to its original implementation if the
          *       given parser fails having consumed input.
          * @since 4.0.0
          */
        def alter[A](newSpace: CharPredicate)(within: =>Parsley[A]): Parsley[A] = {
            if (!desc.spaceDesc.whitespaceIsContextDependent) {
                // $COVERAGE-OFF$
                throw new UnsupportedOperationException( // scalastyle:ignore throw
                    "Whitespace cannot be altered unless `spaceDesc.whitespaceIsContextDependent` is true"
                )
                // $COVERAGE-ON$
            }
            wsImpl.rollback(wsImpl.local(whiteSpace(newSpace))(within))
        }

        private def _whiteSpace: Parsley[Unit] = whiteSpace(desc.spaceDesc.space)

        /** TODO:
          *
          * @since 4.0.0
          */
        val whiteSpace: Parsley[Unit] = {
            if (desc.spaceDesc.whitespaceIsContextDependent) wsImpl.get.flatten
            else _whiteSpace
        }

        private [Lexer] def whiteSpace(impl: CharPredicate): Parsley[Unit] = impl match {
            case NotRequired => skipComments
            case Basic(ws) => new Parsley(new singletons.WhiteSpace(ws, desc.spaceDesc.commentStart, desc.spaceDesc.commentEnd,
                                                                        desc.spaceDesc.commentLine, desc.spaceDesc.nestedComments)).hide
            case Unicode(ws) if desc.spaceDesc.supportsComments =>
                skipMany(attempt(new Parsley(new singletons.Comment(desc.spaceDesc.commentStart,
                                                                    desc.spaceDesc.commentEnd,
                                                                    desc.spaceDesc.commentLine,
                                                                    desc.spaceDesc.nestedComments))) <|> satisfyUtf16(ws)).hide
            case Unicode(ws) => skipMany(satisfyUtf16(ws)).hide
        }

        /** TODO:
          *
          * @since 4.0.0
          */
        lazy val skipComments: Parsley[Unit] = {
            if (!desc.spaceDesc.supportsComments) unit
            else {
                new Parsley(new singletons.SkipComments(desc.spaceDesc.commentStart, desc.spaceDesc.commentEnd,
                                                        desc.spaceDesc.commentLine,  desc.spaceDesc.nestedComments)).hide
            }
        }
    }

    // legacy API
    // $COVERAGE-OFF$
    @deprecated("use `space.whiteSpace` instead")
    def whiteSpace: Parsley[Unit] = space.whiteSpace
    @deprecated("use `space.skipComments` instead")
    def skipComments: Parsley[Unit] = space.skipComments
    @deprecated("use `lexeme.names.identifier` instead")
    def identifier: Parsley[String] = lexeme.names.identifier
    @deprecated("use `lexeme.symbol.softKeyword` instead")
    def keyword(name: String): Parsley[Unit] = lexeme.symbol.softKeyword(name)
    @deprecated("use `lexeme.names.userDefinedOperator` instead")
    def userOp: Parsley[String] = lexeme.names.userDefinedOperator
    @deprecated("use `lexeme.symbol.softOperator` instead")
    def operator(name: String): Parsley[Unit] = lexeme.symbol.softOperator(name)
    @deprecated("use `nonlexeme.symbol.softOperator` instead")
    def operator_(name: String): Parsley[Unit] = nonlexeme.symbol.softOperator(name)
    @deprecated("use `lexeme.symbol.softOperator` instead")
    def maxOp(name: String): Parsley[Unit] = lexeme.symbol.softOperator(name)
    @deprecated("use `nonlexeme.symbol.softOperator` instead")
    def maxOp_(name: String): Parsley[Unit] = nonlexeme.symbol.softOperator(name)
    @deprecated("use `lexeme.text.character.basicMultilingualPlane` instead")
    def charLiteral: Parsley[Char] = lexeme.text.character.basicMultilingualPlane
    @deprecated("use `lexeme.text.string.unicode` instead")
    def stringLiteral: Parsley[String] = lexeme.text.string.unicode
    @deprecated("use `nonlexeme.text.string.unicode` instead")
    def stringLiteral_ : Parsley[String] = nonlexeme.text.string.unicode
    @deprecated("use `nonlexeme.text.rawString.unicode` instead")
    def rawStringLiteral: Parsley[String] = nonlexeme.text.rawString.unicode
    @deprecated("use `lexeme.numeric.natural.number.map(_.toInt)` instead")
    def natural: Parsley[Int] = lexeme.numeric.natural.number.map(_.toInt)
    @deprecated("use `lexeme.numeric.integer.numer.map(_.toInt)` instead")
    def integer: Parsley[Int] = lexeme.numeric.integer.number.map(_.toInt)
    @deprecated("use `lexeme.numeric.positiveReal.decimal.map(_.toDouble)` instead")
    def unsignedFloat: Parsley[Double] = lexeme.numeric.positiveReal.decimal.map(_.toDouble)
    @deprecated("use `lexeme.numeric.real.decimal.map(_.toDouble)` instead")
    def float: Parsley[Double] = lexeme.numeric.real.decimal.map(_.toDouble)
    @deprecated("use `lexeme.numeric.signedCombined.number.map(...)` instead")
    def number: Parsley[Either[Int, Double]] = lexeme.numeric.signedCombined.number.map(_.fold(x => Left(x.toInt), y => Right(y.toDouble)))
    @deprecated("use `lexeme.numeric.unsignedCombined.number.map(...)` instead")
    def naturalOrFloat: Parsley[Either[Int, Double]] = lexeme.numeric.unsignedCombined.number.map(_.fold(x => Left(x.toInt), y => Right(y.toDouble)))
    @deprecated("use `lexeme.numeric.natural.decimal.map(_.toInt)` instead")
    def decimal: Parsley[Int] = lexeme.numeric.natural.decimal.map(_.toInt)
    @deprecated("use `lexeme.numeric.natural.hexadecimal.map(_.toInt)` instead")
    def hexadecimal: Parsley[Int] = lexeme.numeric.natural.hexadecimal.map(_.toInt)
    @deprecated("use `lexeme.numeric.natural.octal.map(_.toInt)` instead")
    def octal: Parsley[Int] = lexeme.numeric.natural.octal.map(_.toInt)
    @deprecated("use `lexeme(parsley.character.string(name))` instead")
    def symbol(name: String): Parsley[String] = lexeme(parsley.character.string(name))
    @deprecated("use `lexeme.symbol(name) #> name` instead")
    def symbol(name: Char): Parsley[Char] = lexeme.symbol(name) #> name
    @deprecated("use `lexeme.symbol(name) #> name` instead")
    def symbol_(name: String): Parsley[String] = lexeme.symbol(name) #> name
    @deprecated("use `lexeme.enclosing.parens` instead")
    def parens[A](p: =>Parsley[A]): Parsley[A] = lexeme.enclosing.parens(p)
    @deprecated("use `lexeme.enclosing.braces` instead")
    def braces[A](p: =>Parsley[A]): Parsley[A] = lexeme.enclosing.braces(p)
    @deprecated("use `lexeme.enclosing.angles` instead")
    def angles[A](p: =>Parsley[A]): Parsley[A] = lexeme.enclosing.angles(p)
    @deprecated("use `lexeme.enclosing.brackets` instead")
    def brackets[A](p: =>Parsley[A]): Parsley[A] = lexeme.enclosing.brackets(p)
    @deprecated("use `lexeme.symbol.semi #> ';'` instead")
    def semi: Parsley[Char] = lexeme.symbol.semi #> ';'
    @deprecated("use `lexeme.symbol.comma #> ','` instead")
    def comma: Parsley[Char] = lexeme.symbol.comma #> ','
    @deprecated("use `lexeme.symbol.colon #> ':'` instead")
    def colon: Parsley[Char] = lexeme.symbol.colon #> ':'
    @deprecated("use `lexeme.symbol.dot #> '.'` instead")
    def dot: Parsley[Char] = lexeme.symbol.dot #> '.'
    @deprecated("use `lexeme.separators.semiSep` instead")
    def semiSep[A](p: Parsley[A]): Parsley[List[A]] = lexeme.separators.semiSep(p)
    @deprecated("use `lexeme.separators.semiSep1` instead")
    def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = lexeme.separators.semiSep1(p)
    @deprecated("use `lexeme.separators.commaSep` instead")
    def commaSep[A](p: Parsley[A]): Parsley[List[A]] = lexeme.separators.commaSep(p)
    @deprecated("use `lexeme.separators.commaSep1` instead")
    def commaSep1[A](p: Parsley[A]): Parsley[List[A]] = lexeme.separators.commaSep1(p)
    // $COVERAGE-ON$
}
