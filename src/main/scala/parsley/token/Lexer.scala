/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import scala.language.implicitConversions

import parsley.Parsley, Parsley.{attempt, empty, notFollowedBy, pure, unit}
import parsley.character.{char, digit, hexDigit, octDigit, satisfy, string, stringOfMany, oneOf}
import parsley.combinator.{between, many, sepBy, sepBy1, skipMany, skipSome}
import parsley.errors.combinator.{amend, entrench, unexpected, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.lift.lift2

import parsley.internal.deepembedding.Sign.{DoubleType, IntType, SignType}
import parsley.internal.deepembedding.singletons
import scala.annotation.implicitNotFound

/**
  * When provided with a `LanguageDef`, this class will produce a large variety of parsers that can be used for
  * tokenisation of a language. This includes parsing numbers and strings in their various formats and ensuring that
  * all operations consume whitespace after them (so-called lexeme parsers). These are very useful in parsing
  * programming languages. This class also has a large number of hand-optimised intrinsic parsers to improve performance!
  * @param lang The rules that govern the language we are tokenising
  * @since 2.2.0
  */
class Lexer private (lang: descriptions.LanguageDesc) {
    def this(lang: LanguageDef) = this(lang.toDesc)

    // public API
    object lexemes {
        lazy val identifier: Parsley[String] = lexeme(nonlexemes.identifier)
        def keyword(name: String): Parsley[Unit] = lexeme(nonlexemes.keyword(name))

        lazy val userOp: Parsley[String] = lexeme(nonlexemes.userOp)
        lazy val reservedOp: Parsley[String] = lexeme(nonlexemes.reservedOp)
        def operator(name: String): Parsley[Unit] = lexeme(nonlexemes.operator(name))
        def maxOp(name: String): Parsley[Unit] = lexeme(nonlexemes.maxOp(name))

        lazy val charLiteral: Parsley[Char] = lexeme(nonlexemes.charLiteral)
        lazy val stringLiteral: Parsley[String] = lexeme(nonlexemes.stringLiteral)
        lazy val rawStringLiteral: Parsley[String] = lexeme(nonlexemes.rawStringLiteral)

        lazy val natural: Parsley[BigInt] = lexeme(nonlexemes.natural)
        lazy val integer: Parsley[BigInt] = lexeme(nonlexemes.integer)
        lazy val unsignedFloat: Parsley[BigDecimal] = lexeme(nonlexemes.unsignedFloat)
        lazy val float: Parsley[BigDecimal] = lexeme(nonlexemes.float)
        lazy val naturalOrFloat: Parsley[Either[BigInt, BigDecimal]] = lexeme(nonlexemes.naturalOrFloat)
        lazy val number: Parsley[Either[BigInt, BigDecimal]] = lexeme(nonlexemes.number)
        lazy val decimal: Parsley[BigInt] = lexeme(nonlexemes.decimal)
        lazy val hexadecimal: Parsley[BigInt] = lexeme(nonlexemes.hexadecimal)
        lazy val octal: Parsley[BigInt] = lexeme(nonlexemes.octal)

        object numeric {
            def unsigned = natural
            object natural {
                val decimal: Parsley[BigInt] = lexeme(lexemes.numeric.natural.decimal)
                val hexadecimal: Parsley[BigInt] = lexeme(lexemes.numeric.natural.hexadecimal)
                val octal: Parsley[BigInt] = lexeme(lexemes.numeric.natural.octal)
                val binary: Parsley[BigInt] = lexeme(lexemes.numeric.natural.binary)
                val number: Parsley[BigInt] = lexeme(lexemes.numeric.natural.number)

                def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.number8)
                def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.decimal8)
                def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.hexadecimal8)
                def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.octal8)
                def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.binary8)

                def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.number16)
                def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.decimal16)
                def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.hexadecimal16)
                def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.octal16)
                def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.binary16)

                def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.number32)
                def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.decimal32)
                def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.hexadecimal32)
                def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.octal32)
                def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.binary32)

                def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.number64)
                def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.decimal64)
                def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.hexadecimal64)
                def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.octal64)
                def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.natural.binary64)
            }

            def signed = integer
            object integer {
                val decimal: Parsley[BigInt] = lexeme(lexemes.numeric.integer.decimal)
                val hexadecimal: Parsley[BigInt] = lexeme(lexemes.numeric.integer.hexadecimal)
                val octal: Parsley[BigInt] = lexeme(lexemes.numeric.integer.octal)
                val binary: Parsley[BigInt] = lexeme(lexemes.numeric.integer.binary)
                val number: Parsley[BigInt] = lexeme(lexemes.numeric.integer.number)

                def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.number8)
                def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.decimal8)
                def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.hexadecimal8)
                def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.octal8)
                def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.binary8)

                def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.number16)
                def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.decimal16)
                def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.hexadecimal16)
                def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.octal16)
                def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.binary16)

                def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.number32)
                def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.decimal32)
                def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.hexadecimal32)
                def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.octal32)
                def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.binary32)

                def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.number64)
                def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.decimal64)
                def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.hexadecimal64)
                def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.octal64)
                def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = lexeme(lexemes.numeric.integer.binary64)
            }

            def floating = rational
            object rational {

            }
        }

        def symbol(name: String): Parsley[String] = lexeme(string(name))
        def symbol(name: Char): Parsley[Char] = lexeme(char(name))
        def symbol_(name: String): Parsley[String] = lexeme(attempt(string(name)))
        def parens[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '(', ')', "parenthesis", "parentheses")
        def braces[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '{', '}', "brace", "braces")
        def angles[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '<', '>', "angle bracket", "angle brackets")
        def brackets[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '[', ']', "square bracket", "square brackets")
        lazy val semi: Parsley[Char] = lexeme(';'.label("semicolon"))
        lazy val comma: Parsley[Char] = lexeme(','.label("comma"))
        lazy val colon: Parsley[Char] = lexeme(':'.label("colon"))
        lazy val dot: Parsley[Char] = lexeme('.'.label("dot"))
        def semiSep[A](p: Parsley[A]): Parsley[List[A]] = sepBy(p, semi)
        def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = sepBy1(p, semi)
        def commaSep[A](p: Parsley[A]): Parsley[List[A]] = sepBy(p, comma)
        def commaSep1[A](p: Parsley[A]): Parsley[List[A]] = sepBy1(p, comma)

        private def enclosing[A](p: =>Parsley[A], open: Char, close: Char, singular: String, plural: String) =
            between(lexeme(open.label(s"open $singular")),
                    lexeme(close.label(s"matching closing $singular").explain(s"unclosed $plural")),
                    p)
    }

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

        lazy val charLiteral: Parsley[Char] = between('\''.label("character"), '\''.label("end of character"), characterChar)
        lazy val stringLiteral: Parsley[String] = lang.whitespaceDesc.space match {
            case Static(ws) => new Parsley(new singletons.StringLiteral(ws))
            case _ => between('"'.label("string"), '"'.label("end of string"), many(stringChar)).map(_.flatten.mkString)
        }
        lazy val rawStringLiteral: Parsley[String] = new Parsley(singletons.RawStringLiteral)

        /* OLD NUMERIC API START */
        lazy val natural: Parsley[BigInt] = new Parsley(singletons.Natural)
        lazy val integer: Parsley[BigInt] = sign(IntType) <*> natural
        lazy val unsignedFloat: Parsley[BigDecimal] = new Parsley(singletons.Float)
        lazy val float: Parsley[BigDecimal] = (sign(DoubleType) <*> unsignedFloat).label("float")
        lazy val naturalOrFloat: Parsley[Either[BigInt, BigDecimal]] = attempt(unsignedFloat.map(Right(_))) <|> natural.map(Left(_)).label("unsigned number")
        lazy val number: Parsley[Either[BigInt, BigDecimal]] =
                ('+' *> naturalOrFloat
            <|> '-' *> naturalOrFloat.map{ case Left(n) => Left(-n); case Right(f) => Right(-f) }
            <|> naturalOrFloat).label("number")
        lazy val decimal: Parsley[BigInt] = Lexer.this.number(base = 10, digit)
        lazy val hexadecimal: Parsley[BigInt] = '0' *> prefixedNumber('x', 16, hexDigit)
        lazy val octal: Parsley[BigInt] = '0' *> prefixedNumber('o', 8, octDigit)
        /* OLD NUMERIC API END */

        /* NEW NUMERIC API START */
        object numeric {
            // There are three categories of finite numeric literals: naturals, integers, and rationals
            // These are not particularly intuitive names for programmers and could be written as unsigned, signed, and floating
            // each of these categories has the option for:
            //   precisions
            //   decimal/hexadecimal/octal/binary
            //   a grouping of the above
            // Naturals
            def unsigned = natural
            object natural {
                private def ofRadix(radix: Int, digit: Parsley[Char]) = digit.foldLeft1[BigInt](0)((x, d) => x*radix + d.asDigit)

                val decimal: Parsley[BigInt] = ofRadix(10, digit)
                val hexadecimal: Parsley[BigInt] = attempt('0' *> oneOf('x', 'X') *> ofRadix(16, hexDigit))
                val octal: Parsley[BigInt] = attempt('0' *> oneOf('o', 'O') *> ofRadix(8, octDigit))
                val binary: Parsley[BigInt] = attempt('0' *> oneOf('b', 'B') *> ofRadix(2, oneOf('0', '1')))
                val number: Parsley[BigInt] = hexadecimal <|> octal <|> binary <|> decimal

                def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = numberBounded(_8)
                def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = decimalBounded(_8)
                def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = hexadecimalBounded(_8)
                def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = octalBounded(_8)
                def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = binaryBounded(_8)

                def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = numberBounded(_16)
                def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = decimalBounded(_16)
                def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = hexadecimalBounded(_16)
                def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = octalBounded(_16)
                def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = binaryBounded(_16)

                def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = numberBounded(_32)
                def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = decimalBounded(_32)
                def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = hexadecimalBounded(_32)
                def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = octalBounded(_32)
                def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = binaryBounded(_32)

                def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = numberBounded(_64)
                def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = decimalBounded(_64)
                def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = hexadecimalBounded(_64)
                def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = octalBounded(_64)
                def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = binaryBounded(_64)

                // TODO: render in the "native" radix
                private def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self, T]): Parsley[T] = {
                    number.collectMsg(x => Seq(s"literal $x is larger than the max value of ${bits.upperUnsigned}")) {
                        case x if x <= bits.upperUnsigned => ev.fromBigInt(x)
                    }
                }

                private def numberBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(number, bits, 10)
                private def decimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(decimal, bits, 10)
                private def hexadecimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(hexadecimal, bits, 16)
                private def octalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(octal, bits, 8)
                private def binaryBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(binary, bits, 2)
            }

            // Integers
            def signed = integer
            object integer {
                val decimal: Parsley[BigInt] = attempt(sign(IntType) <*> natural.number)
                val hexadecimal: Parsley[BigInt] = attempt(sign(IntType) <*> natural.hexadecimal)
                val octal: Parsley[BigInt] = attempt(sign(IntType) <*> natural.octal)
                val binary: Parsley[BigInt] = attempt(sign(IntType) <*> natural.binary)
                val number: Parsley[BigInt] = hexadecimal <|> octal <|> binary <|> decimal

                def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = numberBounded(_8)
                def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = decimalBounded(_8)
                def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = hexadecimalBounded(_8)
                def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = octalBounded(_8)
                def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = binaryBounded(_8)

                def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = numberBounded(_16)
                def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = decimalBounded(_16)
                def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = hexadecimalBounded(_16)
                def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = octalBounded(_16)
                def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = binaryBounded(_16)

                def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = numberBounded(_32)
                def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = decimalBounded(_32)
                def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = hexadecimalBounded(_32)
                def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = octalBounded(_32)
                def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = binaryBounded(_32)

                def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = numberBounded(_64)
                def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = decimalBounded(_64)
                def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = hexadecimalBounded(_64)
                def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = octalBounded(_64)
                def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = binaryBounded(_64)

                // TODO: render in the "native" radix
                private def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self, T]): Parsley[T] = {
                    number.collectMsg(x => Seq(if (x > bits.upperSigned) s"literal $x is larger than the max value of ${bits.upperSigned}"
                                               else                      s"literal $x is less than the min value of ${bits.lowerSigned}")) {
                        case x if bits.lowerSigned <= x && x <= bits.upperSigned => ev.fromBigInt(x)
                    }
                }

                private def numberBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(number, bits, 10)
                private def decimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(decimal, bits, 10)
                private def hexadecimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(hexadecimal, bits, 16)
                private def octalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(octal, bits, 8)
                private def binaryBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(binary, bits, 2)
            }

            // Rationals
            // TODO: We are going to support hexadecimal, binary, and octal floating point literals
            // decimal is 0[.6875][(e|E)[-|+]0] (configuration for whether + is allowed/mandatory in exponent) as well as e/E or other char and if that's enabled
            // hexadecimal is 0x0[.B][(p|P)[-|+]0] with the final part in decimal and pn is 2^n (configuration for whether the exponent is required and + and epEP)
            // binary is 0b0[.1011][(p|e|P|E)[-|+]0] with the final part in decimal and pn is 2^n (configuration for whether the exponent is required and + and epEP)
            // octal is 0o0[.54][(p|e|P|E)[-|+]0]
            def floating = rational
            object rational {

            }
        }
        /* NEW NUMERIC API END */
    }

    // TODO: Exposing this to the external API is a pain, because it's a path-dependent type, and violates private...
    // perhaps we make an interface for this?
    object implicits /*extends something?*/ {
        implicit def implicitLexeme(s: String): Parsley[Unit] = {
            if (lang.identDesc.keywords(s))       lexemes.keyword(s)
            else if (lang.operators(s))           lexemes.maxOp(s)
            else                                  lexemes.symbol_(s).void
        }
    }

    /**`lexeme(p)` first applies parser `p` and then the `whiteSpace` parser, returning the value of
     * `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
     * point without white space. The only point where the `whiteSpace` parser should be called
     * explicitly is the start of the main parser in order to skip any leading white space.*/
    def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* whiteSpace

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the `LanguageDef`), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the lexer.*/
    lazy val whiteSpace: Parsley[Unit] = whiteSpace_(lang.whitespaceDesc.space).hide

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the parameter), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the lexer.*/
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

    /**Parses any comments and skips them, this includes both line comments and block comments.*/
    lazy val skipComments: Parsley[Unit] = {
        if (!lang.whitespaceDesc.supportsComments) unit
        else {
            new Parsley(new singletons.SkipComments(lang.whitespaceDesc.commentStart, lang.whitespaceDesc.commentEnd,
                                                    lang.whitespaceDesc.commentLine,  lang.whitespaceDesc.nestedComments))
        }
    }

    // legacy API
    /**This lexeme parser parses a legal identifier. Returns the identifier string. This parser will
     * fail on identifiers that are reserved words (i.e. keywords). Legal identifier characters and
     * keywords are defined in the `LanguageDef` provided to the lexer. An identifier is treated
     * as a single token using `attempt`.*/
    @deprecated
    def identifier: Parsley[String] = lexemes.identifier

    /**The lexeme parser `keyword(name)` parses the symbol `name`, but it also checks that the `name`
     * is not a prefix of a valid identifier. A `keyword` is treated as a single token using `attempt`.*/
    @deprecated
    def keyword(name: String): Parsley[Unit] = lexemes.keyword(name)

    /**This lexeme parser parses a legal operator. Returns the name of the operator. This parser
     * will fail on any operators that are reserved operators. Legal operator characters and
     * reserved operators are defined in the `LanguageDef` provided to the lexer. A
     * `userOp` is treated as a single token using `attempt`.*/
    @deprecated
    def userOp: Parsley[String] = lexemes.userOp

    /**This non-lexeme parser parses a reserved operator. Returns the name of the operator.
     * Legal operator characters and reserved operators are defined in the `LanguageDef`
     * provided to the lexer. A `reservedOp_` is treated as a single token using `attempt`.*/
    @deprecated
    def reservedOp_ : Parsley[String] = nonlexemes.reservedOp

    /**This lexeme parser parses a reserved operator. Returns the name of the operator. Legal
     * operator characters and reserved operators are defined in the `LanguageDef` provided
     * to the lexer. A `reservedOp` is treated as a single token using `attempt`.*/
    @deprecated
    def reservedOp: Parsley[String] = lexemes.reservedOp

    /**The lexeme parser `operator(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    @deprecated
    def operator(name: String): Parsley[Unit] = lexemes.operator(name)

    /**The non-lexeme parser `operator_(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    @deprecated
    def operator_(name: String): Parsley[Unit] = nonlexemes.operator(name)

    /**The lexeme parser `maxOp(name)` parses the symbol `name`, but also checks that the `name`
      * is not part of a larger reserved operator. An `operator` is treated as a single token using
      * `attempt`.*/
    @deprecated
    def maxOp(name: String): Parsley[Unit] = lexemes.maxOp(name)

    /**The non-lexeme parser `maxOp_(name)` parses the symbol `name`, but also checks that the `name`
      * is not part of a larger reserved operator. An `operator` is treated as a single token using
      * `attempt`.*/
    @deprecated
    def maxOp_(name: String): Parsley[Unit] = nonlexemes.maxOp(name)

    /**This lexeme parser parses a single literal character. Returns the literal character value.
     * This parser deals correctly with escape sequences. The literal character is parsed according
     * to the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    @deprecated
    def charLiteral: Parsley[Char] = lexemes.charLiteral

    /**This lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    @deprecated
    def stringLiteral: Parsley[String] = lexemes.stringLiteral

    /**This non-lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    @deprecated
    def stringLiteral_ : Parsley[String] = nonlexemes.stringLiteral

    /**This non-lexeme parser parses a string in a raw fashion. The escape characters in the string
     * remain untouched. While escaped quotes do not end the string, they remain as \" in the result
     * instead of becoming a quote character. Does not support string gaps. */
    @deprecated
    def rawStringLiteral: Parsley[String] = nonlexemes.rawStringLiteral

    /**This lexeme parser parses a natural number (a positive whole number). Returns the value of
     * the number. The number can specified in `decimal`, `hexadecimal` or `octal`. The number is
     * parsed according to the grammar rules in the Haskell report.*/
    @deprecated
    def natural: Parsley[Int] = lexemes.natural.map(_.toInt)

    /**This lexeme parser parses an integer (a whole number). This parser is like `natural` except
     * that it can be prefixed with a sign (i.e '-' or '+'). Returns the value of the number. The
     * number can be specified in `decimal`, `hexadecimal` or `octal`. The number is parsed
     * according to the grammar rules in the haskell report.*/
    @deprecated
    def integer: Parsley[Int] = lexemes.integer.map(_.toInt)

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report.*/
    @deprecated
    def unsignedFloat: Parsley[Double] = lexemes.unsignedFloat.map(_.toDouble)

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report. Accepts an optional
     * '+' or '-' sign.*/
    @deprecated
    def float: Parsley[Double] = lexemes.float.map(_.toDouble)

    /**This lexeme parser parses either `integer` or `float`. Returns the value of the number. This
     * parser deals with any overlap in the grammar rules for naturals and floats. The number is
     * parsed according to the grammar rules defined in the Haskell report.*/
    @deprecated
    def number: Parsley[Either[Int, Double]] = lexemes.number.map(_.fold(x => Left(x.toInt), y => Right(y.toDouble)))

    /**This lexeme parser parses either `natural` or `unsigned float`. Returns the value of the number. This
      * parser deals with any overlap in the grammar rules for naturals and floats. The number is
      * parsed according to the grammar rules defined in the Haskell report.*/
    @deprecated
    def naturalOrFloat: Parsley[Either[Int, Double]] = lexemes.naturalOrFloat.map(_.fold(x => Left(x.toInt), y => Right(y.toDouble)))

    /**Parses a positive whole number in the decimal system. Returns the value of the number.*/
    @deprecated
    def decimal: Parsley[Int] = lexemes.decimal.map(_.toInt)

    /**Parses a positive whole number in the hexadecimal system. The number should be prefixed with
     * "0x" or "0X". Returns the value of the number.*/
    @deprecated
    def hexadecimal: Parsley[Int] = lexemes.hexadecimal.map(_.toInt)

    /**Parses a positive whole number in the octal system. The number should be prefixed with "0o"
     * or "0O". Returns the value of the number.*/
    @deprecated
    def octal: Parsley[Int] = lexemes.octal.map(_.toInt)

    /**Lexeme parser `symbol(s)` parses `string(s)` and skips trailing white space.*/
    @deprecated
    def symbol(name: String): Parsley[String] = lexemes.symbol(name)
    /**Lexeme parser `symbol(c)` parses `char(c)` and skips trailing white space.*/
    @deprecated
    def symbol(name: Char): Parsley[Char] = lexemes.symbol(name)

    /**Like `symbol`, but treats it as a single token using `attempt`. Only useful for
     * strings, since characters are already single token.*/
    @deprecated
    def symbol_(name: String): Parsley[String] = lexemes.symbol_(name)

    /**Lexeme parser `parens(p)` parses `p` enclosed in parenthesis, returning the value of `p`.*/
    @deprecated
    def parens[A](p: =>Parsley[A]): Parsley[A] = lexemes.parens(p)

    /**Lexeme parser `braces(p)` parses `p` enclosed in braces ('{', '}'), returning the value of 'p'*/
    @deprecated
    def braces[A](p: =>Parsley[A]): Parsley[A] = lexemes.braces(p)

    /**Lexeme parser `angles(p)` parses `p` enclosed in angle brackets ('<', '>'), returning the
     * value of `p`.*/
    @deprecated
    def angles[A](p: =>Parsley[A]): Parsley[A] = lexemes.angles(p)

    /**Lexeme parser `brackets(p)` parses `p` enclosed in brackets ('[', ']'), returning the value
     * of `p`.*/
    @deprecated
    def brackets[A](p: =>Parsley[A]): Parsley[A] = lexemes.brackets(p)

    /**Lexeme parser `semi` parses the character ';' and skips any trailing white space. Returns ";"*/
    @deprecated
    def semi: Parsley[Char] = lexemes.semi

    /**Lexeme parser `comma` parses the character ',' and skips any trailing white space. Returns ","*/
    @deprecated
    def comma: Parsley[Char] = lexemes.comma

    /**Lexeme parser `colon` parses the character ':' and skips any trailing white space. Returns ":"*/
    @deprecated
    def colon: Parsley[Char] = lexemes.colon

    /**Lexeme parser `dot` parses the character '.' and skips any trailing white space. Returns "."*/
    @deprecated
    def dot: Parsley[Char] = lexemes.dot

    /**Lexeme parser `semiSep(p)` parses zero or more occurrences of `p` separated by `semi`. Returns
     * a list of values returned by `p`.*/
    @deprecated
    def semiSep[A](p: Parsley[A]): Parsley[List[A]] = lexemes.semiSep(p)

    /**Lexeme parser `semiSep1(p)` parses one or more occurrences of `p` separated by `semi`. Returns
     * a list of values returned by `p`.*/
    @deprecated
    def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = lexemes.semiSep1(p)

    /**Lexeme parser `commaSep(p)` parses zero or more occurrences of `p` separated by `comma`.
     * Returns a list of values returned by `p`.*/
    @deprecated
    def commaSep[A](p: Parsley[A]): Parsley[List[A]] = lexemes.commaSep(p)

    /**Lexeme parser `commaSep1(p)` parses one or more occurrences of `p` separated by `comma`.
     * Returns a list of values returned by `p`.*/
    @deprecated
    def commaSep1[A](p: Parsley[A]): Parsley[List[A]] = lexemes.commaSep1(p)

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
    private lazy val identStart = toParser(lang.identDesc.identStart)
    private lazy val identLetter = toParser(lang.identDesc.identLetter)
    private lazy val ident = lift2((c: Char, cs: String) => s"$c$cs", identStart, stringOfMany(identLetter))

    // Operators & Reserved ops
    private lazy val opStart = toParser(lang.opStart)
    private lazy val opLetter = toParser(lang.opLetter)
    private lazy val oper = lift2((c: Char, cs: String) => s"$c$cs", opStart, stringOfMany(opLetter))

    // Chars & Strings
    private def letter(terminal: Char): Parsley[Char] = satisfy(c => c != terminal && c != '\\' && c > '\u0016')

    private lazy val escapeCode = new Parsley(singletons.Escape)
    private lazy val charEscape = '\\' *> escapeCode
    private lazy val charLetter = letter('\'')
    private lazy val characterChar = (charLetter <|> charEscape).label("literal character")

    private val escapeEmpty = '&'
    private lazy val escapeGap = skipSome(space.label("string gap")) *> '\\'.label("end of string gap")
    private lazy val stringLetter = letter('"')
    private lazy val stringEscape: Parsley[Option[Char]] = {
        '\\' *> (escapeGap #> None
             <|> escapeEmpty #> None
             <|> (escapeCode.map(Some(_))).explain("invalid escape sequence"))
    }
    private lazy val stringChar: Parsley[Option[Char]] = ((stringLetter.map(Some(_))) <|> stringEscape).label("string character")

    // Numbers
    private def sign(ty: SignType) = new Parsley(new singletons.Sign[ty.resultType](ty))
    private def number(base: Int, baseDigit: Parsley[Char]): Parsley[BigInt] = baseDigit.foldLeft1[BigInt](0)((x, d) => x*base + d.asDigit)
    private def prefixedNumber(prefix: Char, base: Int, digit: Parsley[Char]) = satisfy(c => c.toLower == prefix.toLower) *> number(base, digit)

    // White space & symbols
    private lazy val space = toParser(lang.whitespaceDesc.space)

    private def toParser(e: Impl) = e match {
        case NotRequired => empty
        case Static(f)   => satisfy(f)
        case Parser(p)   => p.asInstanceOf[Parsley[Char]]
        // $COVERAGE-OFF$
        case _ => ??? // scalastyle:ignore not.implemented.error.usage
        // $COVERAGE-ON$
    }
}
