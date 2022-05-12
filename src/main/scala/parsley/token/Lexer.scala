/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import scala.language.implicitConversions

import parsley.Parsley, Parsley.{attempt, empty, notFollowedBy, pure, unit}
import parsley.character.{char, digit, hexDigit, octDigit, satisfy, string}
import parsley.combinator.{between, many, sepBy, sepBy1, skipMany, skipSome, some}
import parsley.errors.combinator.{amend, entrench, fail, unexpected, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.lift.lift2

import parsley.internal.deepembedding.Sign.{DoubleType, IntType, SignType}
import parsley.internal.deepembedding.singletons

/**
  * When provided with a `LanguageDef`, this class will produce a large variety of parsers that can be used for
  * tokenisation of a language. This includes parsing numbers and strings in their various formats and ensuring that
  * all operations consume whitespace after them (so-called lexeme parsers). These are very useful in parsing
  * programming languages. This class also has a large number of hand-optimised intrinsic parsers to improve performance!
  * @param lang The rules that govern the language we are tokenising
  * @since 2.2.0
  */
class Lexer(lang: LanguageDef)
{
    private def keyOrOp(startImpl: Impl, letterImpl: Impl, parser: Parsley[String], illegal: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        val builder = (start: Char => Boolean, letter: Char => Boolean) =>
            new Parsley(new singletons.NonSpecific(combinatorName, name, illegalName, start, letter, illegal))
        lexeme {
            (startImpl, letterImpl) match {
                case (Static(start), Static(letter)) => builder(start, letter)
                case _ =>
                    attempt {
                        amend {
                            entrench(parser).flatMap {
                                case x if illegal(x) => unexpected(s"$illegalName $x")
                                case x => pure(x)
                            }
                        }
                    }.label(name)
            }
        }
    }

    // Identifiers & Reserved words
    /**This lexeme parser parses a legal identifier. Returns the identifier string. This parser will
     * fail on identifiers that are reserved words (i.e. keywords). Legal identifier characters and
     * keywords are defined in the `LanguageDef` provided to the lexer. An identifier is treated
     * as a single token using `attempt`.*/
    lazy val identifier: Parsley[String] = keyOrOp(lang.identStart, lang.identLetter, ident, isReservedName(_),  "identifier", "identifier", "keyword")

    /**The lexeme parser `keyword(name)` parses the symbol `name`, but it also checks that the `name`
     * is not a prefix of a valid identifier. A `keyword` is treated as a single token using `attempt`.*/
    def keyword(name: String): Parsley[Unit] = lang.identLetter match
    {
        case Static(letter) => lexeme(new Parsley(new singletons.Specific("keyword", name, letter, lang.caseSensitive)))
        case _ => lexeme(attempt(caseString(name) *> notFollowedBy(identLetter).label("end of " + name)))
    }

    private def caseString(name: String): Parsley[String] =
    {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) c.toLower <|> c.toUpper else c
        if (lang.caseSensitive) string(name)
        else name.foldLeft(pure(name))((p, c) => p <* caseChar(c)).label(name)
    }
    private def isReservedName(name: String): Boolean = theReservedNames.contains(if (lang.caseSensitive) name else name.toLowerCase)
    private val theReservedNames =  if (lang.caseSensitive) lang.keywords else lang.keywords.map(_.toLowerCase)
    private lazy val identStart = toParser(lang.identStart)
    private lazy val identLetter = toParser(lang.identLetter)
    private lazy val ident = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, identStart, many(identLetter))

    // Operators & Reserved ops
    /**This lexeme parser parses a legal operator. Returns the name of the operator. This parser
     * will fail on any operators that are reserved operators. Legal operator characters and
     * reserved operators are defined in the `LanguageDef` provided to the lexer. A
     * `userOp` is treated as a single token using `attempt`.*/
    lazy val userOp: Parsley[String] = keyOrOp(lang.opStart, lang.opLetter, oper, isReservedOp(_), "userOp", "operator", "reserved operator")

    /**This non-lexeme parser parses a reserved operator. Returns the name of the operator.
     * Legal operator characters and reserved operators are defined in the `LanguageDef`
     * provided to the lexer. A `reservedOp_` is treated as a single token using `attempt`.*/
    lazy val reservedOp_ : Parsley[String] = keyOrOp(lang.opStart, lang.opLetter, oper, !isReservedOp(_), "reservedOp", "operator", "non-reserved operator")

    /**This lexeme parser parses a reserved operator. Returns the name of the operator. Legal
     * operator characters and reserved operators are defined in the `LanguageDef` provided
     * to the lexer. A `reservedOp` is treated as a single token using `attempt`.*/
    lazy val reservedOp: Parsley[String] = lexeme(reservedOp_)

    /**The lexeme parser `operator(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    def operator(name: String): Parsley[Unit] = lexeme(operator_(name))

    /**The non-lexeme parser `operator_(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    def operator_(name: String): Parsley[Unit] = lang.opLetter match
    {
        case Static(letter) => new Parsley(new singletons.Specific("operator", name, letter, true))
        case _ => attempt(string(name) *> notFollowedBy(opLetter).label("end of " + name))
    }

    /**The lexeme parser `maxOp(name)` parses the symbol `name`, but also checks that the `name`
      * is not part of a larger reserved operator. An `operator` is treated as a single token using
      * `attempt`.*/
    def maxOp(name: String): Parsley[Unit] = lexeme(maxOp_(name))

    /**The non-lexeme parser `maxOp_(name)` parses the symbol `name`, but also checks that the `name`
      * is not part of a larger reserved operator. An `operator` is treated as a single token using
      * `attempt`.*/
    def maxOp_(name: String): Parsley[Unit] = new Parsley(new singletons.MaxOp(name, lang.operators)).void

    private def isReservedOp(op: String): Boolean = lang.operators.contains(op)
    private lazy val opStart = toParser(lang.opStart)
    private lazy val opLetter = toParser(lang.opLetter)
    private lazy val oper = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, opStart, many(opLetter))

    // Chars & Strings
    /**This lexeme parser parses a single literal character. Returns the literal character value.
     * This parser deals correctly with escape sequences. The literal character is parsed according
     * to the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val charLiteral: Parsley[Char] = lexeme(between('\''.label("character"), '\''.label("end of character"), characterChar))

    /**This lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val stringLiteral: Parsley[String] = lexeme(stringLiteral_)

    /**This non-lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val stringLiteral_ : Parsley[String] = lang.space match
    {
        case Static(ws) => new Parsley(new singletons.StringLiteral(ws))
        case _ => between('"'.label("string"), '"'.label("end of string"), many(stringChar)).map(_.flatten.mkString)
    }

    /**This non-lexeme parser parses a string in a raw fashion. The escape characters in the string
     * remain untouched. While escaped quotes do not end the string, they remain as \" in the result
     * instead of becoming a quote character. Does not support string gaps. */
    lazy val rawStringLiteral: Parsley[String] = new Parsley(singletons.RawStringLiteral)

    private def letter(terminal: Char): Parsley[Char] = satisfy(c => c != terminal && c != '\\' && c > '\u0016')

    private lazy val escapeCode = new Parsley(singletons.Escape)
    private lazy val charEscape = '\\' *> escapeCode
    private lazy val charLetter = letter('\'')
    private lazy val characterChar = (charLetter <|> charEscape).label("literal character")

    private val escapeEmpty = '&'
    private lazy val escapeGap = skipSome(space.label("string gap")) *> '\\'.label("end of string gap")
    private lazy val stringLetter = letter('"')
    private lazy val stringEscape: Parsley[Option[Char]] =
    {
        '\\' *> (escapeGap #> None
             <|> escapeEmpty #> None
             <|> (escapeCode.map(Some(_))).explain("invalid escape sequence"))
    }
    private lazy val stringChar: Parsley[Option[Char]] = ((stringLetter.map(Some(_))) <|> stringEscape).label("string character")

    // Numbers
    /**This lexeme parser parses a natural number (a positive whole number). Returns the value of
     * the number. The number can specified in `decimal`, `hexadecimal` or `octal`. The number is
     * parsed according to the grammar rules in the Haskell report.*/
    lazy val natural: Parsley[Int] = lexeme(nat)

    /**This lexeme parser parses an integer (a whole number). This parser is like `natural` except
     * that it can be prefixed with a sign (i.e '-' or '+'). Returns the value of the number. The
     * number can be specified in `decimal`, `hexadecimal` or `octal`. The number is parsed
     * according to the grammar rules in the haskell report.*/
    lazy val integer: Parsley[Int] = lexeme(int.label("integer"))

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report.*/
    lazy val unsignedFloat: Parsley[Double] = lexeme(floating)

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report. Accepts an optional
     * '+' or '-' sign.*/
    lazy val float: Parsley[Double] = lexeme(signedFloating.label("float"))

    /**This lexeme parser parses either `integer` or `float`. Returns the value of the number. This
     * parser deals with any overlap in the grammar rules for naturals and floats. The number is
     * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val number: Parsley[Either[Int, Double]] = lexeme(number_.label("number"))

    /**This lexeme parser parses either `natural` or `unsigned float`. Returns the value of the number. This
      * parser deals with any overlap in the grammar rules for naturals and floats. The number is
      * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val naturalOrFloat: Parsley[Either[Int, Double]] = lexeme(natFloat.label("unsigned number"))

    private lazy val decimal_ = number(base = 10, digit)

    private def prefixedNumber(prefix: Char, base: Int, digit: Parsley[Char]) = satisfy(c => c.toLower == prefix.toLower) *> number(base, digit)
    private lazy val hexadecimal_ = prefixedNumber('x', 16, hexDigit)
    private lazy val octal_ = prefixedNumber('o', 8, octDigit)

    // Floats
    private def sign(ty: SignType) = new Parsley(new singletons.Sign[ty.resultType](ty))
    private lazy val floating = new Parsley(singletons.Float)
    private lazy val signedFloating = sign(DoubleType) <*> floating
    private lazy val natFloat = attempt(floating.map(Right(_))) <|> nat.map(Left(_))
    private lazy val number_ =
        ('+' *> natFloat
     <|> '-' *> natFloat.map{ case Left(n) => Left(-n); case Right(f) => Right(-f) }
     <|> natFloat)

    // Integers and Naturals
    private lazy val nat = new Parsley(singletons.Natural)
    private lazy val int = sign(IntType) <*> nat

    /**Parses a positive whole number in the decimal system. Returns the value of the number.*/
    lazy val decimal: Parsley[Int] = lexeme(decimal_)

    /**Parses a positive whole number in the hexadecimal system. The number should be prefixed with
     * "0x" or "0X". Returns the value of the number.*/
    lazy val hexadecimal: Parsley[Int] = lexeme('0' *> hexadecimal_)

    /**Parses a positive whole number in the octal system. The number should be prefixed with "0o"
     * or "0O". Returns the value of the number.*/
    lazy val octal: Parsley[Int] = lexeme('0' *> octal_)

    private def number(base: Int, baseDigit: Parsley[Char]): Parsley[Int] = baseDigit.foldLeft1(0)((x, d) => base*x + d.asDigit)

    // White space & symbols
    /**Lexeme parser `symbol(s)` parses `string(s)` and skips trailing white space.*/
    def symbol(name: String): Parsley[String] = lexeme(string(name))
    /**Lexeme parser `symbol(c)` parses `char(c)` and skips trailing white space.*/
    def symbol(name: Char): Parsley[Char] = lexeme(char(name))

    /**Like `symbol`, but treats it as a single token using `attempt`. Only useful for
     * strings, since characters are already single token.*/
    def symbol_(name: String): Parsley[String] = lexeme(attempt(string(name)))

    /**`lexeme(p)` first applies parser `p` and then the `whiteSpace` parser, returning the value of
     * `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
     * point without white space. The only point where the `whiteSpace` parser should be called
     * explicitly is the start of the main parser in order to skip any leading white space.*/
    def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* whiteSpace

    private lazy val space = toParser(lang.space)

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the `LanguageDef`), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the lexer.*/
    lazy val whiteSpace: Parsley[Unit] = whiteSpace_(lang.space).hide

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the parameter), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the lexer.*/
    val whiteSpace_ : Impl => Parsley[Unit] =
    {
        case NotRequired => skipComments
        case Static(ws) => new Parsley(new singletons.WhiteSpace(ws, lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments))
        case Parser(space_) if lang.supportsComments =>
            skipMany(attempt(new Parsley(new singletons.Comment(lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments))) <|> space_)
        case Parser(space_) => skipMany(space_)
        // $COVERAGE-OFF$
        case _ => ??? // scalastyle:ignore not.implemented.error.usage
        // $COVERAGE-ON$
    }

    /**Parses any comments and skips them, this includes both line comments and block comments.*/
    lazy val skipComments: Parsley[Unit] = {
        if (!lang.supportsComments) unit
        else {
            new Parsley(new singletons.SkipComments(lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments))
        }
    }

    private def enclosing[A](p: =>Parsley[A], open: Char, close: Char, singular: String, plural: String) =
        between(lexeme(open.label(s"open $singular")),
                lexeme(close.label(s"matching closing $singular").explain(s"unclosed $plural")),
                p)

    // Bracketing
    /**Lexeme parser `parens(p)` parses `p` enclosed in parenthesis, returning the value of `p`.*/
    def parens[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '(', ')', "parenthesis", "parentheses")

    /**Lexeme parser `braces(p)` parses `p` enclosed in braces ('{', '}'), returning the value of 'p'*/
    def braces[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '{', '}', "brace", "braces")

    /**Lexeme parser `angles(p)` parses `p` enclosed in angle brackets ('<', '>'), returning the
     * value of `p`.*/
    def angles[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '<', '>', "angle bracket", "angle brackets")

    /**Lexeme parser `brackets(p)` parses `p` enclosed in brackets ('[', ']'), returning the value
     * of `p`.*/
    def brackets[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '[', ']', "square bracket", "square brackets")

    /**Lexeme parser `semi` parses the character ';' and skips any trailing white space. Returns ";"*/
    val semi: Parsley[Char] = lexeme(';'.label("semicolon"))

    /**Lexeme parser `comma` parses the character ',' and skips any trailing white space. Returns ","*/
    val comma: Parsley[Char] = lexeme(','.label("comma"))

    /**Lexeme parser `colon` parses the character ':' and skips any trailing white space. Returns ":"*/
    val colon: Parsley[Char] = lexeme(':'.label("colon"))

    /**Lexeme parser `dot` parses the character '.' and skips any trailing white space. Returns "."*/
    val dot: Parsley[Char] = lexeme('.'.label("dot"))

    /**Lexeme parser `semiSep(p)` parses zero or more occurrences of `p` separated by `semi`. Returns
     * a list of values returned by `p`.*/
    def semiSep[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy(p, semi)

    /**Lexeme parser `semiSep1(p)` parses one or more occurrences of `p` separated by `semi`. Returns
     * a list of values returned by `p`.*/
    def semiSep1[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy1(p, semi)

    /**Lexeme parser `commaSep(p)` parses zero or more occurrences of `p` separated by `comma`.
     * Returns a list of values returned by `p`.*/
    def commaSep[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy(p, comma)

    /**Lexeme parser `commaSep1(p)` parses one or more occurrences of `p` separated by `comma`.
     * Returns a list of values returned by `p`.*/
    def commaSep1[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy1(p, comma)

    private def toParser(e: Impl) = e match
    {
        case NotRequired => empty
        case Static(f)   => satisfy(f)
        case Parser(p)   => p.asInstanceOf[Parsley[Char]]
        // $COVERAGE-OFF$
        case _ => ??? // scalastyle:ignore not.implemented.error.usage
        // $COVERAGE-ON$
    }
}
