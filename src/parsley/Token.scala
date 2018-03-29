package parsley

import Parsley._
import Combinator.{between, choice, notFollowedBy, sepBy, sepBy1, skipSome, some, decide}
import Char.{charLift, digit, hexDigit, noneOf, octDigit, oneOf, satisfy, stringLift, upper}

import scala.util.Try

final case class LanguageDef(commentStart: String,
                             commentEnd: String,
                             commentLine: String,
                             nestedComments: Boolean,
                             identStart: Parsley[Char],
                             identLetter: Parsley[Char],
                             opStart: Parsley[Char],
                             opLetter: Parsley[Char],
                             keywords: Set[String],
                             operators: Set[String],
                             caseSensitive: Boolean,
                             space: Parsley[_])
                                        
final class TokenParser(languageDef: LanguageDef)
{
    // Identifiers & Reserved words
    /**This lexeme parser parses a legal identifier. Returns the identifier string. This parser will
     * fail on identifiers that are reserved words (i.e. keywords). Legal identifier characters and
     * keywords are defined in the `LanguageDef` provided to the token parser. An identifier is treated
     * as a single token using `attempt`.*/
    lazy val identifier: Parsley[String] = lexeme(attempt(ident >?> (!isReservedName(_), "keyword " + _)))
    
    /**The lexeme parser `keyword(name)` parses the symbol `name`, but it also checks that the `name`
     * is not a prefix of a valid identifier. A `keyword` is treated as a single token using `attempt`.*/
    def keyword(name: String): Parsley[Unit] = lexeme(attempt(caseString(name) *> notFollowedBy(languageDef.identLetter) ? ("end of " + name)))
    
    private def caseString(name: String): Parsley[String] =
    {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) c.toLower <|> c.toUpper else c
        if (languageDef.caseSensitive) name
        else name.foldRight(pure(name))((c, p) => caseChar(c) *> p) ? name
    }
    private def isReservedName(name: String): Boolean = theReservedNames.contains(if (languageDef.caseSensitive) name else name.toLowerCase)
    private lazy val theReservedNames =  if (languageDef.caseSensitive) languageDef.keywords else languageDef.keywords.map(_.toLowerCase)
    private lazy val ident = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, languageDef.identStart, many(languageDef.identLetter)) ? "identifier"

    // Operators & Reserved ops
    /**This lexeme parser parses a legal operator. Returns the name of the operator. This parser
     * will fail on any operators that are reserved operators. Legal operator characters and
     * reserved operators are defined in the `LanguageDef` provided to the token parser. A
     * `userOp` is treated as a single token using `attempt`.*/
    lazy val userOp: Parsley[String] = lexeme(attempt(oper >?> (!isReservedOp(_), "reserved operator " + _)))

    /**This non-lexeme parser parses a reserved operator. Returns the name of the operator. This parser
      * will fail on any operators that are reserved operators. Legal operator characters and
      * reserved operators are defined in the `LanguageDef` provided to the token parser. A
      * `reservedOp_` is treated as a single token using `attempt`.*/
    lazy val reservedOp_ : Parsley[String] = attempt(oper >?> (isReservedOp, "non-reserved operator " + _))

    /**This lexeme parser parses a reserved operator. Returns the name of the operator. This parser
     * will fail on any operators that are reserved operators. Legal operator characters and
     * reserved operators are defined in the `LanguageDef` provided to the token parser. A
     * `reservedOp` is treated as a single token using `attempt`.*/
    lazy val reservedOp: Parsley[String] = lexeme(reservedOp_)

    /**The lexeme parser `operator(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using 
     * `attempt`.*/
    def operator(name: String): Parsley[Unit] = lexeme(operator_(name))
    
    /**The lexeme parser `operator_(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using 
     * `attempt`.*/
    def operator_(name: String): Parsley[Unit] = attempt(name *> notFollowedBy(languageDef.opLetter) ? ("end of " + name))

    private def isReservedOp(op: String): Boolean = languageDef.operators.contains(op)
    private lazy val oper = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, languageDef.opStart, many(languageDef.opLetter)) ? "operator"
    
    // Chars & Strings
    /**This lexeme parser parses a single literal character. Returns the literal character value.
     * This parser deals correctly with escape sequences. The literal character is parsed according
     * to the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val charLiteral: Parsley[Char] = lexeme(between('\'', '\'' ? "end of character", characterChar)) ? "character"
    
    /**This lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val stringLiteral: Parsley[String] = lexeme(stringLiteral_)
    
    /**This non-lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val stringLiteral_ : Parsley[String] = between('"' ? "string", '"' ? "end of string", many(stringChar)) <#> (_.flatten.mkString)
    
    /**This non-lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val rawStringLiteral: Parsley[String] = between('"' ? "string", '"' ? "end of string", many(stringLetter_)) <#> (_.mkString)

    private lazy val decimal_ = number(10, digit)
    private lazy val charControl = '^' *> upper.map(c => (c - 'A' + 1).toChar)
    private lazy val charNum =
    { 
        (decimal_
     <|> 'o' *> number(8, octDigit)
     <|> 'x' *> number(16, hexDigit)) >?> (_ <= 0x10FFFF, _ => "invalid escape sequence") <#> (_.toChar)
    }

    private val ascii2codes = List("BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP")
    private val ascii3codes = List("NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                                   "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                                   "CAN", "SUB", "ESC", "DEL")
    private val ascii2 = List('\u0008', '\u0009', '\n', '\u000b', '\u000c', '\u000d', '\u000e', '\u000f',
                              '\u0019', '\u001c', '\u001d', '\u001e', '\u001f', '\u0020')
    private val ascii3 = List('\u0000', '\u0001', '\u0002', '\u0003', '\u0004', '\u0005', '\u0006',
                              '\u0007', '\u0010', '\u0011', '\u0012', '\u0013', '\u0014', '\u0015',
                              '\u0016', '\u0017', '\u0018', '\u001a', '\u001b', '\u001f')
    private val escMap = "abfnrtv\\\"\'".toList zip "\u0007\b\u000c\n\r\t\u000b\\\"\'".toList
    private val asciiMap = ascii3codes ++ ascii2codes zip ascii3 ++ ascii2
    private lazy val charEsc = choice(escMap.map{case (c, code) => c #> code})
    private lazy val charAscii = choice(asciiMap.map{case (asc, code) => attempt(asc #> code)})
    private lazy val escapeCode = (charEsc <|> charNum <|> charAscii <\> charControl) ? "escape code"
    private lazy val charEscape = '\\' *> escapeCode
    private lazy val charLetter = satisfy(c => (c != '\'') && (c != '\\') && (c > '\u0016'))
    private lazy val characterChar = (charLetter <|> charEscape) ? "literal character"
    
    private val escapeEmpty = '&'
    private lazy val escapeGap = skipSome(languageDef.space) *> '\\' ? "end of string gap"
    private lazy val stringLetter = satisfy(c => (c != '"') && (c != '\\') && (c > '\u0016'))
    private lazy val stringLetter_ = satisfy(c => (c != '"') && (c > '\u0016'))
    private lazy val stringEscape: Parsley[Option[Char]] =
    {
        '\\' *> (escapeGap #> None
             <|> escapeEmpty #> None
             <|> (escapeCode <#> (Some(_))))
    }
    private lazy val stringChar: Parsley[Option[Char]] = ((stringLetter <#> (Some(_))) <|> stringEscape) ? "string character"
    
    // Numbers
    /**This lexeme parser parses a natural number (a positive whole number). Returns the value of
     * the number. The number can specified in `decimal`, `hexadecimal` or `octal`. The number is
     * parsed according to the grammar rules in the Haskell report.*/
    lazy val natural: Parsley[Int] = lexeme(nat) ? "natural"
    
    /**This lexeme parser parses an integer (a whole number). This parser is like `natural` except
     * that it can be prefixed with a sign (i.e '-' or '+'). Returns the value of the number. The
     * number can be specified in `decimal`, `hexadecimal` or `octal`. The number is parsed
     * according to the grammar rules in the haskell report.*/
    lazy val integer: Parsley[Int] = lexeme(int) ? "integer"
    
    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report.*/
    lazy val float: Parsley[Double] = lexeme(floating) ? "float"
    
    /**This lexeme parser parses either `natural` or `float`. Returns the value of the number. This
     * parser deals with any overlap in the grammar rules for naturals and floats. The number is 
     * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val naturalOrFloat: Parsley[Either[Int, Double]] = lexeme(natFloat) ? "number"
    
    private lazy val hexadecimal_ = oneOf(Set('x', 'X')) *> number(16, hexDigit)
    private lazy val octal_ = oneOf(Set('o', 'O')) *> number(8, octDigit)

    // Floats
    private lazy val fraction = ('.' <::> some(digit)).map(_.mkString) ? "fraction"
    private lazy val exponent_ =
    {
        val sign = '+' #> "+" <|> '-' #> "-" </> ""
        oneOf(Set('e', 'E')) *> lift2((sign: String, exp: Int) => 'e' + sign + exp, sign, decimal_) ? "exponent"
    }
    private def fractExponent(n: Int) =
    {
        def readDouble(s: String) = Try(s.toDouble).toOption
        val fractExp: Parsley[Option[Double]] =
            (lift2((fract: String, expo: String) => readDouble(n + fract + expo), fraction, exponent_.getOrElse(""))
         <|> exponent_.map(expo => readDouble(n + expo)))
        decide(fractExp)
    }
    private lazy val floating = decimal_ >>= (n => fractExponent(n))
    private def fractFloat(n: Int) = fractExponent(n) <#> (Right(_))
    private lazy val decimalFloat = decimal_ >>= (n => fractFloat(n).getOrElse(Left(n)))
    private lazy val zeroNumFloat =
    {
        ((hexadecimal_ <|> octal_) <#> (Left(_))) <|> decimalFloat <|> fractFloat(0) </> Left(0)
    }
    private lazy val natFloat = '0' *> zeroNumFloat <|> decimalFloat
    
    // Integers and Naturals
    // Original Parsec defines sign as a lexeme here, this is considered by many as a bug
    private lazy val zeroNumber = ('0' *> (hexadecimal_ <|> octal_ <|> decimal_ </> 0)) ? ""
    private lazy val nat = zeroNumber <|> decimal_
    private lazy val sign = ('-' #> ((x: Int) => -x)
                         <|> '+' #> ((x: Int) => x)
                         </> identity[Int] _)
    private lazy val int = sign <*> nat
    
    /**Parses a positive whole number in the decimal system. Returns the value of the number.*/
    lazy val decimal: Parsley[Int] = lexeme(decimal_)
    
    /**Parses a positive whole number in the hexadecimal system. The number should be prefixed with 
     * "0x" or "0X". Returns the value of the number.*/
    lazy val hexadecimal: Parsley[Int] = lexeme('0' *> hexadecimal_)
    
    /**Parses a positive whole number in the octal system. The number should be prefixed with "0o"
     * or "0O". Returns the value of the number.*/
    lazy val octal: Parsley[Int] = lexeme('0' *> octal_)
    
    private def number(base: Int, baseDigit: Parsley[Char]): Parsley[Int] =
    {
        for (digits <- some(baseDigit)) yield digits.foldLeft(0)((x, d) => base*x + d.asDigit)
    }
    
    // White space & symbols
    /**Lexeme parser `symbol(s)` parses `string(s)` and skips trailing white space.*/
    def symbol(name: String): Parsley[String] = lexeme[String](name)
    
    /**Like `symbol`, but treats it as a single token using `attempt`*/
    def symbol_(name: String): Parsley[String] = attempt(symbol(name))
    
    /**`lexeme(p)` first applies parser `p` and then the `whiteSpace` parser, returning the value of
     * `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
     * point without white space. The only point where the `whiteSpace` parser should be called
     * explicitly is the start of the main parser in order to skip any leading white space.*/
    def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* whiteSpace
    
    private lazy val inCommentMulti: Parsley[Unit] =
            (languageDef.commentEnd *> unit
         <\> multiLineComment *> inCommentMulti
         <|> skipSome(noneOf(startEnd)) *> inCommentMulti
         <|> oneOf(startEnd) *> inCommentMulti ? "end of comment")
    private val startEnd: Set[Char] = (languageDef.commentEnd + languageDef.commentStart).toSet
    private lazy val inCommentSingle: Parsley[Unit] =
            (languageDef.commentEnd *> unit
         <\> skipSome(noneOf(startEnd)) *> inCommentSingle
         <|> oneOf(startEnd) *> inCommentSingle ? "end of comment")
    private lazy val inComment = if (languageDef.nestedComments) inCommentMulti else inCommentSingle
    private lazy val oneLineComment = attempt(languageDef.commentLine) *> skipMany(satisfy(_!='\n'))
    private lazy val multiLineComment: Parsley[Unit] = attempt(languageDef.commentStart) *> inComment
    
    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the `LanguageDef`), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the token parser.*/
    lazy val whiteSpace: Parsley[Unit] =
    {
        val space = languageDef.space
        val noLine = languageDef.commentLine.isEmpty
        val noMulti = languageDef.commentStart.isEmpty
        if (noLine && noMulti) skipMany(space ? "")
        else if (noLine)       skipMany((space <|> multiLineComment) ? "")
        else if (noMulti)      skipMany((space <|> oneLineComment) ? "")
        else                   skipMany((space <|> multiLineComment <|> oneLineComment) ? "")
    }
    
    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the parameter), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the token parser.*/
    val whiteSpace_ : Parsley[_] => Parsley[Unit] =
    {
        val noLine = languageDef.commentLine.isEmpty
        val noMulti = languageDef.commentStart.isEmpty
        if (noLine && noMulti) space => skipMany(space ? "")
        else if (noLine)       space => skipMany((space <|> multiLineComment) ? "")
        else if (noMulti)      space => skipMany((space <|> oneLineComment) ? "")
        else                   space => skipMany((space <|> multiLineComment <|> oneLineComment) ? "")
    }
    
    /**Parses any comments and skips them, this includes both line comments and block comments.*/
    lazy val skipComment: Parsley[Unit] =
    {
        val noLine = languageDef.commentLine.isEmpty
        val noMulti = languageDef.commentStart.isEmpty
        if (noLine && noMulti) unit
        else if (noLine)       skipMany(multiLineComment ? "")
        else if (noMulti)      skipMany(oneLineComment ? "")
        else                   skipMany(multiLineComment <|> oneLineComment ? "")
    }
    
    // Bracketing
    /**Lexeme parser `parens(p)` parses `p` enclosed in parenthesis, returning the value of `p`.*/
    def parens[A](p: =>Parsley[A]): Parsley[A] = between(symbol("(") ? "open parenthesis", symbol(")") ? "closing parenthesis" <|> fail("unclosed parentheses"), p)
    
    /**Lexeme parser `braces(p)` parses `p` enclosed in braces ('{', '}'), returning the value of 'p'*/
    def braces[A](p: =>Parsley[A]): Parsley[A] = between(symbol("{") ? "open brace", symbol("}") ? "matching closing brace" <|> fail("unclosed braces"), p)
    
    /**Lexeme parser `angles(p)` parses `p` enclosed in angle brackets ('<', '>'), returning the
     * value of `p`.*/
    def angles[A](p: =>Parsley[A]): Parsley[A] = between(symbol("<") ? "open angle bracket", symbol(">") ? "matching closing angle bracket" <|> fail("unclosed angle brackets"), p)
    
    /**Lexeme parser `brackets(p)` parses `p` enclosed in brackets ('[', ']'), returning the value
     * of `p`.*/
    def brackets[A](p: =>Parsley[A]): Parsley[A] = between(symbol("[") ? "open square bracket", symbol("]") ? "matching closing square bracket" <|> fail("unclosed square brackets"), p)
    
    /**Lexeme parser `semi` parses the character ';' and skips any trailing white space. Returns ";"*/
    lazy val semi: Parsley[String] = symbol(";") ? "semicolon"
    
    /**Lexeme parser `comma` parses the character ',' and skips any trailing white space. Returns ","*/
    lazy val comma: Parsley[String] = symbol(",") ? "comma"
    
    /**Lexeme parser `colon` parses the character ':' and skips any trailing white space. Returns ":"*/
    lazy val colon: Parsley[String] = symbol(":") ? "colon"
    
    /**Lexeme parser `dot` parses the character '.' and skips any trailing white space. Returns "."*/
    lazy val dot: Parsley[String] = symbol(".") ? "dot"
    
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
}