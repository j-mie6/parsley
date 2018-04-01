package parsley

import Parsley._
import Combinator.{between, notFollowedBy, sepBy, sepBy1, skipSome, some}
import Char.{charLift, digit, hexDigit, octDigit, oneOf, satisfy, stringLift}
import parsley.DeepToken.{SkipComments, WhiteSpace}

import scala.reflect.runtime.universe._

final case class LanguageDef(commentStart: String,
                             commentEnd: String,
                             commentLine: String,
                             nestedComments: Boolean,
                             identStart: Either[Set[Char], Parsley[Char]],
                             identLetter: Either[Set[Char], Parsley[Char]],
                             opStart: Either[Set[Char], Parsley[Char]],
                             opLetter: Either[Set[Char], Parsley[Char]],
                             keywords: Set[String],
                             operators: Set[String],
                             caseSensitive: Boolean,
                             space: Either[Set[Char], Parsley[_]])
                                        
final class TokenParser(lang: LanguageDef)
{
    // Identifiers & Reserved words
    /**This lexeme parser parses a legal identifier. Returns the identifier string. This parser will
     * fail on identifiers that are reserved words (i.e. keywords). Legal identifier characters and
     * keywords are defined in the `LanguageDef` provided to the token parser. An identifier is treated
     * as a single token using `attempt`.*/
    lazy val identifier: Parsley[String] = (lang.identStart, lang.identLetter) match
    {
        case (Left(start), Left(letter)) => lexeme(new DeepToken.Identifier(start, letter, theReservedNames))
        case _ => lexeme (attempt (ident >?> (! isReservedName (_), "unexpected keyword " + _) ) )
    }

    /**The lexeme parser `keyword(name)` parses the symbol `name`, but it also checks that the `name`
     * is not a prefix of a valid identifier. A `keyword` is treated as a single token using `attempt`.*/
    // TODO intrinsic
    def keyword(name: String): Parsley[Unit] = lexeme(attempt(caseString(name) *> notFollowedBy(identLetter) ? ("end of " + name)))

    private def caseString(name: String): Parsley[String] =
    {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) c.toLower <|> c.toUpper else c
        if (lang.caseSensitive) name
        else name.foldRight(pure(name))((c, p) => caseChar(c) *> p) ? name
    }
    private def isReservedName(name: String): Boolean = theReservedNames.contains(if (lang.caseSensitive) name else name.toLowerCase)
    private val theReservedNames =  if (lang.caseSensitive) lang.keywords else lang.keywords.map(_.toLowerCase)
    private lazy val identStart = toParser(lang.identStart)
    private lazy val identLetter = toParser(lang.identLetter)
    private lazy val ident = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, identStart, many(identLetter)) ? "identifier"

    // Operators & Reserved ops
    /**This lexeme parser parses a legal operator. Returns the name of the operator. This parser
     * will fail on any operators that are reserved operators. Legal operator characters and
     * reserved operators are defined in the `LanguageDef` provided to the token parser. A
     * `userOp` is treated as a single token using `attempt`.*/
    lazy val userOp: Parsley[String] = (lang.opStart, lang.opLetter) match
    {
        case (Left(start), Left(letter)) => lexeme(new DeepToken.UserOp(start, letter, lang.operators))
        case _ => lexeme(attempt(oper >?> (!isReservedOp(_), "unexpected reserved operator " + _)))
    }

    /**This non-lexeme parser parses a reserved operator. Returns the name of the operator.
     * Legal operator characters and reserved operators are defined in the `LanguageDef`
     * provided to the token parser. A `reservedOp_` is treated as a single token using `attempt`.*/
    lazy val reservedOp_ : Parsley[String] = (lang.opStart, lang.opLetter) match
    {
        case (Left(start), Left(letter)) => lexeme(new DeepToken.ReservedOp(start, letter, lang.operators))
        case _ => attempt(oper >?> (isReservedOp, "unexpected non-reserved operator " + _))
    }

    /**This lexeme parser parses a reserved operator. Returns the name of the operator. Legal
     * operator characters and reserved operators are defined in the `LanguageDef` provided
     * to the token parser. A `reservedOp` is treated as a single token using `attempt`.*/
    lazy val reservedOp: Parsley[String] = lexeme(reservedOp_)

    /**The lexeme parser `operator(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    def operator(name: String): Parsley[Unit] = lexeme(operator_(name))

    /**The non-lexeme parser `operator_(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    // TODO intrinsic
    def operator_(name: String): Parsley[Unit] = attempt(name *> notFollowedBy(opLetter) ? ("end of " + name))

    private def isReservedOp(op: String): Boolean = lang.operators.contains(op)
    private lazy val opStart = toParser(lang.opStart)
    private lazy val opLetter = toParser(lang.opLetter)
    private lazy val oper = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, opStart, many(opLetter)) ? "operator"

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
    lazy val stringLiteral_ : Parsley[String] = lang.space match
    {
        case Left(ws) => new DeepToken.StringLiteral(ws)
        case _ => between('"' ? "string", '"' ? "end of string", many(stringChar)) <#> (_.flatten.mkString)
    }

    /**This non-lexeme parser parses a string in a raw fashion. The escape characters in the string
     * remain untouched. While escaped quotes do not end the string, they remain as \" in the result
     * instead of becoming a quote character. Does not support string gaps. */
    lazy val rawStringLiteral: Parsley[String] = new DeepToken.RawStringLiteral

    private lazy val escapeCode = new DeepToken.Escape
    private lazy val charEscape = '\\' *> escapeCode
    private lazy val charLetter = satisfy(c => (c != '\'') && (c != '\\') && (c > '\u0016'))
    private lazy val characterChar = (charLetter <|> charEscape) ? "literal character"

    private val escapeEmpty = '&'
    private lazy val escapeGap = skipSome(space) *> '\\' ? "end of string gap"
    private lazy val stringLetter = satisfy(c => (c != '"') && (c != '\\') && (c > '\u0016'))
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
    lazy val natural: Parsley[Int] = lexeme(nat)

    /**This lexeme parser parses an integer (a whole number). This parser is like `natural` except
     * that it can be prefixed with a sign (i.e '-' or '+'). Returns the value of the number. The
     * number can be specified in `decimal`, `hexadecimal` or `octal`. The number is parsed
     * according to the grammar rules in the haskell report.*/
    lazy val integer: Parsley[Int] = lexeme(int) ? "integer"

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report.*/
    lazy val unsignedFloat: Parsley[Double] = lexeme(floating)

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report. Accepts an optional
     * '+' or '-' sign.*/
    lazy val float: Parsley[Double] = lexeme(signedFloating) ? "float"

    /**This lexeme parser parses either `integer` or `float`. Returns the value of the number. This
     * parser deals with any overlap in the grammar rules for naturals and floats. The number is
     * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val number: Parsley[Either[Int, Double]] = lexeme(number_) ? "number"

    /**This lexeme parser parses either `natural` or `unsigned float`. Returns the value of the number. This
      * parser deals with any overlap in the grammar rules for naturals and floats. The number is
      * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val naturalOrFloat: Parsley[Either[Int, Double]] = lexeme(natFloat) ? "unsigned number"

    private lazy val decimal_ = number(10, digit)
    private lazy val hexadecimal_ = oneOf(Set('x', 'X')) *> number(16, hexDigit)
    private lazy val octal_ = oneOf(Set('o', 'O')) *> number(8, octDigit)

    // Floats
    private def sign[A: TypeTag] = new DeepToken.Sign[A]
    private lazy val floating = new DeepToken.Float
    private lazy val signedFloating = sign[Double] <*> floating
    private lazy val natFloat = attempt(floating.map(Right(_))) <|> nat.map(Left(_))
    private lazy val number_ =
        ('+' *> natFloat
     <|> '-' *> natFloat.map{ case Left(n) => Left(-n); case Right(f) => Right(-f) }
     <|> natFloat)

    // Integers and Naturals
    private lazy val nat = new DeepToken.Natural
    private lazy val int = sign[Int] <*> nat

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
    /**Lexeme parser `symbol(c)` parses `char(c)` and skips trailing white space.*/
    def symbol(name: Char): Parsley[Char] = lexeme[Char](name)

    /**Like `symbol`, but treats it as a single token using `attempt`. Only useful for
     * strings, since characters are already single token.*/
    def symbol_(name: String): Parsley[String] = attempt(symbol(name))

    /**`lexeme(p)` first applies parser `p` and then the `whiteSpace` parser, returning the value of
     * `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
     * point without white space. The only point where the `whiteSpace` parser should be called
     * explicitly is the start of the main parser in order to skip any leading white space.*/
    def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* whiteSpace

    private lazy val space = lang.space match
    {
        case Left(cs) => oneOf(cs)
        case Right(p) => p
    }

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the `LanguageDef`), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the token parser.*/
    lazy val whiteSpace: Parsley[Unit] = lang.space match
    {
        case Left(ws) => new WhiteSpace(ws, lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments) *> unit
        case Right(p) => skipMany(p ? "" <|> skipComment)
    }

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the parameter), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the token parser.*/
    // TODO - making this an intrinsic will take extra work!
    val whiteSpace_ : Parsley[_] => Parsley[Unit] = space => skipMany((space ? "") <|> skipComment)

    /**Parses any comments and skips them, this includes both line comments and block comments.*/
    lazy val skipComment: Parsley[Unit] = new SkipComments(lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments) *> unit
    
    // Bracketing
    /**Lexeme parser `parens(p)` parses `p` enclosed in parenthesis, returning the value of `p`.*/
    def parens[A](p: =>Parsley[A]): Parsley[A] = between(symbol('(') ? "open parenthesis", symbol(')') ? "closing parenthesis" <|> fail("unclosed parentheses"), p)
    
    /**Lexeme parser `braces(p)` parses `p` enclosed in braces ('{', '}'), returning the value of 'p'*/
    def braces[A](p: =>Parsley[A]): Parsley[A] = between(symbol('{') ? "open brace", symbol('}') ? "matching closing brace" <|> fail("unclosed braces"), p)
    
    /**Lexeme parser `angles(p)` parses `p` enclosed in angle brackets ('<', '>'), returning the
     * value of `p`.*/
    def angles[A](p: =>Parsley[A]): Parsley[A] = between(symbol('<') ? "open angle bracket", symbol('>') ? "matching closing angle bracket" <|> fail("unclosed angle brackets"), p)
    
    /**Lexeme parser `brackets(p)` parses `p` enclosed in brackets ('[', ']'), returning the value
     * of `p`.*/
    def brackets[A](p: =>Parsley[A]): Parsley[A] = between(symbol('[') ? "open square bracket", symbol(']') ? "matching closing square bracket" <|> fail("unclosed square brackets"), p)
    
    /**Lexeme parser `semi` parses the character ';' and skips any trailing white space. Returns ";"*/
    lazy val semi: Parsley[Char] = symbol(';') ? "semicolon"
    
    /**Lexeme parser `comma` parses the character ',' and skips any trailing white space. Returns ","*/
    lazy val comma: Parsley[Char] = symbol(',') ? "comma"
    
    /**Lexeme parser `colon` parses the character ':' and skips any trailing white space. Returns ":"*/
    lazy val colon: Parsley[Char] = symbol(':') ? "colon"
    
    /**Lexeme parser `dot` parses the character '.' and skips any trailing white space. Returns "."*/
    lazy val dot: Parsley[Char] = symbol('.') ? "dot"
    
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

    private def toParser(e: Either[Set[Char], Parsley[Char]]) = e match
    {
        case Left(cs) => oneOf(cs)
        case Right(p) => p
    }
}

private [parsley] object DeepToken
{
    sealed private [parsley] abstract class DeepTokenBase[+A] extends Parsley[A]
    {
        final override private [parsley] def optimise = this
    }

    sealed private [parsley] abstract class Resultless extends DeepEmbedding.Resultless
    {
        final override private [parsley] def optimise = this
    }

    private [parsley] class WhiteSpace(ws: Set[Char], start: String, end: String, line: String, nested: Boolean) extends Resultless
    {
        override protected def preprocess(cont: Parsley[Nothing] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new WhiteSpace(ws, start, end, line, nested)
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenWhiteSpace(ws, start, end, line, nested)
            cont
        }
    }

    private [parsley] class SkipComments(start: String, end: String, line: String, nested: Boolean) extends Resultless
    {
        override protected def preprocess(cont: Parsley[Nothing] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new SkipComments(start, end, line, nested)
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenSkipComments(start, end, line, nested)
            cont
        }
    }

    private [parsley] class Sign[A: TypeTag] extends DeepTokenBase[A => A]
    {
        override protected def preprocess(cont: Parsley[A => A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new Sign[A]
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenSign[A](expected)
            cont
        }
    }

    private [parsley] class Natural extends DeepTokenBase[Int]
    {
        override protected def preprocess(cont: Parsley[Int] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new Natural
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenNatural(expected)
            cont
        }
    }

    private [parsley] class Float extends DeepTokenBase[Double]
    {
        override protected def preprocess(cont: Parsley[Double] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new Float
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenFloat(expected)
            cont
        }
    }

    private [parsley] class Escape extends DeepTokenBase[Char]
    {
        override protected def preprocess(cont: Parsley[Char] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new Escape
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenEscape(expected)
            cont
        }
    }

    private [parsley] class StringLiteral(ws: Set[Char]) extends DeepTokenBase[String]
    {
        override protected def preprocess(cont: Parsley[String] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new StringLiteral(ws)
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenString(ws, expected)
            cont
        }
    }

    private [parsley] class RawStringLiteral extends DeepTokenBase[String]
    {
        override protected def preprocess(cont: Parsley[String] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new RawStringLiteral
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenRawString(expected)
            cont
        }
    }

    private [parsley] class Identifier(start: Set[Char], letter: Set[Char], keywords: Set[String]) extends DeepTokenBase[String]
    {
        override protected def preprocess(cont: Parsley[String] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new Identifier(start, letter, keywords)
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenIdentifier(start, letter, keywords, expected)
            cont
        }
    }

    private [parsley] class UserOp(start: Set[Char], letter: Set[Char], operators: Set[String]) extends DeepTokenBase[String]
    {
        override protected def preprocess(cont: Parsley[String] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new UserOp(start, letter, operators)
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenUserOperator(start, letter, operators, expected)
            cont
        }
    }

    private [parsley] class ReservedOp(start: Set[Char], letter: Set[Char], operators: Set[String]) extends DeepTokenBase[String]
    {
        override protected def preprocess(cont: Parsley[String] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int) =
        {
            val w = new Identifier(start, letter, operators)
            w.expected = label
            cont(w)
        }
        override private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter) =
        {
            instrs += new instructions.TokenOperator(start, letter, operators, expected)
            cont
        }
    }
}

object TokenTest
{
    def main(args: Array[String]): Unit =
    {
        val ws = Left(Set(' ', '\n'))
        val istart = Left(('a' to 'z').toSet ++ ('A' to 'Z').toSet + '_')
        val iletter = Left(('0' to '9').toSet ++ ('a' to 'z').toSet ++ ('A' to 'Z').toSet + '_')
        val lang = LanguageDef("##", "##", "#", false, istart, iletter, Left(Set('+', '-', '*', '/', '=')), Left(Set('+', '-', '*', '/', '=')), Set("var"), Set("+", "-", "*", "/", "="), true, ws)
        val tokeniser = new TokenParser(lang)
        val parser = tokeniser.identifier <* Combinator.eof
        val input = "a_really_really_really_long_name_2"
        println(parser.pretty)
        println(runParser(parser, input))
        val start = System.currentTimeMillis
        for (_ <- 1 to 10000000)
        {
            // 23.4 seconds
            runParserFastUnsafe(parser, input)
        }
        println(System.currentTimeMillis - start)
    }
}