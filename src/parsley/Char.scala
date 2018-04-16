package parsley

import parsley.Parsley._

import scala.annotation.switch
import scala.language.implicitConversions

object Char
{
    // Implicit Conversions
    @inline implicit def stringLift(str: String): Parsley[String] = string(str)
    @inline implicit def charLift(c: Char): Parsley[Char] = char(c)

    /** Reads a character from the input stream and returns it, else fails if the character is not found at the head
      * of the stream.
      * @param c The character to search for
      * @return `c` if it can be found at the head of the input
      */
    def char(c: Char): Parsley[Char] = new DeepEmbedding.CharTok(c)

    /** Reads a character from the head of the input stream if and only if it satisfies the given predicate. Else it
      * fails without consuming the character.
      * @param f The function to test the character on
      * @return `c` if `f(c)` is true.
      */
    def satisfy(f: Char => Boolean): Parsley[Char] = new DeepEmbedding.Satisfy(f)

    /** Reads a string from the input stream and returns it, else fails if the string is not found at the head
      * of the stream.
      * @param s The string to match against
      * @return `s` if it can be found at the head of the input
      */
    def string(s: String): Parsley[String] = new DeepEmbedding.StringTok(s)

    /**`oneOf(cs)` succeeds if the current character is in the supplied set of characters `cs`.
      * Returns the parsed character. See also `satisfy`.*/
    def oneOf(cs: Set[Char]): Parsley[Char] = satisfy(cs.contains)

    /**As the dual of `oneOf`, `noneOf(cs)` succeeds if the current character is not in the supplied
      * set of characters `cs`. Returns the parsed character.*/
    def noneOf(cs: Set[Char]): Parsley[Char] = satisfy(!cs.contains(_))

    /**The parser `anyChar` accepts any kind of character. Returns the accepted character.*/
    val anyChar: Parsley[Char] = satisfy(_ => true) ? "any character"

    /**Parses a whitespace character (either ' ' or '\t'). Returns the parsed character.*/
    val space: Parsley[Char] = satisfy(isSpace) ? "space/tab"

    /**Skips zero or more whitespace characters. See also `skipMany`. Uses space.*/
    val spaces: Parsley[Unit] = skipMany(space)

    /**Parses a whitespace character (' ', '\t', '\n', '\r', '\f', '\v'). Returns the parsed character.*/
    val whitespace: Parsley[Char] = satisfy(isWhitespace) ? "whitespace"

    /**Skips zero or more whitespace characters. See also `skipMany`. Uses whitespace.*/
    val whitespaces: Parsley[Unit] = skipMany(whitespace)

    /**Parses a newline character ('\n'). Returns a newline character.*/
    val newline: Parsley[Char] = '\n' ? "newline"

    /**Parses a carriage return character '\r' followed by a newline character '\n', returns the newline character.*/
    val crlf: Parsley[Char] = ('\r' *> '\n') ? "crlf newline"

    /**Parses a CRLF or LF end-of-line. Returns a newline character ('\n').*/
    val endOfLine: Parsley[Char] = ('\n' <|> ('\r' *> '\n')) ? "end of line"

    /**Parses a tab character ('\t'). Returns a tab character.*/
    val tab: Parsley[Char] = '\t' ? "tab"

    /**Parses an upper case letter. Returns the parsed character.*/
    val upper: Parsley[Char] = satisfy(_.isUpper) ? "uppercase letter"

    /**Parses a lower case letter. Returns the parsed character.*/
    val lower: Parsley[Char] = satisfy(_.isLower) ? "lowercase letter"

    /**Parses a letter or digit. Returns the parsed character.*/
    val alphaNum: Parsley[Char] = satisfy(_.isLetterOrDigit) ? "alpha-numeric character"

    /**Parses a letter. Returns the parsed character.*/
    val letter: Parsley[Char] = satisfy(_.isLetter) ? "letter"

    /**Parses a digit. Returns the parsed character.*/
    val digit: Parsley[Char] = satisfy(_.isDigit) ? "digit"

    /**Parses a hexadecimal digit. Returns the parsed character.*/
    val hexDigit: Parsley[Char] = satisfy(isHexDigit)

    /**Parses an octal digit. Returns the parsed character.*/
    val octDigit: Parsley[Char] = satisfy(isOctDigit) ? "octal digit"

    // Functions
    /** Helper function, equivalent to the predicate used by whitespace. Useful for providing to LanguageDef */
    def isWhitespace(c: Char): Boolean = (c: @switch) match
    {
        case ' ' | '\t' | '\n' | '\r' | '\f' | '\u000b' => true
        case _ => false
    }

    /** Helper function, equivalent to the predicate used by hexDigit. Useful for providing to LanguageDef */
    def isHexDigit(c: Char): Boolean = (c: @switch) match
    {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
             | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
             | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' => true
        case _ => false
    }

    /** Helper function, equivalent to the predicate used by octDigit. Useful for providing to LanguageDef */
    def isOctDigit(c: Char): Boolean = c <= '7' && c >= '0'

    /** Helper function, equivalent to the predicate used by space. Useful for providing to LanguageDef */
    def isSpace(c: Char): Boolean = c == ' ' || c == '\t'
}
