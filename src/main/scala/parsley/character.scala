package parsley

import parsley.Parsley.empty
import parsley.combinator.skipMany
import parsley.implicits.character.charLift
import parsley.internal.deepembedding.singletons
import parsley.errors.combinator.ErrorMethods

import scala.annotation.switch
import scala.language.implicitConversions
import scala.collection.immutable.NumericRange

/** This module contains many parsers to do with reading one or more characters. Almost every parser will need something from this module.
  * @since 2.2.0
  *
  * @groupprio pred 100
  * @groupname pred Character Predicates
  * @groupdesc pred
  *     These are useful for providing to a [[token.LanguageDef]] to specify behaviour for the lexer.
  *     Other than that, they aren't ''particularly'' useful.
  *
  * @groupprio core 0
  * @groupname core Core Combinators and Parsers
  * @groupdesc core
  *     These are the most primitive combinators for consuming input capable of any input reading tasks.
  *
  * @groupprio skip 75
  * @groupname skip Whitespace Skipping Parsers
  * @groupdesc skip
  *     These parsers are designed to skip chunks of whitespace, for very rudimentary lexing tasks. It
  *     is probably better to use the functionality of [[parsley.token]].
  *
  * @groupprio class 20
  * @groupname class Character Class Combinators
  * @groupdesc class
  *     These combinators allow for working with ''character classes''. This means that a set, or range, of
  *     characters can be specified, and the combinator will return a parser that matches one of those characters
  *     (or conversely, any character that is ''not'' in that set). The parsed character is always returned.
  *
  * @groupprio spec 25
  * @groupname spec Specific Character Parsers
  * @groupdesc spec
  *     These parsers are special cases of `[[satisfy]]` or `[[char]]`. They are worth using, as they are given special error labelling,
  *     producing nicer error messages than their primitive counterparts.
  *
  * @define oneOf
  *     `oneOf(cs)` succeeds if the current character is in the supplied set of characters `cs`.
  *     Returns the parsed character. See also `[[satisfy]]`.
  * @define noneOf
  *     As the dual of `oneOf`, `noneOf(cs)` succeeds if the current character is not in the supplied
  *     set of characters `cs`. Returns the parsed character.
  */
object character
{
    /** Reads a character from the input stream and returns it, else fails if the character is not found at the head
      * of the stream.
      * @param c The character to search for
      * @return `c` if it can be found at the head of the input
      * @group core
      */
    def char(c: Char): Parsley[Char] = new Parsley(new singletons.CharTok(c, None))

    /** Reads a character from the head of the input stream if and only if it satisfies the given predicate. Else it
      * fails without consuming the character.
      * @param f The function to test the character on
      * @return `c` if `f(c)` is true.
      * @group core
      */
    def satisfy(f: Char => Boolean): Parsley[Char] = new Parsley(new singletons.Satisfy(f, None))

    /** Reads a string from the input stream and returns it, else fails if the string is not found at the head
      * of the stream.
      * @param s The string to match against
      * @return `s` if it can be found at the head of the input
      * @group core
      */
    def string(s: String): Parsley[String] = new Parsley(new singletons.StringTok(s, None))

    /** $oneOf
      *
      * @group class
      */
    def oneOf(cs: Set[Char]): Parsley[Char] = cs.size match {
        case 0 => empty
        case 1 => char(cs.head)
        case _ => satisfy(cs.contains).label {
            val Some(label) = parsley.errors.helpers.combineAsList(cs.map(renderChar).toList)
            s"one of $label"
        }
    }

    /** $oneOf
      *
      * @group class
      */
    def oneOf(cs: Char*): Parsley[Char] = oneOf(cs.toSet)

    /** $oneOf
      *
      * @group class
      */
    def oneOf(cs: NumericRange[Char]): Parsley[Char] = cs.size match {
        case 0 => empty
        case 1 => char(cs.head)
        case _ if Math.abs(cs(0).toInt - cs(1).toInt) == 1 => satisfy(cs.contains).label {
            s"one of ${renderChar(cs.min)} to ${renderChar(cs.max)}"
        }
        case _ => satisfy(cs.contains)
    }

    /** $noneOf
      *
      * @group class
      */
    def noneOf(cs: Set[Char]): Parsley[Char] = cs.size match {
        case 0 => item
        case 1 => satisfy(cs.head != _).label(s"anything except ${renderChar(cs.head)}")
        case _ => satisfy(!cs.contains(_)).label {
            val Some(label) = parsley.errors.helpers.combineAsList(cs.map(renderChar).toList)
            s"anything except $label"
        }
    }

    /** $noneOf
      *
      * @group class
      */
    def noneOf(cs: Char*): Parsley[Char] = noneOf(cs.toSet)

    /** $noneOf
      *
      * @group class
      */
    def noneOf(cs: NumericRange[Char]): Parsley[Char] = cs.size match {
        case 0 => item
        case 1 => satisfy(cs.head != _).label(s"anything except ${renderChar(cs.head)}")
        case _ if Math.abs(cs(0).toInt - cs(1).toInt) == 1 => satisfy(!cs.contains(_)).label {
            s"anything outside of ${renderChar(cs.min)} to ${renderChar(cs.max)}"
        }
        case _ => satisfy(!cs.contains(_))
    }

    /** The parser `item` accepts any kind of character. Returns the accepted character.
      *
      * @group core
      */
    val item: Parsley[Char] = satisfy(_ => true).label("any character")

    /** Parses a whitespace character (either ' ' or '\t'). Returns the parsed character.
      *
      * @group spec
      */
    val space: Parsley[Char] = satisfy(isSpace).label("space/tab")

    /** Skips zero or more space characters using `[[space]]` (see also `[[combinator.skipMany]]`).
      *
      * @group skip
      */
    val spaces: Parsley[Unit] = skipMany(space)

    /** Parses a whitespace character (' ', '\t', '\n', '\r', '\f', '\v'). Returns the parsed character.
      *
      * @group spec
      */
    val whitespace: Parsley[Char] = satisfy(isWhitespace).label("whitespace")

    /** Skips zero or more space characters using `[[whitespace]]` (see also `[[combinator.skipMany]]`).
      *
      * @group skip
      */
    val whitespaces: Parsley[Unit] = skipMany(whitespace)

    /** Parses a newline character ('\n'). Returns a newline character.
      *
      * @group spec
      */
    val newline: Parsley[Char] = '\n'.label("newline")

    /** Parses a carriage return character '\r' followed by a newline character '\n', returns the newline character.
      *
      * @group spec
      */
    val crlf: Parsley[Char] = '\r'.label("crlf newline") *> '\n'.label("end of crlf")

    /** Parses a CRLF or LF end-of-line. Returns a newline character ('\n').
      *
      * @group spec
      */
    val endOfLine: Parsley[Char] = '\n'.label("end of line") <|> ('\r'.label("end of line") *> '\n'.label("end of crlf"))

    /** Parses a tab character ('\t'). Returns a tab character.
      *
      * @group spec
      */
    val tab: Parsley[Char] = '\t'.label("tab")

    /** Parses an upper case letter. Returns the parsed character.
      *
      * @group spec
      */
    val upper: Parsley[Char] = satisfy(_.isUpper).label("uppercase letter")

    /** Parses a lower case letter. Returns the parsed character.
      *
      * @group spec
      */
    val lower: Parsley[Char] = satisfy(_.isLower).label("lowercase letter")

    /** Parses a letter or digit. Returns the parsed character.
      *
      * @group spec
      */
    val alphaNum: Parsley[Char] = satisfy(_.isLetterOrDigit).label("alpha-numeric character")

    /** Parses a letter. Returns the parsed character.
      *
      * @group spec
      */
    val letter: Parsley[Char] = satisfy(_.isLetter).label("letter")

    /** Parses a digit. Returns the parsed character.
      *
      * @group spec
      */
    val digit: Parsley[Char] = satisfy(_.isDigit).label("digit")

    /** Parses a hexadecimal digit. Returns the parsed character.
      *
      * @group spec
      */
    val hexDigit: Parsley[Char] = satisfy(isHexDigit)

    /** Parses an octal digit. Returns the parsed character.
      *
      * @group spec
      */
    val octDigit: Parsley[Char] = satisfy(isOctDigit).label("octal digit")

    // Functions
    /** Helper function, equivalent to the predicate used by `[[whitespace]]`.
      *
      * @group pred
      */
    def isWhitespace(c: Char): Boolean = (c: @switch) match {
        case ' ' | '\t' | '\n' | '\r' | '\f' | '\u000b' => true
        case _ => false
    }

    /** Helper function, equivalent to the predicate used by `[[hexDigit]]`.
      *
      * @group pred
      */
    def isHexDigit(c: Char): Boolean = (c: @switch) match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
           | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' => true
        case _ => false
    }

    /** Helper function, equivalent to the predicate used by `[[octDigit]]`.
      *
      * @group pred
      */
    def isOctDigit(c: Char): Boolean = c <= '7' && c >= '0'

    /** Helper function, equivalent to the predicate used by `[[space]]`.
      *
      * @group pred
      */
    def isSpace(c: Char): Boolean = c == ' ' || c == '\t'

    // Sue me.
    private def renderChar(c: Char): String = parsley.errors.helpers.renderRawString(s"$c")
}
