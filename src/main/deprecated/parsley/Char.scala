package parsley

@deprecated("This object will be removed in Parsley 3.0, use `parsley.character` instead", "v2.2.0")
object Char
{
    // $COVERAGE-OFF$
    /** Reads a character from the input stream and returns it, else fails if the character is not found at the head
      * of the stream.
      * @param c The character to search for
      * @return `c` if it can be found at the head of the input
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.char` instead", "v2.2.0")
    def char(c: Char): Parsley[Char] = parsley.character.char(c)

    /** Reads a character from the head of the input stream if and only if it satisfies the given predicate. Else it
      * fails without consuming the character.
      * @param f The function to test the character on
      * @return `c` if `f(c)` is true.
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.satisfy` instead", "v2.2.0")
    def satisfy(f: Char => Boolean): Parsley[Char] = parsley.character.satisfy(f)

    /** Reads a string from the input stream and returns it, else fails if the string is not found at the head
      * of the stream.
      * @param s The string to match against
      * @return `s` if it can be found at the head of the input
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.string` instead", "v2.2.0")
    def string(s: String): Parsley[String] = parsley.character.string(s)

    /**`oneOf(cs)` succeeds if the current character is in the supplied set of characters `cs`.
      * Returns the parsed character. See also `satisfy`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.oneOf` instead", "v2.2.0")
    def oneOf(cs: Set[Char]): Parsley[Char] = parsley.character.oneOf(cs)

    /**As the dual of `oneOf`, `noneOf(cs)` succeeds if the current character is not in the supplied
      * set of characters `cs`. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.noneOf` instead", "v2.2.0")
    def noneOf(cs: Set[Char]): Parsley[Char] = parsley.character.noneOf(cs)

    /**`oneOf(cs)` succeeds if the current character is in the supplied sequence of characters `cs`.
      * Returns the parsed character. See also `satisfy`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.oneOf` instead", "v2.2.0")
    def oneOf(cs: Char*): Parsley[Char] = parsley.character.oneOf(cs: _*)

    /**As the dual of `oneOf`, `noneOf(cs)` succeeds if the current character is not in the supplied
      * sequence of characters `cs`. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.noneOf` instead", "v2.2.0")
    def noneOf(cs: Char*): Parsley[Char] = parsley.character.noneOf(cs: _*)

    /**The parser `anyChar` accepts any kind of character. Returns the accepted character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.anyChar` instead", "v2.2.0")
    val anyChar: Parsley[Char] = parsley.character.anyChar

    /**Parses a whitespace character (either ' ' or '\t'). Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.space` instead", "v2.2.0")
    val space: Parsley[Char] = parsley.character.space

    /**Skips zero or more whitespace characters. See also `skipMany`. Uses space.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.spaces` instead", "v2.2.0")
    val spaces: Parsley[Unit] = parsley.character.spaces

    /**Parses a whitespace character (' ', '\t', '\n', '\r', '\f', '\v'). Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.whitespace` instead", "v2.2.0")
    val whitespace: Parsley[Char] = parsley.character.whitespace

    /**Skips zero or more whitespace characters. See also `skipMany`. Uses whitespace.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.whitespaces` instead", "v2.2.0")
    val whitespaces: Parsley[Unit] = parsley.character.whitespaces

    /**Parses a newline character ('\n'). Returns a newline character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.newline` instead", "v2.2.0")
    val newline: Parsley[Char] = parsley.character.newline

    /**Parses a carriage return character '\r' followed by a newline character '\n', returns the newline character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.crlf` instead", "v2.2.0")
    val crlf: Parsley[Char] = parsley.character.crlf

    /**Parses a CRLF or LF end-of-line. Returns a newline character ('\n').*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.endOfLine` instead", "v2.2.0")
    val endOfLine: Parsley[Char] = parsley.character.endOfLine

    /**Parses a tab character ('\t'). Returns a tab character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.tab` instead", "v2.2.0")
    val tab: Parsley[Char] = parsley.character.tab

    /**Parses an upper case letter. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.upper` instead", "v2.2.0")
    val upper: Parsley[Char] = parsley.character.upper

    /**Parses a lower case letter. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.lower` instead", "v2.2.0")
    val lower: Parsley[Char] = parsley.character.lower

    /**Parses a letter or digit. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.alphaNum` instead", "v2.2.0")
    val alphaNum: Parsley[Char] = parsley.character.alphaNum

    /**Parses a letter. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.letter` instead", "v2.2.0")
    val letter: Parsley[Char] = parsley.character.letter

    /**Parses a digit. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.digit` instead", "v2.2.0")
    val digit: Parsley[Char] = parsley.character.digit

    /**Parses a hexadecimal digit. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.hexDigit` instead", "v2.2.0")
    val hexDigit: Parsley[Char] = parsley.character.hexDigit

    /**Parses an octal digit. Returns the parsed character.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.octDigit` instead", "v2.2.0")
    val octDigit: Parsley[Char] = parsley.character.octDigit

    // Functions
    /** Helper function, equivalent to the predicate used by whitespace. Useful for providing to LanguageDef */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.isWhitespace` instead", "v2.2.0")
    def isWhitespace(c: Char): Boolean = parsley.character.isWhitespace(c)

    /** Helper function, equivalent to the predicate used by hexDigit. Useful for providing to LanguageDef */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.isHexDigit` instead", "v2.2.0")
    def isHexDigit(c: Char): Boolean = parsley.character.isHexDigit(c)

    /** Helper function, equivalent to the predicate used by octDigit. Useful for providing to LanguageDef */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.isOctDigit` instead", "v2.2.0")
    def isOctDigit(c: Char): Boolean = parsley.character.isOctDigit(c)

    /** Helper function, equivalent to the predicate used by space. Useful for providing to LanguageDef */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.character.isSpace` instead", "v2.2.0")
    def isSpace(c: Char): Boolean = parsley.character.isSpace(c)
    // $COVERAGE-ON$
}
