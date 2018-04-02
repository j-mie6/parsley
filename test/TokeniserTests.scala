import parsley.{LanguageDef, TokenParser}
import parsley.Char.{alphaNum, charLift, letter, whitespace, oneOf => inSet}

class TokeniserTests extends ParsleyTest
{
    val scala =
        LanguageDef(
            "/*",
            "*/",
            "//",
            true,
            Right(letter <|> '_'),
            Right(alphaNum <|> '_'),
            Right(inSet(Set('+', '-', ':', '/', '*', '='))),
            Right(inSet(Set('+', '-', ':', '/', '*', '='))),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set.empty[String],
            true,
            Right(whitespace))
    val scala_ =
        LanguageDef(
            "/*",
            "*/",
            "//",
            false,
            Left(('a' to 'z').toSet
              ++ ('A' to 'Z').toSet + '_'),
            Left(('a' to 'z').toSet
              ++ ('A' to 'Z').toSet
              ++ ('0' to '9').toSet + '_'),
            Left(Set('+', '-', ':', '/', '*', '=')),
            Left(Set('+', '-', ':', '/', '*', '=')),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set.empty[String],
            true,
            Left(Set(' ', '\t', '\n', '\r', '\f', '\u000b')))
    val tokeniser = new TokenParser(scala)
    val tokeniser_ = new TokenParser(scala_)

    "identifier" should "read valid identifiers" in pending
    it should "fail if the result is a keyword" in pending
    it must "be the same regardless of the intrinsic" in pending

    "keyword" should "match valid keywords" in pending
    it should "fail if the input has more identifier letters" in pending
    it must "be the same regardless of the intrinsic" in pending

    "userOp" should "read valid operator" in pending
    it should "fail if the result is reserved" in pending
    it must "be the same regardless of the intrinsic" in pending

    "reservedOp" should "match valid reserved operators" in pending
    it should "fail if the result isn't reserved" in pending
    it must "be the same regardless of the intrinsic" in pending

    "operator" should "match valid operators" in pending
    it should "fail if the input has more operator letters" in pending
    it must "be the same regardless of the intrinsic" in pending

    "charLiteral" should "parse valid haskell characters" in pending
    it must "be the same regardless of the intrinsic" in pending

    "stringLiteral" should "parse valid haskell strings" in pending
    it must "be the same regardless of the intrinsic" in pending

    "rawStringLiteral" should "parse valid strings, without processing them" in pending

    "natural" should "parse unsigned decimal numbers" in pending
    it should "parse unsigned hexadecimal numbers" in pending
    it should "parse unsigned octal numbers" in pending

    "integer" should "parse signed naturals" in pending

    "unsignedFloat" should "parse unsigned fractional floats" in pending
    it should "parse unsigned exponential floats" in pending
    it should "parse unsigned fractional exponential floats" in pending
    it should "not parse integers" in pending
    it should "not allow .1 or 1." in pending

    "float" should "parse signed floats" in pending

    "naturalOrFloat" should "parse either naturals or unsigned floats" in pending
    it should "not allow hexadecimal floats" in pending
    it should "not allow octal floats" in pending

    "number" should "parse integers or floats" in pending

    "skipComment" should "parse single-line comments" in pending
    it should "parse multi-line comments" in pending
    it should "parse nested comments when applicable" in pending
    it should "not parse nested comments when applicable" in pending

    "whiteSpace" should "parse all whitespace" in pending
    it should "parse comments interleaved with spaces" in pending
    it must "be the same regardless of the intrinsic" in pending
}
