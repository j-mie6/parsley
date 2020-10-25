import parsley._
import parsley.Char.{alphaNum, letter, whitespace, oneOf => inSet}
import parsley.Implicits.charLift
import parsley.Combinator.eof

class TokeniserTests extends ParsleyTest
{
    val scala =
        LanguageDef(
            "/*",
            "*/",
            "//",
            true,
            Parser(letter <|> '_'),
            Parser(alphaNum <|> '_'),
            Parser(inSet(Set('+', '-', ':', '/', '*', '='))),
            Parser(inSet(Set('+', '-', ':', '/', '*', '='))),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set(":", "=", "::", ":="),
            true,
            Parser(whitespace))
    val scala_ =
        LanguageDef(
            "/*",
            "*/",
            "//",
            false,
            CharSet(('a' to 'z').toSet
                 ++ ('A' to 'Z').toSet + '_'),
            CharSet(('a' to 'z').toSet
                 ++ ('A' to 'Z').toSet
                 ++ ('0' to '9').toSet + '_'),
            CharSet(Set('+', '-', ':', '/', '*', '=')),
            CharSet(Set('+', '-', ':', '/', '*', '=')),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set(":", "=", "::", ":="),
            true,
            Predicate(Char.isWhitespace))
    val tokeniser = new TokenParser(scala)
    val tokeniser_ = new TokenParser(scala_)

    "identifier" should "read valid identifiers" in
    {
        runParser(tokeniser.identifier <* eof, "foo123 ") should be (Success("foo123"))
        runParser(tokeniser.identifier <* eof, "_bar") should be (Success("_bar"))
        runParser(tokeniser.identifier <* eof, "iffy") should be (Success("iffy"))
        runParser(tokeniser.identifier <* eof, "1_bar") shouldBe a [Failure]
    }
    it should "fail if the result is a keyword" in
    {
        runParser(tokeniser.identifier <* eof, "class") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.identifier <* eof, "foo123 ") should be (Success("foo123"))
        runParser(tokeniser_.identifier <* eof, "_bar") should be (Success("_bar"))
        runParser(tokeniser_.identifier <* eof, "iffy") should be (Success("iffy"))
        runParser(tokeniser_.identifier <* eof, "1_bar") should equal
        {
            runParser(tokeniser.identifier <* eof, "1_bar")
        }
        runParser(tokeniser_.identifier <* eof, "class") shouldBe a [Failure]
    }

    "keyword" should "match valid keywords" in
    {
        runParser(tokeniser.keyword("if"), "if then") should be (Success(()))
        runParser(tokeniser.keyword("volatile"), "volatile") should be (Success(()))
    }
    it should "fail if the input has more identifier letters" in
    {
        runParser(tokeniser.keyword("if"), "ifthen") shouldBe a [Failure]
        runParser(tokeniser.keyword("volatile"), "volatilev") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.keyword("if"), "if then") should be (Success(()))
        runParser(tokeniser_.keyword("volatile"), "volatile") should be (Success(()))
        runParser(tokeniser_.keyword("if"), "ifthen") should equal
        {
            runParser(tokeniser.keyword("if"), "ifthen")
        }
        runParser(tokeniser_.keyword("volatile"), "volatilev") should equal
        {
            runParser(tokeniser.keyword("volatile"), "volatilev")
        }
    }
    it must "not consume input on failure" in
    {
        runParser(tokeniser.keyword("if") <|> tokeniser.identifier, "id") should be (Success("id"))
    }

    "userOp" should "read valid operator" in
    {
        runParser(tokeniser.userOp <* eof, ":+:") should be (Success(":+:"))
        runParser(tokeniser.userOp <* eof, ":+:h") shouldBe a [Failure]
    }
    it should "fail if the result is reserved" in
    {
        runParser(tokeniser.userOp <* eof, ":") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.userOp <* eof, ":+:") should be (Success(":+:"))
        runParser(tokeniser_.userOp <* eof, ":+:h") shouldBe a [Failure]
        runParser(tokeniser_.userOp <* eof, ":") shouldBe a [Failure]
    }

    "reservedOp" should "match valid reserved operators" in
    {
        runParser(tokeniser.reservedOp <* eof, "=") should be (Success("="))
        runParser(tokeniser.reservedOp <* eof, ":") should be (Success(":"))
    }
    it should "fail if the result isn't reserved" in
    {
        runParser(tokeniser.reservedOp <* eof, "+") shouldBe a [Failure]
        runParser(tokeniser.reservedOp <* eof, "::=") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.reservedOp <* eof, "=") should be (Success("="))
        runParser(tokeniser_.reservedOp <* eof, ":") should be (Success(":"))
        runParser(tokeniser_.reservedOp <* eof, "+") shouldBe a [Failure]
        runParser(tokeniser_.reservedOp <* eof, "::=") shouldBe a [Failure]
    }

    "operator" should "match valid operators" in
    {
        runParser(tokeniser.operator("=") <* eof, "=") should be (Success(()))
        runParser(tokeniser.operator(":") <* eof, ":") should be (Success(()))
        runParser(tokeniser.operator("++") <* eof, "++") should be (Success(()))
    }
    it should "fail if the input has more operator letters" in
    {
        runParser(tokeniser.operator("=") <* eof, "=+") shouldBe a [Failure]
        runParser(tokeniser.operator(":") <* eof, "::") shouldBe a [Failure]
        runParser(tokeniser.operator("++") <* eof, "++=") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.operator("=") <* eof, "=") should equal
        {
            runParser(tokeniser.operator("=") <* eof, "=")
        }
        runParser(tokeniser_.operator(":") <* eof, ":") should equal
        {
            runParser(tokeniser.operator(":") <* eof, ":")
        }
        runParser(tokeniser_.operator("++") <* eof, "++") should equal
        {
            runParser(tokeniser.operator("++") <* eof, "++")
        }
        runParser(tokeniser_.operator("=") <* eof, "=+") should equal
        {
            runParser(tokeniser.operator("=") <* eof, "=+")
        }
        runParser(tokeniser_.operator(":") <* eof, "::") should equal
        {
            runParser(tokeniser.operator(":") <* eof, "::")
        }
        runParser(tokeniser_.operator("++") <* eof, "++=") should equal
        {
            runParser(tokeniser.operator("++") <* eof, "++=")
        }
    }

    "maxOp" should "match valid operators" in
    {
        runParser(tokeniser_.maxOp("=") <* eof, "=") should be (Success(()))
        runParser(tokeniser_.maxOp(":") <* eof, ":") should be (Success(()))
        runParser(tokeniser_.maxOp("++") <* eof, "++") should be (Success(()))
        runParser(tokeniser_.maxOp("+:") <* ':' <* eof, "+::") should be (Success(()))
        runParser(tokeniser_.maxOp("=") <* '=' <* eof, "==") should be (Success(()))
    }
    it must "fail if the operator is a valid prefix of another operator and that operator is parsable" in
    {
        runParser(tokeniser_.maxOp(":") <* '=' <* eof, ":=") shouldBe a [Failure]
        runParser(tokeniser_.maxOp(":") <* ':' <* eof, "::") shouldBe a [Failure]
    }

    "charLiteral" should "parse valid haskell characters" in
    {
        runParser(tokeniser.charLiteral, "'a'") should be (Success('a'))
        runParser(tokeniser.charLiteral, "'\\n'") should be (Success('\n'))
        runParser(tokeniser.charLiteral, "'\\xa'") should be (Success('\n'))
        runParser(tokeniser.charLiteral, "'\\^J'") should be (Success('\n'))
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.charLiteral, "'a'") should be (Success('a'))
        runParser(tokeniser_.charLiteral, "'\\n'") should be (Success('\n'))
        runParser(tokeniser_.charLiteral, "'\\xa'") should be (Success('\n'))
        runParser(tokeniser_.charLiteral, "'\\^J'") should be (Success('\n'))
    }

    "stringLiteral" should "parse valid haskell strings" in
    {
        runParser(tokeniser.stringLiteral, """"This string should have correct\t\xa whitespace properties!\8."""") should be
        {
            Success("This string should have correct\t\n whitespace properties!\b.")
        }
        runParser(tokeniser.stringLiteral, """"\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be
        {
            Success("I can write them like this!\n\n10")
        }
        runParser(tokeniser.stringLiteral, """"Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be
        {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        runParser(tokeniser.stringLiteral, "\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be
        {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser_.stringLiteral, """"This string should have correct\t\xa whitespace properties!\8."""") should be
        {
            Success("This string should have correct\t\n whitespace properties!\b.")
        }
        runParser(tokeniser_.stringLiteral, """"\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be
        {
            Success("I can write them like this!\n\n10")
        }
        runParser(tokeniser_.stringLiteral, """"Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be
        {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        runParser(tokeniser_.stringLiteral, "\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be
        {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
    }

    "rawStringLiteral" should "parse valid strings, without processing them" in
    {
        runParser(tokeniser.rawStringLiteral, """"this string is completely raw\n, nothing should be \xa changed!"""") should be
        {
            Success("""this string is completely raw\n, nothing should be \xa changed!""")
        }
        runParser(tokeniser.rawStringLiteral, """"Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this"""") should be
        {
            Success("""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this""")
        }
        runParser(tokeniser.rawStringLiteral, """"But we should be able to escape \", but it should remain like that!"""") should be
        {
            Success("""But we should be able to escape \", but it should remain like that!""")
        }
    }

    "natural" should "parse unsigned decimal numbers" in
    {
        runParser(tokeniser.natural, "0") should be (Success(0))
        runParser(tokeniser.natural, "1024") should be (Success(1024))
        runParser(tokeniser.natural, "1024  ") should be (Success(1024))
    }
    it should "parse unsigned hexadecimal numbers" in
    {
        runParser(tokeniser.natural, "0x340") should be (Success(0x340))
        runParser(tokeniser.natural, "0xFF") should be (Success(0xFF))
    }
    it should "parse unsigned octal numbers" in
    {
        runParser(tokeniser.natural, "0o201") should be (Success(129))
    }

    "integer" should "parse signed naturals" in
    {
        runParser(tokeniser.integer, "10") should be (Success(10))
        runParser(tokeniser.integer, "+10") should be (Success(10))
        runParser(tokeniser.integer, "-0xb") should be (Success(-0xb))
    }

    "unsignedFloat" should "parse unsigned fractional floats" in
    {
        runParser(tokeniser.unsignedFloat, "3.142") should be (Success(3.142))
        runParser(tokeniser.unsignedFloat, "0.23") should be (Success(0.23))
        runParser(tokeniser.unsignedFloat, "10.0") should be (Success(10.0))
    }
    it should "parse unsigned exponential floats" in
    {
        runParser(tokeniser.unsignedFloat, "3e10") should be (Success(3e10))
        runParser(tokeniser.unsignedFloat, "5E-4") should be (Success(5e-4))
    }
    it should "parse unsigned fractional exponential floats" in
    {
        runParser(tokeniser.unsignedFloat, "3.142e2") should be (Success(3.142e2))
        runParser(tokeniser.unsignedFloat, "0.23e1") should be (Success(0.23e1))
        runParser(tokeniser.unsignedFloat, "10.0e-5") should be (Success(10.0e-5))
    }
    it should "not parse integers" in
    {
        runParser(tokeniser.unsignedFloat, "3") shouldBe a [Failure]
    }
    it should "not allow .1 or 1." in
    {
        runParser(tokeniser.unsignedFloat, ".0") shouldBe a [Failure]
        runParser(tokeniser.unsignedFloat, "0.") shouldBe a [Failure]
    }

    "float" should "parse signed floats" in
    {
        runParser(tokeniser.float, "-3.142") should be (Success(-3.142))
        runParser(tokeniser.float, "-3e-4") should be (Success(-3e-4))
        runParser(tokeniser.float, "+1.2e2") should be (Success(1.2e2))
        runParser(tokeniser.float, "1.2") should be (Success(1.2))
    }

    "naturalOrFloat" should "parse either naturals or unsigned floats" in
    {
        runParser(tokeniser.naturalOrFloat, "3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        runParser(tokeniser.naturalOrFloat, "0.23") should be (Success(Right(0.23)))
        runParser(tokeniser.naturalOrFloat, "10.0\n") should be (Success(Right(10.0)))
        runParser(tokeniser.naturalOrFloat, "3e10") should be (Success(Right(3e10)))
        runParser(tokeniser.naturalOrFloat, "5E-4") should be (Success(Right(5e-4)))
        runParser(tokeniser.naturalOrFloat, "3.142e2\t ") should be (Success(Right(3.142e2)))
        runParser(tokeniser.naturalOrFloat, "0.23e1") should be (Success(Right(0.23e1)))
        runParser(tokeniser.naturalOrFloat, "10.0e-5") should be (Success(Right(10.0e-5)))
        runParser(tokeniser.naturalOrFloat, "1024") should be (Success(Left(1024)))
        runParser(tokeniser.naturalOrFloat, "0x340") should be (Success(Left(0x340)))
        runParser(tokeniser.naturalOrFloat, "0xFF") should be (Success(Left(0xFF)))
        runParser(tokeniser.naturalOrFloat, "0o201 //ooh, octal") should be (Success(Left(129)))
    }
    it should "not allow hexadecimal floats" in
    {
        runParser(tokeniser.naturalOrFloat <* eof, "0x340.0") shouldBe a [Failure]
    }
    it should "not allow octal floats" in
    {
        runParser(tokeniser.naturalOrFloat <* eof, "0o201.0") shouldBe a [Failure]
    }

    "number" should "parse integers or floats" in
    {
        runParser(tokeniser.number, "3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        runParser(tokeniser.number, "-0.23") should be (Success(Right(-0.23)))
        runParser(tokeniser.number, "10.0\n") should be (Success(Right(10.0)))
        runParser(tokeniser.number, "+3e10") should be (Success(Right(3e10)))
        runParser(tokeniser.number, "5E-4") should be (Success(Right(5e-4)))
        runParser(tokeniser.number, "3.142e2\t ") should be (Success(Right(3.142e2)))
        runParser(tokeniser.number, "+0.23e1") should be (Success(Right(0.23e1)))
        runParser(tokeniser.number, "10.0e-5") should be (Success(Right(10.0e-5)))
        runParser(tokeniser.number, "-1024") should be (Success(Left(-1024)))
        runParser(tokeniser.number, "0x340") should be (Success(Left(0x340)))
        runParser(tokeniser.number, "0xFF") should be (Success(Left(0xFF)))
        runParser(tokeniser.number, "0o201 //ooh, octal") should be (Success(Left(129)))
    }

    "skipComments" should "parse single-line comments" in
    {
        runParser(tokeniser.skipComments <* eof, "// hello world!") should be (Success(()))
        runParser(tokeniser.skipComments *> '\n' *> 'a', "// hello world!\na") should be (Success('a'))
    }
    it should "parse multi-line comments" in
    {
        runParser(tokeniser.skipComments <* eof, "/* hello *w/orld!*/") should be (Success(()))
        runParser(tokeniser.skipComments *> 'a', "/* hello *w/orld!*/a") should be (Success('a'))
        runParser(tokeniser.skipComments *> '\n' *> 'a', "/* hello world!*///another comment\na") should be (Success('a'))
    }
    it should "parse nested comments when applicable" in
    {
        runParser(tokeniser.skipComments <* eof, "/*/*hello world*/ this /*comment*/ is nested*/") should be (Success(()))
        runParser(tokeniser.skipComments <* eof, "/*/*hello world*/ this /*comment is nested*/") shouldBe a [Failure]
    }
    it should "not parse nested comments when applicable" in
    {
        runParser(tokeniser_.skipComments <* eof, "/*/*hello world*/ this /*comment*/ is nested*/") shouldBe a [Failure]
        runParser(tokeniser_.skipComments <* eof, "/*/*hello world this /*comment is nested*/") should be (Success(()))
    }

    "whiteSpace" should "parse all whitespace" in
    {
        runParser(tokeniser.whiteSpace <* eof, " \n\t \r\n ") should not be a [Failure]
    }
    it should "parse comments interleaved with spaces" in
    {
        runParser(tokeniser.whiteSpace <* eof, "/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should not be a [Failure]
    }
    it must "be the same regardless of the intrinsic" in
    {
        runParser(tokeniser.whiteSpace <* eof, " \n\t \r\n ") should equal
        {
            runParser(tokeniser_.whiteSpace <* eof, " \n\t \r\n ")
        }
        runParser(tokeniser.whiteSpace <* eof, "/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should equal
        {
            runParser(tokeniser_.whiteSpace <* eof, "/*this comment*/ /*is spaced out*/\n//by whitespace!\n ")
        }
    }
}
