package parsley

import parsley._
import parsley.Char.{alphaNum, letter, whitespace, oneOf => inSet}
import parsley.Implicits.charLift
import parsley.Combinator.eof

import scala.language.implicitConversions

class TokeniserTests extends ParsleyTest {
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

    "identifier" should "read valid identifiers" in {
        (tokeniser.identifier <* eof).runParser("foo123 ") should be (Success("foo123"))
        (tokeniser.identifier <* eof).runParser("_bar") should be (Success("_bar"))
        (tokeniser.identifier <* eof).runParser("iffy") should be (Success("iffy"))
        (tokeniser.identifier <* eof).runParser("1_bar") shouldBe a [Failure]
    }
    it should "fail if the result is a keyword" in {
        (tokeniser.identifier <* eof).runParser("class") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.identifier <* eof).runParser("foo123 ") should be (Success("foo123"))
        (tokeniser_.identifier <* eof).runParser("_bar") should be (Success("_bar"))
        (tokeniser_.identifier <* eof).runParser("iffy") should be (Success("iffy"))
        (tokeniser_.identifier <* eof).runParser("1_bar") should equal {
            (tokeniser.identifier <* eof).runParser("1_bar")
        }
        (tokeniser_.identifier <* eof).runParser("class") shouldBe a [Failure]
    }

    "keyword" should "match valid keywords" in {
        tokeniser.keyword("if").runParser("if then") should be (Success(()))
        tokeniser.keyword("volatile").runParser("volatile") should be (Success(()))
    }
    it should "fail if the input has more identifier letters" in {
        tokeniser.keyword("if").runParser("ifthen") shouldBe a [Failure]
        tokeniser.keyword("volatile").runParser("volatilev") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.keyword("if").runParser("if then") should be (Success(()))
        tokeniser_.keyword("volatile").runParser("volatile") should be (Success(()))
        tokeniser_.keyword("if").runParser("ifthen") should equal {
            tokeniser.keyword("if").runParser("ifthen")
        }
        tokeniser_.keyword("volatile").runParser("volatilev") should equal {
            tokeniser.keyword("volatile").runParser("volatilev")
        }
    }
    it must "not consume input on failure" in {
        (tokeniser.keyword("if") <|> tokeniser.identifier).runParser("id") should be (Success("id"))
    }

    "userOp" should "read valid operator" in {
        (tokeniser.userOp <* eof).runParser(":+:") should be (Success(":+:"))
        (tokeniser.userOp <* eof).runParser(":+:h") shouldBe a [Failure]
    }
    it should "fail if the result is reserved" in {
        (tokeniser.userOp <* eof).runParser(":") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.userOp <* eof).runParser(":+:") should be (Success(":+:"))
        (tokeniser_.userOp <* eof).runParser(":+:h") shouldBe a [Failure]
        (tokeniser_.userOp <* eof).runParser(":") shouldBe a [Failure]
    }

    "reservedOp" should "match valid reserved operators" in {
        (tokeniser.reservedOp <* eof).runParser("=") should be (Success("="))
        (tokeniser.reservedOp <* eof).runParser(":") should be (Success(":"))
    }
    it should "fail if the result isn't reserved" in {
        (tokeniser.reservedOp <* eof).runParser("+") shouldBe a [Failure]
        (tokeniser.reservedOp <* eof).runParser("::=") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.reservedOp <* eof).runParser("=") should be (Success("="))
        (tokeniser_.reservedOp <* eof).runParser(":") should be (Success(":"))
        (tokeniser_.reservedOp <* eof).runParser("+") shouldBe a [Failure]
        (tokeniser_.reservedOp <* eof).runParser("::=") shouldBe a [Failure]
    }

    "operator" should "match valid operators" in {
        (tokeniser.operator("=") <* eof).runParser("=") should be (Success(()))
        (tokeniser.operator(":") <* eof).runParser(":") should be (Success(()))
        (tokeniser.operator("++") <* eof).runParser("++") should be (Success(()))
    }
    it should "fail if the input has more operator letters" in {
        (tokeniser.operator("=") <* eof).runParser("=+") shouldBe a [Failure]
        (tokeniser.operator(":") <* eof).runParser("::") shouldBe a [Failure]
        (tokeniser.operator("++") <* eof).runParser("++=") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.operator("=") <* eof).runParser("=") should equal {
            (tokeniser.operator("=") <* eof).runParser("=")
        }
        (tokeniser_.operator(":") <* eof).runParser(":") should equal {
            (tokeniser.operator(":") <* eof).runParser(":")
        }
        (tokeniser_.operator("++") <* eof).runParser("++") should equal {
            (tokeniser.operator("++") <* eof).runParser("++")
        }
        (tokeniser_.operator("=") <* eof).runParser("=+") should equal {
            (tokeniser.operator("=") <* eof).runParser("=+")
        }
        (tokeniser_.operator(":") <* eof).runParser("::") should equal {
            (tokeniser.operator(":") <* eof).runParser("::")
        }
        (tokeniser_.operator("++") <* eof).runParser("++=") should equal {
            (tokeniser.operator("++") <* eof).runParser("++=")
        }
        (tokeniser_.operator("+") <|> tokeniser_.operator("++") <* eof).runParser("++") should equal {
            (tokeniser.operator("+") <|> tokeniser.operator("++") <* eof).runParser("++")
        }
    }

    "maxOp" should "match valid operators" in {
        (tokeniser_.maxOp("=") <* eof).runParser("=") should be (Success(()))
        (tokeniser_.maxOp(":") <* eof).runParser(":") should be (Success(()))
        (tokeniser_.maxOp("++") <* eof).runParser("++") should be (Success(()))
        (tokeniser_.maxOp("+:") <* ':' <* eof).runParser("+::") should be (Success(()))
        (tokeniser_.maxOp("=") <* '=' <* eof).runParser("==") should be (Success(()))
    }
    it must "fail if the operator is a valid prefix of another operator and that operator is parsable" in {
        (tokeniser_.maxOp(":") <* '=' <* eof).runParser(":=") shouldBe a [Failure]
        (tokeniser_.maxOp(":") <* ':' <* eof).runParser("::") shouldBe a [Failure]
    }

    "charLiteral" should "parse valid haskell characters" in {
        tokeniser.charLiteral.runParser("'a'") should be (Success('a'))
        tokeniser.charLiteral.runParser("'\\n'") should be (Success('\n'))
        tokeniser.charLiteral.runParser("'\\xa'") should be (Success('\n'))
        tokeniser.charLiteral.runParser("'\\^J'") should be (Success('\n'))
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.charLiteral.runParser("'a'") should be (Success('a'))
        tokeniser_.charLiteral.runParser("'\\n'") should be (Success('\n'))
        tokeniser_.charLiteral.runParser("'\\xa'") should be (Success('\n'))
        tokeniser_.charLiteral.runParser("'\\^J'") should be (Success('\n'))
    }

    "stringLiteral" should "parse valid haskell strings" in {
        tokeniser.stringLiteral.runParser(""""This string should have correct\t\xa whitespace properties!\8."""") should be {
            Success("This string should have correct\t\n whitespace properties!\b.")
        }
        tokeniser.stringLiteral.runParser(""""\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be {
            Success("I can write them like this!\n\n10")
        }
        tokeniser.stringLiteral.runParser(""""Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        tokeniser.stringLiteral.runParser("\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.stringLiteral.runParser(""""This string should have correct\t\xa whitespace properties!\8."""") should be {
            Success("This string should have correct\t\n whitespace properties!\b.")
        }
        tokeniser_.stringLiteral.runParser(""""\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be {
            Success("I can write them like this!\n\n10")
        }
        tokeniser_.stringLiteral.runParser(""""Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        tokeniser_.stringLiteral.runParser("\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
    }

    "rawStringLiteral" should "parse valid strings, without processing them" in {
        tokeniser.rawStringLiteral.runParser(""""this string is completely raw\n, nothing should be \xa changed!"""") should be {
            Success("""this string is completely raw\n, nothing should be \xa changed!""")
        }
        tokeniser.rawStringLiteral.runParser(""""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this"""") should be {
            Success("""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this""")
        }
        tokeniser.rawStringLiteral.runParser(""""But we should be able to escape \", but it should remain like that!"""") should be {
            Success("""But we should be able to escape \", but it should remain like that!""")
        }
    }

    "natural" should "parse unsigned decimal numbers" in {
        tokeniser.natural.runParser("0") should be (Success(0))
        tokeniser.natural.runParser("1024") should be (Success(1024))
        tokeniser.natural.runParser("1024  ") should be (Success(1024))
    }
    it should "parse unsigned hexadecimal numbers" in {
        tokeniser.natural.runParser("0x340") should be (Success(0x340))
        tokeniser.natural.runParser("0xFF") should be (Success(0xFF))
    }
    it should "parse unsigned octal numbers" in {
        tokeniser.natural.runParser("0o201") should be (Success(129))
    }

    "integer" should "parse signed naturals" in {
        tokeniser.integer.runParser("10") should be (Success(10))
        tokeniser.integer.runParser("+10") should be (Success(10))
        tokeniser.integer.runParser("-0xb") should be (Success(-0xb))
    }

    "unsignedFloat" should "parse unsigned fractional floats" in {
        tokeniser.unsignedFloat.runParser("3.142") should be (Success(3.142))
        tokeniser.unsignedFloat.runParser("0.23") should be (Success(0.23))
        tokeniser.unsignedFloat.runParser("10.0") should be (Success(10.0))
    }
    it should "parse unsigned exponential floats" in {
        tokeniser.unsignedFloat.runParser("3e10") should be (Success(3e10))
        tokeniser.unsignedFloat.runParser("5E-4") should be (Success(5e-4))
    }
    it should "parse unsigned fractional exponential floats" in {
        tokeniser.unsignedFloat.runParser("3.142e2") should be (Success(3.142e2))
        tokeniser.unsignedFloat.runParser("0.23e1") should be (Success(0.23e1))
        tokeniser.unsignedFloat.runParser("10.0e-5") should be (Success(10.0e-5))
    }
    it should "not parse integers" in {
        tokeniser.unsignedFloat.runParser("3") shouldBe a [Failure]
    }
    it should "not allow .1 or 1." in {
        tokeniser.unsignedFloat.runParser(".0") shouldBe a [Failure]
        tokeniser.unsignedFloat.runParser("0.") shouldBe a [Failure]
    }

    "float" should "parse signed floats" in {
        tokeniser.float.runParser("-3.142") should be (Success(-3.142))
        tokeniser.float.runParser("-3e-4") should be (Success(-3e-4))
        tokeniser.float.runParser("+1.2e2") should be (Success(1.2e2))
        tokeniser.float.runParser("1.2") should be (Success(1.2))
    }

    "naturalOrFloat" should "parse either naturals or unsigned floats" in {
        tokeniser.naturalOrFloat.runParser("3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        tokeniser.naturalOrFloat.runParser("0.23") should be (Success(Right(0.23)))
        tokeniser.naturalOrFloat.runParser("10.0\n") should be (Success(Right(10.0)))
        tokeniser.naturalOrFloat.runParser("3e10") should be (Success(Right(3e10)))
        tokeniser.naturalOrFloat.runParser("5E-4") should be (Success(Right(5e-4)))
        tokeniser.naturalOrFloat.runParser("3.142e2\t ") should be (Success(Right(3.142e2)))
        tokeniser.naturalOrFloat.runParser("0.23e1") should be (Success(Right(0.23e1)))
        tokeniser.naturalOrFloat.runParser("10.0e-5") should be (Success(Right(10.0e-5)))
        tokeniser.naturalOrFloat.runParser("1024") should be (Success(Left(1024)))
        tokeniser.naturalOrFloat.runParser("0x340") should be (Success(Left(0x340)))
        tokeniser.naturalOrFloat.runParser("0xFF") should be (Success(Left(0xFF)))
        tokeniser.naturalOrFloat.runParser("0o201 //ooh, octal") should be (Success(Left(129)))
    }
    it should "not allow hexadecimal floats" in {
        (tokeniser.naturalOrFloat <* eof).runParser("0x340.0") shouldBe a [Failure]
    }
    it should "not allow octal floats" in {
        (tokeniser.naturalOrFloat <* eof).runParser("0o201.0") shouldBe a [Failure]
    }

    "number" should "parse integers or floats" in {
        tokeniser.number.runParser("3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        tokeniser.number.runParser("-0.23") should be (Success(Right(-0.23)))
        tokeniser.number.runParser("10.0\n") should be (Success(Right(10.0)))
        tokeniser.number.runParser("+3e10") should be (Success(Right(3e10)))
        tokeniser.number.runParser("5E-4") should be (Success(Right(5e-4)))
        tokeniser.number.runParser("3.142e2\t ") should be (Success(Right(3.142e2)))
        tokeniser.number.runParser("+0.23e1") should be (Success(Right(0.23e1)))
        tokeniser.number.runParser("10.0e-5") should be (Success(Right(10.0e-5)))
        tokeniser.number.runParser("-1024") should be (Success(Left(-1024)))
        tokeniser.number.runParser("0x340") should be (Success(Left(0x340)))
        tokeniser.number.runParser("0xFF") should be (Success(Left(0xFF)))
        tokeniser.number.runParser("0o201 //ooh, octal") should be (Success(Left(129)))
    }

    "skipComments" should "parse single-line comments" in {
        (tokeniser.skipComments <* eof).runParser("// hello world!") should be (Success(()))
        (tokeniser.skipComments *> '\n' *> 'a').runParser("// hello world!\na") should be (Success('a'))
    }
    it should "parse multi-line comments" in {
        (tokeniser.skipComments <* eof).runParser("/* hello *w/orld!*/") should be (Success(()))
        (tokeniser.skipComments *> 'a').runParser("/* hello *w/orld!*/a") should be (Success('a'))
        (tokeniser.skipComments *> '\n' *> 'a').runParser("/* hello world!*///another comment\na") should be (Success('a'))
    }
    it should "parse nested comments when applicable" in {
        (tokeniser.skipComments <* eof).runParser("/*/*hello world*/ this /*comment*/ is nested*/") should be (Success(()))
        (tokeniser.skipComments <* eof).runParser("/*/*hello world*/ this /*comment is nested*/") shouldBe a [Failure]
    }
    it should "not parse nested comments when applicable" in {
        (tokeniser_.skipComments <* eof).runParser("/*/*hello world*/ this /*comment*/ is nested*/") shouldBe a [Failure]
        (tokeniser_.skipComments <* eof).runParser("/*/*hello world this /*comment is nested*/") should be (Success(()))
    }

    "whiteSpace" should "parse all whitespace" in {
        (tokeniser.whiteSpace <* eof).runParser(" \n\t \r\n ") should not be a [Failure]
    }
    it should "parse comments interleaved with spaces" in {
        (tokeniser.whiteSpace <* eof).runParser("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should not be a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser.whiteSpace <* eof).runParser(" \n\t \r\n ") should equal {
            (tokeniser_.whiteSpace <* eof).runParser(" \n\t \r\n ")
        }
        (tokeniser.whiteSpace <* eof).runParser("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should equal {
            (tokeniser_.whiteSpace <* eof).runParser("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ")
        }
    }
}
