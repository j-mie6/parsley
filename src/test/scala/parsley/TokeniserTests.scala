package parsley

import parsley._
import parsley.character.{alphaNum, letter, whitespace, oneOf => inSet}
import parsley.implicits.charLift
import parsley.combinator.eof

import scala.language.implicitConversions

class TokeniserTests extends ParsleyTest {
    val scala =
        token.LanguageDef(
            "/*",
            "*/",
            "//",
            true,
            token.Parser(letter <|> '_'),
            token.Parser(alphaNum <|> '_'),
            token.Parser(inSet('+', '-', ':', '/', '*', '=')),
            token.Parser(inSet('+', '-', ':', '/', '*', '=')),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set(":", "=", "::", ":="),
            true,
            token.Parser(whitespace))
    val scala_ =
        token.LanguageDef(
            "/*",
            "*/",
            "//",
            false,
            token.CharSet(('a' to 'z').toSet
                       ++ ('A' to 'Z').toSet + '_'),
                 token.CharSet(('a' to 'z').toSet
                            ++ ('A' to 'Z').toSet
                            ++ ('0' to '9').toSet + '_'),
                 token.CharSet(Set('+', '-', ':', '/', '*', '=')),
                 token.CharSet(Set('+', '-', ':', '/', '*', '=')),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set(":", "=", "::", ":="),
            true,
            token.Predicate(character.isWhitespace))
    val tokeniser = new token.Lexer(scala)
    val tokeniser_ = new token.Lexer(scala_)

    "identifier" should "read valid identifiers" in {
        (tokeniser.identifier <* eof).parse("foo123 ") should be (Success("foo123"))
        (tokeniser.identifier <* eof).parse("_bar") should be (Success("_bar"))
        (tokeniser.identifier <* eof).parse("iffy") should be (Success("iffy"))
        (tokeniser.identifier <* eof).parse("1_bar") shouldBe a [Failure]
    }
    it should "fail if the result is a keyword" in {
        (tokeniser.identifier <* eof).parse("class") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.identifier <* eof).parse("foo123 ") should be (Success("foo123"))
        (tokeniser_.identifier <* eof).parse("_bar") should be (Success("_bar"))
        (tokeniser_.identifier <* eof).parse("iffy") should be (Success("iffy"))
        (tokeniser_.identifier <* eof).parse("1_bar") should equal {
            (tokeniser.identifier <* eof).parse("1_bar")
        }
        (tokeniser_.identifier <* eof).parse("class") shouldBe a [Failure]
    }

    "keyword" should "match valid keywords" in {
        tokeniser.keyword("if").parse("if then") should be (Success(()))
        tokeniser.keyword("volatile").parse("volatile") should be (Success(()))
    }
    it should "fail if the input has more identifier letters" in {
        tokeniser.keyword("if").parse("ifthen") shouldBe a [Failure]
        tokeniser.keyword("volatile").parse("volatilev") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.keyword("if").parse("if then") should be (Success(()))
        tokeniser_.keyword("volatile").parse("volatile") should be (Success(()))
        tokeniser_.keyword("if").parse("ifthen") should equal {
            tokeniser.keyword("if").parse("ifthen")
        }
        tokeniser_.keyword("volatile").parse("volatilev") should equal {
            tokeniser.keyword("volatile").parse("volatilev")
        }
    }
    it must "not consume input on failure" in {
        (tokeniser.keyword("if") <|> tokeniser.identifier).parse("id") should be (Success("id"))
    }

    "userOp" should "read valid operator" in {
        (tokeniser.userOp <* eof).parse(":+:") should be (Success(":+:"))
        (tokeniser.userOp <* eof).parse(":+:h") shouldBe a [Failure]
    }
    it should "fail if the result is reserved" in {
        (tokeniser.userOp <* eof).parse(":") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.userOp <* eof).parse(":+:") should be (Success(":+:"))
        (tokeniser_.userOp <* eof).parse(":+:h") shouldBe a [Failure]
        (tokeniser_.userOp <* eof).parse(":") shouldBe a [Failure]
    }

    "reservedOp" should "match valid reserved operators" in {
        (tokeniser.reservedOp <* eof).parse("=") should be (Success("="))
        (tokeniser.reservedOp <* eof).parse(":") should be (Success(":"))
    }
    it should "fail if the result isn't reserved" in {
        (tokeniser.reservedOp <* eof).parse("+") shouldBe a [Failure]
        (tokeniser.reservedOp <* eof).parse("::=") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.reservedOp <* eof).parse("=") should be (Success("="))
        (tokeniser_.reservedOp <* eof).parse(":") should be (Success(":"))
        (tokeniser_.reservedOp <* eof).parse("+") shouldBe a [Failure]
        (tokeniser_.reservedOp <* eof).parse("::=") shouldBe a [Failure]
    }

    "operator" should "match valid operators" in {
        (tokeniser.operator("=") <* eof).parse("=") should be (Success(()))
        (tokeniser.operator(":") <* eof).parse(":") should be (Success(()))
        (tokeniser.operator("++") <* eof).parse("++") should be (Success(()))
    }
    it should "fail if the input has more operator letters" in {
        (tokeniser.operator("=") <* eof).parse("=+") shouldBe a [Failure]
        (tokeniser.operator(":") <* eof).parse("::") shouldBe a [Failure]
        (tokeniser.operator("++") <* eof).parse("++=") shouldBe a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.operator("=") <* eof).parse("=") should equal {
            (tokeniser.operator("=") <* eof).parse("=")
        }
        (tokeniser_.operator(":") <* eof).parse(":") should equal {
            (tokeniser.operator(":") <* eof).parse(":")
        }
        (tokeniser_.operator("++") <* eof).parse("++") should equal {
            (tokeniser.operator("++") <* eof).parse("++")
        }
        (tokeniser_.operator("=") <* eof).parse("=+") should equal {
            (tokeniser.operator("=") <* eof).parse("=+")
        }
        (tokeniser_.operator(":") <* eof).parse("::") should equal {
            (tokeniser.operator(":") <* eof).parse("::")
        }
        (tokeniser_.operator("++") <* eof).parse("++=") should equal {
            (tokeniser.operator("++") <* eof).parse("++=")
        }
        (tokeniser_.operator("+") <|> tokeniser_.operator("++") <* eof).parse("++") should equal {
            (tokeniser.operator("+") <|> tokeniser.operator("++") <* eof).parse("++")
        }
    }

    "maxOp" should "match valid operators" in {
        (tokeniser_.maxOp("=") <* eof).parse("=") should be (Success(()))
        (tokeniser_.maxOp(":") <* eof).parse(":") should be (Success(()))
        (tokeniser_.maxOp("++") <* eof).parse("++") should be (Success(()))
        (tokeniser_.maxOp("+:") <* ':' <* eof).parse("+::") should be (Success(()))
        (tokeniser_.maxOp("=") <* '=' <* eof).parse("==") should be (Success(()))
    }
    it must "fail if the operator is a valid prefix of another operator and that operator is parsable" in {
        (tokeniser_.maxOp(":") <* '=' <* eof).parse(":=") shouldBe a [Failure]
        (tokeniser_.maxOp(":") <* ':' <* eof).parse("::") shouldBe a [Failure]
    }

    "charLiteral" should "parse valid haskell characters" in {
        tokeniser.charLiteral.parse("'a'") should be (Success('a'))
        tokeniser.charLiteral.parse("'\\n'") should be (Success('\n'))
        tokeniser.charLiteral.parse("'\\xa'") should be (Success('\n'))
        tokeniser.charLiteral.parse("'\\^J'") should be (Success('\n'))
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.charLiteral.parse("'a'") should be (Success('a'))
        tokeniser_.charLiteral.parse("'\\n'") should be (Success('\n'))
        tokeniser_.charLiteral.parse("'\\xa'") should be (Success('\n'))
        tokeniser_.charLiteral.parse("'\\^J'") should be (Success('\n'))
    }

    "stringLiteral" should "parse valid haskell strings" in {
        tokeniser.stringLiteral.parse(""""This string should have correct\t\xa whitespace properties!\8."""") should be {
            Success("This string should have correct\t\n whitespace properties!\b.")
        }
        tokeniser.stringLiteral.parse(""""\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be {
            Success("I can write them like this!\n\n10")
        }
        tokeniser.stringLiteral.parse(""""Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        tokeniser.stringLiteral.parse("\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.stringLiteral.parse(""""This string should have correct\t\xa whitespace properties!\8."""") should be {
            Success("This string should have correct\t\n whitespace properties!\b.")
        }
        tokeniser_.stringLiteral.parse(""""\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be {
            Success("I can write them like this!\n\n10")
        }
        tokeniser_.stringLiteral.parse(""""Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        tokeniser_.stringLiteral.parse("\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
    }

    "rawStringLiteral" should "parse valid strings, without processing them" in {
        tokeniser.rawStringLiteral.parse(""""this string is completely raw\n, nothing should be \xa changed!"""") should be {
            Success("""this string is completely raw\n, nothing should be \xa changed!""")
        }
        tokeniser.rawStringLiteral.parse(""""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this"""") should be {
            Success("""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this""")
        }
        tokeniser.rawStringLiteral.parse(""""But we should be able to escape \", but it should remain like that!"""") should be {
            Success("""But we should be able to escape \", but it should remain like that!""")
        }
    }

    "natural" should "parse unsigned decimal numbers" in {
        tokeniser.natural.parse("0") should be (Success(0))
        tokeniser.natural.parse("1024") should be (Success(1024))
        tokeniser.natural.parse("1024  ") should be (Success(1024))
    }
    it should "parse unsigned hexadecimal numbers" in {
        tokeniser.natural.parse("0x340") should be (Success(0x340))
        tokeniser.natural.parse("0xFF") should be (Success(0xFF))
    }
    it should "parse unsigned octal numbers" in {
        tokeniser.natural.parse("0o201") should be (Success(129))
    }

    "integer" should "parse signed naturals" in {
        tokeniser.integer.parse("10") should be (Success(10))
        tokeniser.integer.parse("+10") should be (Success(10))
        tokeniser.integer.parse("-0xb") should be (Success(-0xb))
    }

    "decimal" should "parse unsigned integers in the decimal system" in {
        tokeniser.decimal.parse("123") should be (Success(123))
    }
    it should "not succeed when given no input" in {
        tokeniser.decimal.parse("") shouldBe a [Failure]
    }

    "hexadecimal" should "parse unsigned hexadecimal integers" in {
        tokeniser.hexadecimal.parse("0xff") should be (Success(255))
    }
    it should "require at least one digit" in {
        tokeniser.hexadecimal.parse("") shouldBe a [Failure]
        tokeniser.hexadecimal.parse("0") shouldBe a [Failure]
        tokeniser.hexadecimal.parse("0x") shouldBe a [Failure]
    }

    "unsignedFloat" should "parse unsigned fractional floats" in {
        tokeniser.unsignedFloat.parse("3.142") should be (Success(3.142))
        tokeniser.unsignedFloat.parse("0.23") should be (Success(0.23))
        tokeniser.unsignedFloat.parse("10.0") should be (Success(10.0))
    }
    it should "parse unsigned exponential floats" in {
        tokeniser.unsignedFloat.parse("3e10") should be (Success(3e10))
        tokeniser.unsignedFloat.parse("5E-4") should be (Success(5e-4))
    }
    it should "parse unsigned fractional exponential floats" in {
        tokeniser.unsignedFloat.parse("3.142e2") should be (Success(3.142e2))
        tokeniser.unsignedFloat.parse("0.23e1") should be (Success(0.23e1))
        tokeniser.unsignedFloat.parse("10.0e-5") should be (Success(10.0e-5))
    }
    it should "not parse integers" in {
        tokeniser.unsignedFloat.parse("3") shouldBe a [Failure]
    }
    it should "not allow .1 or 1." in {
        tokeniser.unsignedFloat.parse(".0") shouldBe a [Failure]
        tokeniser.unsignedFloat.parse("0.") shouldBe a [Failure]
    }

    "float" should "parse signed floats" in {
        tokeniser.float.parse("-3.142") should be (Success(-3.142))
        tokeniser.float.parse("-3e-4") should be (Success(-3e-4))
        tokeniser.float.parse("+1.2e2") should be (Success(1.2e2))
        tokeniser.float.parse("1.2") should be (Success(1.2))
    }

    "naturalOrFloat" should "parse either naturals or unsigned floats" in {
        tokeniser.naturalOrFloat.parse("3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        tokeniser.naturalOrFloat.parse("0.23") should be (Success(Right(0.23)))
        tokeniser.naturalOrFloat.parse("10.0\n") should be (Success(Right(10.0)))
        tokeniser.naturalOrFloat.parse("3e10") should be (Success(Right(3e10)))
        tokeniser.naturalOrFloat.parse("5E-4") should be (Success(Right(5e-4)))
        tokeniser.naturalOrFloat.parse("3.142e2\t ") should be (Success(Right(3.142e2)))
        tokeniser.naturalOrFloat.parse("0.23e1") should be (Success(Right(0.23e1)))
        tokeniser.naturalOrFloat.parse("10.0e-5") should be (Success(Right(10.0e-5)))
        tokeniser.naturalOrFloat.parse("1024") should be (Success(Left(1024)))
        tokeniser.naturalOrFloat.parse("0x340") should be (Success(Left(0x340)))
        tokeniser.naturalOrFloat.parse("0xFF") should be (Success(Left(0xFF)))
        tokeniser.naturalOrFloat.parse("0o201 //ooh, octal") should be (Success(Left(129)))
    }
    it should "not allow hexadecimal floats" in {
        (tokeniser.naturalOrFloat <* eof).parse("0x340.0") shouldBe a [Failure]
    }
    it should "not allow octal floats" in {
        (tokeniser.naturalOrFloat <* eof).parse("0o201.0") shouldBe a [Failure]
    }

    "number" should "parse integers or floats" in {
        tokeniser.number.parse("3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        tokeniser.number.parse("-0.23") should be (Success(Right(-0.23)))
        tokeniser.number.parse("10.0\n") should be (Success(Right(10.0)))
        tokeniser.number.parse("+3e10") should be (Success(Right(3e10)))
        tokeniser.number.parse("5E-4") should be (Success(Right(5e-4)))
        tokeniser.number.parse("3.142e2\t ") should be (Success(Right(3.142e2)))
        tokeniser.number.parse("+0.23e1") should be (Success(Right(0.23e1)))
        tokeniser.number.parse("10.0e-5") should be (Success(Right(10.0e-5)))
        tokeniser.number.parse("-1024") should be (Success(Left(-1024)))
        tokeniser.number.parse("0x340") should be (Success(Left(0x340)))
        tokeniser.number.parse("0xFF") should be (Success(Left(0xFF)))
        tokeniser.number.parse("0o201 //ooh, octal") should be (Success(Left(129)))
    }

    "skipComments" should "parse single-line comments" in {
        (tokeniser.skipComments <* eof).parse("// hello world!") should be (Success(()))
        (tokeniser.skipComments *> '\n' *> 'a').parse("// hello world!\na") should be (Success('a'))
    }
    it should "parse multi-line comments" in {
        (tokeniser.skipComments <* eof).parse("/* hello *w/orld!*/") should be (Success(()))
        (tokeniser.skipComments *> 'a').parse("/* hello *w/orld!*/a") should be (Success('a'))
        (tokeniser.skipComments *> '\n' *> 'a').parse("/* hello world!*///another comment\na") should be (Success('a'))
    }
    it should "parse nested comments when applicable" in {
        (tokeniser.skipComments <* eof).parse("/*/*hello world*/ this /*comment*/ is nested*/") should be (Success(()))
        (tokeniser.skipComments <* eof).parse("/*/*hello world*/ this /*comment is nested*/") shouldBe a [Failure]
    }
    it should "not parse nested comments when applicable" in {
        (tokeniser_.skipComments <* eof).parse("/*/*hello world*/ this /*comment*/ is nested*/") shouldBe a [Failure]
        (tokeniser_.skipComments <* eof).parse("/*/*hello world this /*comment is nested*/") should be (Success(()))
    }

    "whiteSpace" should "parse all whitespace" in {
        (tokeniser.whiteSpace <* eof).parse(" \n\t \r\n ") should not be a [Failure]
    }
    it should "parse comments interleaved with spaces" in {
        (tokeniser.whiteSpace <* eof).parse("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should not be a [Failure]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser.whiteSpace <* eof).parse(" \n\t \r\n ") should equal {
            (tokeniser_.whiteSpace <* eof).parse(" \n\t \r\n ")
        }
        (tokeniser.whiteSpace <* eof).parse("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should equal {
            (tokeniser_.whiteSpace <* eof).parse("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ")
        }
    }

    "comments" should "not aggressively eat everything" in {
        val lexer1 = new token.Lexer(token.LanguageDef.plain.copy(commentLine = "//", space = token.Parser(Parsley.empty)))
        val lexer2 = new token.Lexer(token.LanguageDef.plain.copy(commentStart = "/*", commentEnd = "*/", space = token.Parser(Parsley.empty)))
        val lexer3 = new token.Lexer(token.LanguageDef.plain.copy(commentLine = "//", commentStart = "/*", commentEnd = "*/", space = token.Parser(Parsley.empty)))
        (lexer1.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
        (lexer2.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
        (lexer3.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
    }
}
