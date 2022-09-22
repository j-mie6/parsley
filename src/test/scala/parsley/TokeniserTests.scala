/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley._
import Parsley.col
import parsley.character.{letterOrDigit, letter, whitespace, oneOf => inSet}
import parsley.implicits.character.charLift
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
            token.Parser(letterOrDigit <|> '_'),
            token.Parser(inSet('+', '-', ':', '/', '*', '=')),
            token.Parser(inSet('+', '-', '/', '*')),
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
            token.CharSet('+', '-', ':', '/', '*', '='),
            token.CharSet('+', '-', '/', '*'),
            Set("if", "else", "for", "yield", "while", "def", "class",
                "trait", "abstract", "override"),
            Set(":", "=", "::", ":="),
            true,
            token.Predicate(character.isWhitespace))
    val tokeniser = new token.Lexer(scala)
    val tokeniser_ = new token.Lexer(scala_)

    "identifier" should "read valid identifiers" in {
        (tokeniser.lexeme.names.identifier <* eof).parse("foo123 ") should be (Success("foo123"))
        (tokeniser.lexeme.names.identifier <* eof).parse("_bar") should be (Success("_bar"))
        (tokeniser.lexeme.names.identifier <* eof).parse("iffy") should be (Success("iffy"))
        (tokeniser.lexeme.names.identifier <* eof).parse("1_bar") shouldBe a [Failure[_]]
    }
    it should "fail if the result is a keyword" in {
        (tokeniser.lexeme.names.identifier <* eof).parse("class") shouldBe a [Failure[_]]
    }
    it should "point at the correct place for the error" in {
        (tokeniser.lexeme.names.identifier <* eof).parse("class") should matchPattern {
            case Failure(TestError((1, 1), _)) =>
        }
    }
    it should "report the correct labels" in {
        inside((tokeniser.lexeme.names.identifier <* eof).parse("class")) {
            case Failure(TestError(_, VanillaError(unexpected, expecteds, reasons))) =>
                unexpected should contain (Named("keyword class"))
                expecteds should contain only (Named("identifier"))
                reasons shouldBe empty
        }
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.lexeme.names.identifier <* eof).parse("foo123 ") should be (Success("foo123"))
        (tokeniser_.lexeme.names.identifier <* eof).parse("_bar") should be (Success("_bar"))
        (tokeniser_.lexeme.names.identifier <* eof).parse("iffy") should be (Success("iffy"))
        (tokeniser_.lexeme.names.identifier <* eof).parse("1_bar") should equal {
            (tokeniser.lexeme.names.identifier <* eof).parse("1_bar")
        }
        (tokeniser_.lexeme.names.identifier <* eof).parse("class") shouldBe a [Failure[_]]
        (tokeniser_.lexeme.names.identifier <* eof).parse("class") should matchPattern {
            case Failure(TestError((1, 1), _)) =>
        }
        inside((tokeniser_.lexeme.names.identifier <* eof).parse("class")) {
            case Failure(TestError(_, VanillaError(unexpected, expecteds, reasons))) =>
                unexpected should contain (Named("keyword class"))
                expecteds should contain only (Named("identifier"))
                reasons shouldBe empty
        }
    }

    "keyword" should "match valid keywords" in {
        tokeniser.lexeme.symbol.softKeyword("if").parse("if then") should be (Success(()))
        tokeniser.lexeme.symbol.softKeyword("volatile").parse("volatile") should be (Success(()))
    }
    it should "fail if the input has more identifier letters" in {
        tokeniser.lexeme.symbol.softKeyword("if").parse("ifthen") shouldBe a [Failure[_]]
        tokeniser.lexeme.symbol.softKeyword("volatile").parse("volatilev") shouldBe a [Failure[_]]
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.lexeme.symbol.softKeyword("if").parse("if then") should be (Success(()))
        tokeniser_.lexeme.symbol.softKeyword("volatile").parse("volatile") should be (Success(()))
        tokeniser_.lexeme.symbol.softKeyword("if").parse("ifthen") should equal {
            tokeniser.lexeme.symbol.softKeyword("if").parse("ifthen")
        }
        tokeniser_.lexeme.symbol.softKeyword("volatile").parse("volatilev") should equal {
            tokeniser.lexeme.symbol.softKeyword("volatile").parse("volatilev")
        }
    }
    it must "not consume input on failure" in {
        (tokeniser.lexeme.symbol.softKeyword("if") <|> tokeniser.lexeme.names.identifier).parse("id") should be (Success("id"))
    }

    "userOp" should "read valid operator" in {
        (tokeniser.lexeme.names.userOp <* eof).parse(":++") should be (Success(":++"))
        (tokeniser.lexeme.names.userOp <* eof).parse(":++h") shouldBe a [Failure[_]]
    }
    it should "fail if the result is reserved" in {
        (tokeniser.lexeme.names.userOp <* eof).parse(":") shouldBe a [Failure[_]]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.lexeme.names.userOp <* eof).parse(":++") should be (Success(":++"))
        (tokeniser_.lexeme.names.userOp <* eof).parse(":++h") shouldBe a [Failure[_]]
        (tokeniser_.lexeme.names.userOp <* eof).parse(":") shouldBe a [Failure[_]]
    }

    "reservedOp" should "match valid reserved operators" in {
        (tokeniser.lexeme.names.reservedOp <* eof).parse("=") should be (Success("="))
        (tokeniser.lexeme.names.reservedOp <* eof).parse(":") should be (Success(":"))
    }
    it should "fail if the result isn't reserved" in {
        (tokeniser.lexeme.names.reservedOp <* eof).parse("+") shouldBe a [Failure[_]]
        (tokeniser.lexeme.names.reservedOp <* eof).parse("::=") shouldBe a [Failure[_]]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.lexeme.names.reservedOp <* eof).parse("=") should be (Success("="))
        (tokeniser_.lexeme.names.reservedOp <* eof).parse(":") should be (Success(":"))
        (tokeniser_.lexeme.names.reservedOp <* eof).parse("+") shouldBe a [Failure[_]]
        (tokeniser_.lexeme.names.reservedOp <* eof).parse("::=") shouldBe a [Failure[_]]
    }

    "operator" should "match valid operators" in {
        (tokeniser.lexeme.symbol.operator("=") <* eof).parse("=") should be (Success(()))
        (tokeniser.lexeme.symbol.operator(":") <* eof).parse(":") should be (Success(()))
        (tokeniser.lexeme.symbol.operator("++") <* eof).parse("++") should be (Success(()))
    }
    it should "fail if the input has more operator letters" in {
        (tokeniser.lexeme.symbol.operator("=") <* eof).parse("=+") shouldBe a [Failure[_]]
        (tokeniser.lexeme.symbol.operator(":") <* eof).parse("::") shouldBe a [Failure[_]]
        (tokeniser.lexeme.symbol.operator("++") <* eof).parse("++=") shouldBe a [Failure[_]]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser_.lexeme.symbol.operator("=") <* eof).parse("=") should equal {
            (tokeniser.lexeme.symbol.operator("=") <* eof).parse("=")
        }
        (tokeniser_.lexeme.symbol.operator(":") <* eof).parse(":") should equal {
            (tokeniser.lexeme.symbol.operator(":") <* eof).parse(":")
        }
        (tokeniser_.lexeme.symbol.operator("++") <* eof).parse("++") should equal {
            (tokeniser.lexeme.symbol.operator("++") <* eof).parse("++")
        }
        (tokeniser_.lexeme.symbol.operator("=") <* eof).parse("=+") should equal {
            (tokeniser.lexeme.symbol.operator("=") <* eof).parse("=+")
        }
        (tokeniser_.lexeme.symbol.operator(":") <* eof).parse("::") should equal {
            (tokeniser.lexeme.symbol.operator(":") <* eof).parse("::")
        }
        (tokeniser_.lexeme.symbol.operator("++") <* eof).parse("++=") should equal {
            (tokeniser.lexeme.symbol.operator("++") <* eof).parse("++=")
        }
        (tokeniser_.lexeme.symbol.operator("+") <|> tokeniser_.lexeme.symbol.operator("++") <* eof).parse("++") should equal {
            (tokeniser.lexeme.symbol.operator("+") <|> tokeniser.lexeme.symbol.operator("++") <* eof).parse("++")
        }
    }

    "maxOp" should "match valid operators" in {
        (tokeniser_.lexeme.symbol.operator("=") <* eof).parse("=") should be (Success(()))
        (tokeniser_.lexeme.symbol.operator(":") <* eof).parse(":") should be (Success(()))
        (tokeniser_.lexeme.symbol.operator("++") <* eof).parse("++") should be (Success(()))
        (tokeniser_.lexeme.symbol.operator("+:") <* ':' <* eof).parse("+::") should be (Success(()))
        (tokeniser_.lexeme.symbol.operator("=") <* '=' <* eof).parse("==") should be (Success(()))
    }
    it must "fail if the operator is a valid prefix of another operator and that operator is parsable" in {
        (tokeniser_.lexeme.symbol.operator(":") <* '=' <* eof).parse(":=") shouldBe a [Failure[_]]
        (tokeniser_.lexeme.symbol.operator(":") <* ':' <* eof).parse("::") shouldBe a [Failure[_]]
    }

    "charLiteral" should "parse valid haskell characters" in {
        tokeniser.lexeme.text.character.basicMultilingualPlane.parse("'a'") should be (Success('a'))
        tokeniser.lexeme.text.character.basicMultilingualPlane.parse("'\\n'") should be (Success('\n'))
        tokeniser.lexeme.text.character.basicMultilingualPlane.parse("'\\xa'") should be (Success('\n'))
        tokeniser.lexeme.text.character.basicMultilingualPlane.parse("'\\^J'") should be (Success('\n'))
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("'a'") should be (Success('a'))
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("'\\n'") should be (Success('\n'))
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("'\\xa'") should be (Success('\n'))
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("'\\^J'") should be (Success('\n'))
    }
    it must "fail gracefully if there is no closing quote" in {
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("\'") shouldBe a [Failure[_]]
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("\'\\") shouldBe a [Failure[_]]
    }
    it must "fail if given the zero-width char" in {
        tokeniser.lexeme.text.character.basicMultilingualPlane.parse("'\\&'") shouldBe a [Failure[_]]
        tokeniser_.lexeme.text.character.basicMultilingualPlane.parse("'\\&'") shouldBe a [Failure[_]]
    }
    /*it should be "able to handle unicode characters" in {

    }*/

    "stringLiteral" should "parse valid haskell strings" in {
        tokeniser.lexeme.text.string.unicode.parse(""""This string should have correct\t\xa whitespace properties!\8\^@."""") should be {
            Success("This string should have correct\t\n whitespace properties!\b\u0000.")
        }
        tokeniser.lexeme.text.string.unicode.parse(""""\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be {
            Success("I can write them like this!\n\n10")
        }
        tokeniser.lexeme.text.string.unicode.parse(""""Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        tokeniser.lexeme.text.string.unicode.parse("\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
        (tokeniser.lexeme.text.string.unicode <~> col).parse(""""\49\&0"""") should be {
            Success(("10", 9))
        }
    }
    it must "be the same regardless of the intrinsic" in {
        tokeniser_.lexeme.text.string.unicode.parse(""""This string should have correct\t\xa whitespace properties!\8\^@."""") should be {
            Success("This string should have correct\t\n whitespace properties!\b\u0000.")
        }
        tokeniser_.lexeme.text.string.unicode.parse(""""\73\32\99\97\x6e\x20\x77\x72\o151\o164\o145\o40\116\104\101\109\x20\x6c\x69\x6b\o145\o40\o164\o150is!\^J\LF\49\&0"""") should be {
            Success("I can write them like this!\n\n10")
        }
        tokeniser_.lexeme.text.string.unicode.parse(""""Here we test a string with a break in it \                  \which shouldn't show up in the end!"""") should be {
            Success("Here we test a string with a break in it which shouldn't show up in the end!")
        }
        tokeniser_.lexeme.text.string.unicode.parse("\"Breaks can also contain newline\\   \n \\s, but we still don't notice them\"") should be {
            Success("Breaks can also contain newlines, but we still don't notice them")
        }
        (tokeniser_.lexeme.text.string.unicode <~> col).parse(""""\49\&0"""") should be {
            Success(("10", 9))
        }
    }
    it must "handle unicode characters in the literal properly" in {
        tokeniser.lexeme.text.string.unicode.parse("\"\\x1F642\"") should be {
            Success("🙂")
        }
        tokeniser_.lexeme.text.string.unicode.parse("\"\\x1F642\"") should be {
            Success("🙂")
        }
        tokeniser.lexeme.text.string.unicode.parse("\"🙂\"") should be {
            Success("🙂")
        }
        tokeniser_.lexeme.text.string.unicode.parse("\"🙂\"") should be {
            Success("🙂")
        }
    }

    "rawStringLiteral" should "parse valid strings, without processing them" in {
        tokeniser.nonlexeme.text.rawString.unicode.parse(""""this string is completely raw\n, nothing should be \xa changed!"""") should be {
            Success("""this string is completely raw\n, nothing should be \xa changed!""")
        }
        tokeniser.nonlexeme.text.rawString.unicode.parse(""""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this"""") should be {
            Success("""Not even \\\n\\n\n\n\n\n\\\j\joijs\\jsj this""")
        }
        tokeniser.nonlexeme.text.rawString.unicode.parse(""""And we shouldn't be able to escape \", sorry!"""") should be {
            Success("""And we shouldn't be able to escape \""")
        }
    }

    "natural" should "parse unsigned decimal numbers" in {
        tokeniser.lexeme.numeric.natural.decimal.parse("0") should be (Success(0))
        tokeniser.lexeme.numeric.natural.decimal.parse("1024") should be (Success(1024))
        tokeniser.lexeme.numeric.natural.decimal.parse("1024  ") should be (Success(1024))
        tokeniser.lexeme.numeric.natural.number.parse("0") should be (Success(0))
        tokeniser.lexeme.numeric.natural.number.parse("1024") should be (Success(1024))
        tokeniser.lexeme.numeric.natural.number.parse("1024  ") should be (Success(1024))
    }
    it should "parse unsigned hexadecimal numbers" in {
        tokeniser.lexeme.numeric.natural.hexadecimal.parse("0x340") should be (Success(0x340))
        tokeniser.lexeme.numeric.natural.hexadecimal.parse("0xFF") should be (Success(0xFF))
        tokeniser.lexeme.numeric.natural.number.parse("0x340") should be (Success(0x340))
        tokeniser.lexeme.numeric.natural.number.parse("0xFF") should be (Success(0xFF))
    }
    it should "parse unsigned octal numbers" in {
        tokeniser.lexeme.numeric.natural.octal.parse("0o201") should be (Success(129))
        tokeniser.lexeme.numeric.natural.number.parse("0o201") should be (Success(129))
    }

    "integer" should "parse signed naturals" in {
        tokeniser.lexeme.numeric.integer.number.parse("10") should be (Success(10))
        tokeniser.lexeme.numeric.integer.number.parse("+10") should be (Success(10))
        tokeniser.lexeme.numeric.integer.number.parse("-0xb") should be (Success(-0xb))
    }

    "decimal" should "parse unsigned integers in the decimal system" in {
        tokeniser.lexeme.numeric.unsigned.decimal.parse("123") should be (Success(123))
    }
    it should "not succeed when given no input" in {
        tokeniser.lexeme.numeric.unsigned.decimal.parse("") shouldBe a [Failure[_]]
    }

    "hexadecimal" should "parse unsigned hexadecimal integers" in {
        tokeniser.lexeme.numeric.unsigned.hexadecimal.parse("0xff") should be (Success(255))
    }
    it should "require at least one digit" in {
        tokeniser.lexeme.numeric.unsigned.hexadecimal.parse("") shouldBe a [Failure[_]]
        tokeniser.lexeme.numeric.unsigned.hexadecimal.parse("0") shouldBe a [Failure[_]]
        tokeniser.lexeme.numeric.unsigned.hexadecimal.parse("0x") shouldBe a [Failure[_]]
    }

    /*"unsignedFloat" should "parse unsigned fractional floats" in {
        tokeniser.lexemes.unsignedFloat.parse("3.142") should be (Success(3.142))
        tokeniser.lexemes.unsignedFloat.parse("0.23") should be (Success(0.23))
        tokeniser.lexemes.unsignedFloat.parse("10.0") should be (Success(10.0))
    }
    it should "parse unsigned exponential floats" in {
        tokeniser.lexemes.unsignedFloat.parse("3e10") should be (Success(3e10))
        tokeniser.lexemes.unsignedFloat.parse("5E-4") should be (Success(5e-4))
    }
    it should "parse unsigned fractional exponential floats" in {
        tokeniser.lexemes.unsignedFloat.parse("3.142e2") should be (Success(3.142e2))
        tokeniser.lexemes.unsignedFloat.parse("0.23e1") should be (Success(0.23e1))
        tokeniser.lexemes.unsignedFloat.parse("10.0e-5") should be (Success(10.0e-5))
    }
    it should "not parse integers" in {
        tokeniser.lexemes.unsignedFloat.parse("3") shouldBe a [Failure[_]]
    }
    it should "not allow .1 or 1." in {
        tokeniser.lexemes.unsignedFloat.parse(".0") shouldBe a [Failure[_]]
        tokeniser.lexemes.unsignedFloat.parse("0.") shouldBe a [Failure[_]]
    }*/

    "double" should "parse signed doubles" in {
        tokeniser.lexeme.numeric.real.double.parse("-3.142") should be (Success(-3.142))
        tokeniser.lexeme.numeric.real.double.parse("-3e-4") should be (Success(-3e-4))
        tokeniser.lexeme.numeric.real.double.parse("+1.2e2") should be (Success(1.2e2))
        tokeniser.lexeme.numeric.real.double.parse("1.2") should be (Success(1.2))
        tokeniser.lexeme.numeric.real.double.parse("0.2") should be (Success(0.2))
    }

    "naturalOrFloat" should "parse either naturals or unsigned floats" in {
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("0.23") should be (Success(Right(0.23)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("10.0\n") should be (Success(Right(10.0)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("3e10") should be (Success(Right(3e10)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("5E-4") should be (Success(Right(5e-4)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("3.142e2\t ") should be (Success(Right(3.142e2)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("0.23e1") should be (Success(Right(0.23e1)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("10.0e-5") should be (Success(Right(10.0e-5)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("1024") should be (Success(Left(1024)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("0x340") should be (Success(Left(0x340)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("0xFF") should be (Success(Left(0xFF)))
        tokeniser.lexeme.numeric.unsignedCombined.number.parse("0o201 //ooh, octal") should be (Success(Left(129)))
    }
    // Now they do :)
    it should "not allow hexadecimal floats without the exponent" in {
        (tokeniser.lexeme.numeric.unsignedCombined.number <* eof).parse("0x340.0") shouldBe a [Failure[_]]
    }
    it should "not allow octal floats without the exponent" in {
        (tokeniser.lexeme.numeric.unsignedCombined.number <* eof).parse("0o201.0") shouldBe a [Failure[_]]
    }

    "number" should "parse integers or floats" in {
        tokeniser.lexeme.numeric.signedCombined.number.parse("3.142  /*what a sick number am I right*/") should be (Success(Right(3.142)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("-0.23") should be (Success(Right(-0.23)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("10.0\n") should be (Success(Right(10.0)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("+3e10") should be (Success(Right(3e10)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("5E-4") should be (Success(Right(5e-4)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("3.142e2\t ") should be (Success(Right(3.142e2)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("+0.23e1") should be (Success(Right(0.23e1)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("10.0e-5") should be (Success(Right(10.0e-5)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("-1024") should be (Success(Left(-1024)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("0x340") should be (Success(Left(0x340)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("0xFF") should be (Success(Left(0xFF)))
        tokeniser.lexeme.numeric.signedCombined.number.parse("0o201 //ooh, octal") should be (Success(Left(129)))
    }

    "skipComments" should "parse single-line comments" in {
        (tokeniser.space.skipComments <* eof).parse("// hello world!") should be (Success(()))
        (tokeniser.space.skipComments *> '\n' *> 'a').parse("// hello world!\na") should be (Success('a'))
    }
    it should "parse multi-line comments" in {
        (tokeniser.space.skipComments <* eof).parse("/* hello *w/orld!*/") should be (Success(()))
        (tokeniser.space.skipComments *> 'a').parse("/* hello *w/orld!*/a") should be (Success('a'))
        (tokeniser.space.skipComments *> '\n' *> 'a').parse("/* hello world!*///another comment\na") should be (Success('a'))
    }
    it should "parse nested comments when applicable" in {
        (tokeniser.space.skipComments <* eof).parse("/*/*hello world*/ this /*comment*/ is nested*/") should be (Success(()))
        (tokeniser.space.skipComments <* eof).parse("/*/*hello world*/ this /*comment is nested*/") shouldBe a [Failure[_]]
    }
    it should "not parse nested comments when applicable" in {
        (tokeniser_.space.skipComments <* eof).parse("/*/*hello world*/ this /*comment*/ is nested*/") shouldBe a [Failure[_]]
        (tokeniser_.space.skipComments <* eof).parse("/*/*hello world this /*comment is nested*/") should be (Success(()))
    }
    it should "do nothing with no comments" in {
        (tokeniser.space.skipComments).parse("aaa") should be (Success(()))
        (tokeniser_.space.skipComments).parse("aaa") should be (Success(()))
        val lang = token.LanguageDef.plain.copy(
            commentLine = "--",
            commentStart = "{-", // no shared prefix with the single line
            commentEnd = "-}"
            )
        val tokeniser__ = new token.Lexer(lang)
        tokeniser__.space.skipComments.parse("aaa") should be (Success(()))
    }

    "whiteSpace" should "parse all whitespace" in {
        (tokeniser.space.whiteSpace <* eof).parse(" \n\t \r\n ") should not be a [Failure[_]]
    }
    it should "parse comments interleaved with spaces" in {
        (tokeniser.space.whiteSpace <* eof).parse("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should not be a [Failure[_]]
    }
    it must "be the same regardless of the intrinsic" in {
        (tokeniser.space.whiteSpace <* eof).parse(" \n\t \r\n ") should equal {
            (tokeniser_.space.whiteSpace <* eof).parse(" \n\t \r\n ")
        }
        (tokeniser.space.whiteSpace <* eof).parse("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ") should equal {
            (tokeniser_.space.whiteSpace <* eof).parse("/*this comment*/ /*is spaced out*/\n//by whitespace!\n ")
        }
    }

    "comments" should "not aggressively eat everything" in {
        val lexer1 = new token.Lexer(token.LanguageDef.plain.copy(commentLine = "//", space = token.Parser(Parsley.empty)))
        val lexer2 = new token.Lexer(token.LanguageDef.plain.copy(commentStart = "/*", commentEnd = "*/", space = token.Parser(Parsley.empty)))
        val lexer3 = new token.Lexer(token.LanguageDef.plain.copy(commentLine = "//", commentStart = "/*", commentEnd = "*/", space = token.Parser(Parsley.empty)))
        (lexer1.space.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
        (lexer2.space.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
        (lexer3.space.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
    }

    "case sensitivity" should "work for both lowercase and uppercase specified keywords" in {
        val lexer = new token.Lexer(token.LanguageDef.plain.copy(caseSensitive = false, keywords = Set("hi", "HELLo", "BYE")))
        lexer.lexeme.names.identifier.parse("hi") shouldBe a [Failure[_]]
        lexer.lexeme.names.identifier.parse("hello") shouldBe a [Failure[_]]
        lexer.lexeme.names.identifier.parse("bye") shouldBe a [Failure[_]]
        lexer.lexeme.names.identifier.parse("hI") shouldBe a [Failure[_]]
        lexer.lexeme.names.identifier.parse("hELLo") shouldBe a [Failure[_]]
        lexer.lexeme.names.identifier.parse("bYe") shouldBe a [Failure[_]]
        lexer.lexeme.symbol.softKeyword("HELLO").parse("HELLO") shouldBe a [Success[_]]
        lexer.lexeme.symbol.softKeyword("HELLO").parse("hello") shouldBe a [Success[_]]
        lexer.lexeme.symbol.softKeyword("BYE").parse("bye") shouldBe a [Success[_]]
        lexer.lexeme.symbol.softKeyword("hi").parse("HI") shouldBe a [Success[_]]
    }

    it should "not be affected by tablification optimisation" in {
        val lexer = new token.Lexer(token.LanguageDef.plain.copy(caseSensitive = false, keywords = Set("hi", "HELLo", "BYE")))
        val p = lexer.lexeme.symbol.softKeyword("hi") <|> lexer.lexeme.symbol.softKeyword("HELLo") <|> lexer.lexeme.symbol.softKeyword("BYE")
        p.parse("bye") shouldBe a [Success[_]]
        p.parse("Bye") shouldBe a [Success[_]]
    }

    "issue #199" should "not regress: whitespace should work without comments defined" in {
        val lang = token.LanguageDef.plain.copy(space = token.Predicate(_.isWhitespace))
        val lexer = new token.Lexer(lang)
        lexer.space.whiteSpace.parse("[") shouldBe a [Success[_]]
    }
}
