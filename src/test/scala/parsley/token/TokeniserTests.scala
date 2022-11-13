/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import parsley._
import Parsley.col
import parsley.character.{letterOrDigit, letter, whitespace, oneOf => inSet}
import parsley.implicits.character.charLift
import parsley.combinator.eof

import token.{descriptions => desc}

import scala.language.implicitConversions

class TokeniserTests extends ParsleyTest {
    val scala =
        desc.LexicalDesc(
            desc.NameDesc(identifierStart = token.predicate._CharSet(('a' to 'z').toSet ++ ('A' to 'Z').toSet + '_'),
                          identifierLetter = token.predicate._CharSet(('a' to 'z').toSet ++ ('A' to 'Z').toSet ++ ('0' to '9').toSet + '_'),
                          operatorStart = token.predicate._CharSet('+', '-', ':', '/', '*', '='),
                          operatorLetter = token.predicate._CharSet('+', '-', '/', '*')),
            desc.SymbolDesc(hardKeywords = Set("if", "else", "for", "yield", "while", "def", "class",
                                               "trait", "abstract", "override", "val", "var", "lazy"),
                            hardOperators = Set(":", "=", "::", ":="),
                            caseSensitive = true),
            desc.numeric.NumericDesc.plain,
            desc.text.TextDesc.plain,
            desc.SpaceDesc(commentStart = "/*",
                           commentEnd = "*/",
                           commentLine = "//",
                           commentLineAllowsEOF = true,
                           nestedComments = true,
                           space = token.predicate.Basic(character.isWhitespace),
                           whitespaceIsContextDependent = false))
    val scala_ =
        scala.copy(
            spaceDesc = scala.spaceDesc.copy(nestedComments = false)
        )
    val tokeniser = new token.Lexer(scala)
    val tokeniser_ = new token.Lexer(scala_)

    // TODO: separators, enclosing, space, fully

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
        val lang = desc.LexicalDesc.plain.copy(spaceDesc = desc.SpaceDesc.plain.copy(
                commentLine = "--",
                commentStart = "{-", // no shared prefix with the single line
                commentEnd = "-}"
            ))
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
        val lexer1 = new token.Lexer(desc.LexicalDesc.plain.copy(spaceDesc = desc.SpaceDesc.plain.copy(commentLine = "//", space = token.predicate.NotRequired)))
        val lexer2 = new token.Lexer(desc.LexicalDesc.plain.copy(spaceDesc = desc.SpaceDesc.plain.copy(commentStart = "/*", commentEnd = "*/", space = token.predicate.NotRequired)))
        val lexer3 = new token.Lexer(desc.LexicalDesc.plain.copy(spaceDesc = desc.SpaceDesc.plain.copy(commentLine = "//", commentStart = "/*", commentEnd = "*/", space = token.predicate.NotRequired)))
        (lexer1.space.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
        (lexer2.space.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
        (lexer3.space.whiteSpace *> 'a').parse("a") shouldBe a [Success[_]]
    }

    "issue #199" should "not regress: whitespace should work without comments defined" in {
        val lang = desc.LexicalDesc.plain.copy(spaceDesc = desc.SpaceDesc.plain.copy(space = token.predicate.Basic(_.isWhitespace)))
        val lexer = new token.Lexer(lang)
        lexer.space.whiteSpace.parse("[") shouldBe a [Success[_]]
    }
}
