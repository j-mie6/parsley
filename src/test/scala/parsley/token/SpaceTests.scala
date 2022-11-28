package parsley.token

import Predef.{ArrowAssoc => _, _}

import parsley.{Success, ParsleyTest}

import descriptions.{SpaceDesc, LexicalDesc}
import parsley.character.{string, char}

class SpaceTests extends ParsleyTest {
    private def makeSpace(space: SpaceDesc) = new Lexer(LexicalDesc.plain.copy(spaceDesc = space)).space

    val basicNoComments = SpaceDesc.plain.copy(space = predicate.Basic(Character.isWhitespace))
    val unicodeNoComments = basicNoComments.copy(space = predicate.Unicode(Character.isWhitespace))

    "whiteSpace" should "parse spaces when no comments are defined" in cases(makeSpace(basicNoComments).whiteSpace *> string("a")) (
        "a" -> Some("a"),
        "      a" -> Some("a"),
        "\n   \ta" -> Some("a"),
        "/**/ a" -> None,
    )

    it should "supported unicode definition" in cases(makeSpace(unicodeNoComments).whiteSpace *> string("a")) (
        "a" -> Some("a"),
        "      a" -> Some("a"),
        "\n   \ta" -> Some("a"),
        "/**/ a" -> None,
    )

    val basicLine = basicNoComments.copy(commentLine = "--")
    val unicodeLine = unicodeNoComments.copy(commentLine = "--")

    it should "parse spaces and line comments when defined" in {
        cases(makeSpace(basicLine).whiteSpace *> string("a")) (
            "a" -> Some("a"),
            "      a" -> Some("a"),
            "\n   \ta" -> Some("a"),
            "--ab\n --hi\n a" -> Some("a"),
            "--aba" -> None,
        )
        cases(makeSpace(unicodeLine).whiteSpace *> string("a")) (
            "a" -> Some("a"),
            "      a" -> Some("a"),
            "\n   \ta" -> Some("a"),
            "--ab\n --hi\n a" -> Some("a"),
            "--aba" -> None,
        )
    }

    val basicMulti = basicNoComments.copy(commentStart = "/*", commentEnd = "*/")
    val unicodeMulti = unicodeNoComments.copy(commentStart = "/*", commentEnd = "*/")

    it should "parse spaces and multi-line comment when defined" in {
        cases(makeSpace(basicMulti).whiteSpace *> string("a")) (
            "a" -> Some("a"),
            "      a" -> Some("a"),
            "\n   \ta" -> Some("a"),
            "/*ab*/ /*hi*/ a" -> Some("a"),
            "/*aba" -> None,
        )
        cases(makeSpace(unicodeMulti).whiteSpace *> string("a")) (
            "a" -> Some("a"),
            "      a" -> Some("a"),
            "\n   \ta" -> Some("a"),
            "/*ab*/ /*hi*/ a" -> Some("a"),
            "/*aba" -> None,
        )
    }

    val basicMixed = basicNoComments.copy(commentLine = "#", commentStart = "##", commentEnd = "##")
    val unicodeMixed = unicodeNoComments.copy(commentLine = "#", commentStart = "##", commentEnd = "##")

    it should "parse spaces and mixed comments when defined" in {
        cases(makeSpace(basicMixed).whiteSpace *> string("a")) (
            "a" -> Some("a"),
            "      a" -> Some("a"),
            "\n   \ta" -> Some("a"),
            "##ab## #hi\n a" -> Some("a"),
            "##aba" -> None,
            "#aba" -> None,
        )
        cases(makeSpace(unicodeMixed).whiteSpace *> string("a")) (
            "a" -> Some("a"),
            "      a" -> Some("a"),
            "\n   \ta" -> Some("a"),
            "##ab## #hi\n a" -> Some("a"),
            "##aba" -> None,
            "#aba" -> None,
        )
    }

    val basicCommentsOnly = basicMixed.copy(space = predicate.NotRequired)
    val unicodeCommentsOnly = unicodeMixed.copy(space = predicate.NotRequired)

    it should "be skipComments with no whitespace allowed" in {
        val basic = makeSpace(basicCommentsOnly)
        val unicode = makeSpace(unicodeCommentsOnly)
        basic.whiteSpace shouldBe basic.skipComments
        unicode.whiteSpace shouldBe unicode.skipComments
    }

    val basicLineEOF = basicLine.copy(commentLineAllowsEOF = true)
    val basicLineNoEOF = basicLine.copy(commentLineAllowsEOF = false)
    
    it should "allow for line comments to end in EOF" in cases(makeSpace(basicLineEOF).whiteSpace) (
        "--hello world" -> Some(())
    )

    it should "or not allow EOF" in cases(makeSpace(basicLineNoEOF).whiteSpace) (
        "--hello world" -> None
    )

    it should "parse nested comments when applicable" in pending
    it should "not parse nested comments when applicable" in pending

    "skipComments" should "parse single-line comments" in pending
    it should "parse multi-line comments" in pending
    it should "parse nested comments when applicable" in pending
    it should "not parse nested comments when applicable" in pending
    it should "do nothing with no comments" in pending
    it should "allow for line comments to end in EOF" in pending
    it should "or not allow EOF" in pending

    it should "not aggressively eat everything" in {
        val lexer1 = makeSpace(basicCommentsOnly.copy(commentStart = "", commentEnd = ""))
        val lexer2 = makeSpace(basicCommentsOnly.copy(commentLine = ""))
        val lexer3 = makeSpace(unicodeCommentsOnly)
        (lexer1.skipComments *> char('a')).parse("a") shouldBe a [Success[_]]
        (lexer2.skipComments *> char('a')).parse("a") shouldBe a [Success[_]]
        (lexer3.skipComments *> char('a')).parse("a") shouldBe a [Success[_]]
    }

    // TODO: alter, init

    // TODO: fully
}
