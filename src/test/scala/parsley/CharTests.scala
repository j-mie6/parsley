import parsley.Char._
import parsley.{Failure, runParser}
import parsley.Parsley._
import parsley.Implicits.{charLift, stringLift}

class CharTests extends ParsleyTest
{
    "string" should "consume succeed if it is found at head" in
    {
        runParser("abc", "abc") should not be a [Failure]
    }
    it should "not consume input if it fails on first character" in
    {
        runParser("abc" <|> 'b', "b") should not be a [Failure]
    }
    it should "consume input if it fails mid-string" in
    {
        runParser("abc" <|> "ab", "ab") shouldBe a [Failure]
    }
    it should "not consume input if it fails mid-string when combined with attempt" in
    {
        runParser(attempt("abc") <|> "ab", "ab") should not be a [Failure]
    }

    "anyChar" should "accept any character" in
    {
        for (i <- 0 to 65535) runParser(anyChar, i.toChar.toString) should not be a [Failure]
    }
    it should "fail if the input has run out, expecting any character" in
    {
        runParser(anyChar, "") should be (Failure("(line 1, column 1):\n  unexpected end of input\n  expected any character"))
    }

    "space" should "consume ' ' or '\t'" in
    {
        runParser(space, " ") should not be a [Failure]
        runParser(space, "\t") should not be a [Failure]
    }
    it should "expect space/tab otherwise" in
    {
        for (i <- 0 to 65535; if i != ' ' && i != '\t') runParser(space, i.toChar.toString) should be
        {
            Failure("(line 1, column 1):\n  unexpected \"" + i.toChar + "\"\n  expected space/tab")
        }
    }

    "spaces" should "consume lots of spaces" in
    {
        runParser(spaces *> 'a', " \t" * 100 + 'a') should not be a [Failure]
    }
    it should "never fail" in
    {
        runParser(spaces *> 'a', "a") should not be a [Failure]
    }

    "whitespace" should "consume any whitespace chars" in
    {
        runParser(whitespaces *> 'a', " \t\n\r\f\u000b" * 100 + 'a') should not be a [Failure]
    }
    it should "fail otherwise" in
    {
        val cs = " \t\n\r\f\u000b".toSet
        for (i <- 0 to 65535; if !cs.contains(i.toChar))
        {
            runParser(whitespace, i.toChar.toString) shouldBe a [Failure]
        }
    }

    "endOfLine" should "consume windows or unix line endings" in
    {
        runParser(endOfLine, "\n") should not be a [Failure]
        runParser(endOfLine, "\r\n") should not be a [Failure]
    }
    it should "fail otherwise" in
    {
        for (i <- 0 to 65535; if i != 10) runParser(endOfLine, i.toChar.toString) shouldBe a [Failure]
    }

    "upper" should "only accept uppercase characters" in
    {
        for (c <- 'A' to 'Z') runParser(upper, c.toString) should not be a [Failure]
    }
    it should "fail otherwise" in
    {
        for (c <- 'a' to 'z') runParser(upper, c.toString) shouldBe a [Failure]
    }

    "lower" should "only accept lowercase characters" in
    {
        for (c <- 'a' to 'z') runParser(lower, c.toString) should not be a [Failure]
    }
    it should "fail otherwise" in
    {
        for (c <- 'A' to 'Z') runParser(lower, c.toString) shouldBe a [Failure]
    }

    "digit parsers" should "accept the appropriate characters" in
    {
        for (c <- '0' to '9')
        {
            runParser(digit, c.toString) should not be a [Failure]
            runParser(hexDigit, c.toString) should not be a [Failure]
            if (c < '8') runParser(octDigit, c.toString) should not be a [Failure]
        }
        for (c <- 'a' to 'f') runParser(hexDigit, c.toString) should not be a [Failure]
        for (c <- 'A' to 'F') runParser(hexDigit, c.toString) should not be a [Failure]
    }
    they should "fail otherwise" in
    {
        for (c <- 'a' to 'f')
        {
            runParser(digit, c.toString) shouldBe a [Failure]
            runParser(octDigit, c.toString) shouldBe a [Failure]
        }
        for (c <- 'A' to 'F')
        {
            runParser(digit, c.toString) shouldBe a [Failure]
            runParser(octDigit, c.toString) shouldBe a [Failure]
        }
        runParser(octDigit, "8") shouldBe a [Failure]
        runParser(octDigit, "9") shouldBe a [Failure]
    }
}
