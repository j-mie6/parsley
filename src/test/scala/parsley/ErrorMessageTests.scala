package parsley

import parsley.Combinator.eof
import parsley.Parsley._
import parsley.{Failure, Parsley, runParser}
import parsley.Implicits.{charLift, stringLift}

class ErrorMessageTests extends ParsleyTest
{
    //TODO Bind tests
    lazy val r: Parsley[List[String]] = "correct error message" <::> (r </> Nil)
    "?" should "affect base error messages" in
    {
        runParser('a' ? "ay!", "b") should be (Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected ay!"))
    }
    it should "work across a recursion boundary" in
    {
        runParser(r ? "nothing but this :)", "") should be
        {
            Failure("(line 1, column 1):\n  unexpected end of input\n  expected nothing but this :)")
        }
        runParser(r ? "nothing but this :)", "correct error messagec") should be
        {
            Failure("(line 1, column 23):\n  unexpected end of input\n  expected nothing but this :)")
        }
    }

    "fail" should "yield a raw message" in
    {
        runParser(Parsley.fail("hi"), "b") should be
        {
            Failure("(line 1, column 1):\n  hi")
        }
    }
    it should "produce an expected message under influence of ?, along with original message" in
    {
        runParser('a' <|> (Parsley.fail("oops") ? "hi"), "b") should be
        {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\" or hi\n  oops")
        }
    }

    "unexpected" should "yield changes to unexpected messages" in
    {
        runParser(unexpected("bee"), "b")
    }
    it should "produce expected message under influence of ?, along with original message" in
    {
        runParser('a' <|> unexpected("bee") ? "something less cute", "b")
    }

    "empty" should "produce unknown error messages" in
    {
        runParser(Parsley.empty, "b") should be
        {
            Failure("(line 1, column 1):\n  unknown parse error")
        }
    }
    it should "produce no unknown message under influence of ?" in
    {
        runParser(Parsley.empty ? "something, at least", "b") should be
        {
            Failure("(line 1, column 1):\n  expected something, at least")
        }
    }
    it should "not produce an error message at the end of <|> chain" in
    {
        runParser('a' <|> Parsley.empty, "b") should be
        {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\"")
        }
    }
    it should "produce an expected error under influence of ? in <|> chain" in
    {
        runParser('a' <|> Parsley.empty ? "something, at least", "b") should be
        {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\" or something, at least")
        }
    }

    "eof" should "produce unexpected end of input" in
    {
        runParser(eof, "a") should be
        {
            Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected end of input")
        }
    }
    it should "change message under influence of ?" in
    {
        runParser(eof ? "something more", "a") should be
        {
            Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected something more")
        }
    }
}
