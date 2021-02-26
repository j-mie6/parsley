package parsley

import parsley.combinator.{eof, optional}
import parsley.Parsley._
import parsley.implicits.{charLift, stringLift}
import parsley.character.{anyChar, digit}
import parsley.unsafe.ErrorLabel

import scala.language.implicitConversions

class ErrorMessageTests extends ParsleyTest {
    lazy val r: Parsley[List[String]] = "correct error message" <::> r
    "label" should "affect base error messages" in {
        ('a' ? "ay!").runParser("b") should be (Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected ay!\n  >b\n  >^"))
    }
    it should "work across a recursion boundary" in {
        val p = r.unsafeLabel("nothing but this :)")
        (r.unsafeLabel("nothing but this :)")).runParser("") should be {
            Failure("(line 1, column 1):\n  unexpected end of input\n  expected nothing but this :)\n  >\n  >^")
        }
        (r.unsafeLabel("nothing but this :)")).runParser("correct error message") should be {
            Failure("(line 1, column 22):\n  unexpected end of input\n  expected nothing but this :)\n  >correct error message\n  >                     ^")
        }
    }
    it should "replace the first instance" in {
        val s = (optional('a') *> optional('b')).label("hi") *> 'c'
        s.runParser("e") should be {
            Failure("(line 1, column 1):\n  unexpected \"e\"\n  expected \"b\", \"c\", or hi\n  >e\n  >^")
        }
        val t = (optional('a') *> optional('b').label("bee")).label("hi") *> 'c'
        t.runParser("e") should be {
            Failure("(line 1, column 1):\n  unexpected \"e\"\n  expected \"c\", bee, or hi\n  >e\n  >^")
        }
        t.runParser("ae") should be {
            Failure("(line 1, column 2):\n  unexpected \"e\"\n  expected \"c\" or bee\n  >ae\n  > ^")
        }
    }
    it should "not relabel hidden things" in {
        val s = (optional('a').hide *> optional('b')).label("hi") *> 'c'
        s.runParser("e") should be {
            Failure("(line 1, column 1):\n  unexpected \"e\"\n  expected \"c\" or hi\n  >e\n  >^")
        }
        s.runParser("ae") should be {
            Failure("(line 1, column 2):\n  unexpected \"e\"\n  expected \"b\" or \"c\"\n  >ae\n  > ^")
        }
        val t = (optional('a').hide *> optional('b').label("bee")).label("hi") *> 'c'
        t.runParser("e") should be {
            Failure("(line 1, column 1):\n  unexpected \"e\"\n  expected \"c\" or hi\n  >e\n  >^")
        }
        t.runParser("ae") should be {
            Failure("(line 1, column 2):\n  unexpected \"e\"\n  expected \"c\" or bee\n  >ae\n  > ^")
        }
    }

    "explain" should "provide a message, but only on failure" in {
        Parsley.empty.explain("oops!").runParser("") shouldBe Failure("(line 1, column 1):\n  oops!\n  >\n  >^")
        'a'.explain("requires an a").runParser("b") shouldBe Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\"\n  requires an a\n  >b\n  >^")
        ('a'.explain("an a") <|> 'b'.explain("a b")).runParser("c") shouldBe {
            Failure("(line 1, column 1):\n  unexpected \"c\"\n  expected \"a\" or \"b\"\n  an a\n  a b\n  >c\n  >^")
        }
        ('a'.explain("should be absent") *> 'b').runParser("a") shouldBe {
            Failure("(line 1, column 2):\n  unexpected end of input\n  expected \"b\"\n  >a\n  > ^")
        }
    }
    it should "not have any effect when more input has been consumed since it was added" in {
        ('a'.explain("should be absent") <|> ('b' *> digit)).runParser("b") shouldBe {
            Failure("(line 1, column 2):\n  unexpected end of input\n  expected digit\n  >b\n  > ^")
        }
    }

    "fail" should "yield a raw message" in {
        Parsley.fail("hi").runParser("b") should be {
            Failure("(line 1, column 1):\n  hi\n  >b\n  >^")
        }
    }
    // Not anymore it doesn't!
    /*it should "produce an expected message under influence of ?, along with original message" in {
        ('a' <|> (Parsley.fail("oops") ? "hi")).runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\" or hi\n  oops\n\n    b\n    ^")
        }
    }*/

    "unexpected" should "yield changes to unexpected messages" in {
        unexpected("bee").runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected bee\n  >b\n  >^")
        }
    }
    it should "produce expected message under influence of ?, along with original message" in {
        ('a' <|> unexpected("bee") ? "something less cute").runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected bee\n  expected \"a\" or something less cute\n  >b\n  >^")
        }
    }

    "empty" should "produce unknown error messages" in {
        Parsley.empty.runParser("b") should be {
            Failure("(line 1, column 1):\n  unknown parse error\n  >b\n  >^")
        }
    }
    it should "produce no unknown message under influence of ?" in {
        (Parsley.empty ? "something, at least").runParser("b") should be {
            Failure("(line 1, column 1):\n  expected something, at least\n  >b\n  >^")
        }
    }
    it should "not produce an error message at the end of <|> chain" in {
        ('a' <|> Parsley.empty).runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\"\n  >b\n  >^")
        }
    }
    it should "produce an expected error under influence of ? in <|> chain" in {
        ('a' <|> Parsley.empty ? "something, at least").runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\" or something, at least\n  >b\n  >^")
        }
    }

    "eof" should "produce expected end of input" in {
        eof.runParser("a") should be {
            Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected end of input\n  >a\n  >^")
        }
    }
    it should "change message under influence of ?" in {
        (eof ? "something more").runParser("a") should be {
            Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected something more\n  >a\n  >^")
        }
    }

    /*"error position" should "be correctly reset in" in {
        val p = attempt('a' *> digit) <|> Parsley.fail("hello :)")
        p.runParser("aa") should be {
            Failure("(line 1, column 1):\n  unexpected end of input\n  expected any character\n  hello :)")
        }
        p.runParser("c") should be {
            Failure("")
        }
    }*/
}
