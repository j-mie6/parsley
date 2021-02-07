package parsley

import parsley.combinator.eof
import parsley.Parsley._
import parsley.implicits.{charLift, stringLift}
import parsley.character.{anyChar, digit}

import scala.language.implicitConversions

class ErrorMessageTests extends ParsleyTest {
    //TODO: Bind tests
    lazy val r: Parsley[List[String]] = "correct error message" <::> (r </> Nil)
    "label" should "affect base error messages" in {
        'a'.label("ay!").runParser("b") should be (Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected ay!"))
    }
    it should "work across a recursion boundary" in {
        (r ? "nothing but this :)").runParser("") should be {
            Failure("(line 1, column 1):\n  unexpected end of input\n  expected nothing but this :)")
        }
        (r ? "nothing but this :)").runParser("correct error messagec") should be {
            Failure("(line 1, column 23):\n  unexpected end of input\n  expected nothing but this :)")
        }
    }

    "fail" should "yield a raw message" in {
        Parsley.fail("hi").runParser("b") should be {
            Failure("(line 1, column 1):\n  hi")
        }
    }
    it should "produce an expected message under influence of ?, along with original message" in {
        ('a' <|> (Parsley.fail("oops") ? "hi")).runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\" or hi\n  oops")
        }
    }

    "unexpected" should "yield changes to unexpected messages" in {
        unexpected("bee").runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected bee")
        }
    }
    it should "produce expected message under influence of ?, along with original message" in {
        ('a' <|> unexpected("bee") ? "something less cute").runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected bee\n  expected \"a\" or something less cute")
        }
    }

    "empty" should "produce unknown error messages" in {
        Parsley.empty.runParser("b") should be {
            Failure("(line 1, column 1):\n  unknown parse error")
        }
    }
    it should "produce no unknown message under influence of ?" in {
        (Parsley.empty ? "something, at least").runParser("b") should be {
            Failure("(line 1, column 1):\n  expected something, at least")
        }
    }
    it should "not produce an error message at the end of <|> chain" in {
        ('a' <|> Parsley.empty).runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\"")
        }
    }
    it should "produce an expected error under influence of ? in <|> chain" in {
        //println(internal.instructions.pretty(('a' <|> Parsley.empty ? "something, at least").internal.instrs))
        ('a' <|> Parsley.empty ? "something, at least").runParser("b") should be {
            Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\" or something, at least")
        }
    }

    "eof" should "produce expected end of input" in {
        eof.runParser("a") should be {
            Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected end of input")
        }
    }
    it should "change message under influence of ?" in {
        (eof ? "something more").runParser("a") should be {
            Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected something more")
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
