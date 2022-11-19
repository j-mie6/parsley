/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.combinator.{eof, optional}
import parsley.Parsley._
import parsley.implicits.character.{charLift, stringLift}
import parsley.character.{item, digit}
import parsley.errors.combinator.{fail => pfail, unexpected, amend, entrench, ErrorMethods}

import scala.language.implicitConversions

class ErrorTests extends ParsleyTest {
    "mzero parsers" should "always fail" in {
        (Parsley.empty ~> 'a').parse("a") shouldBe a [Failure[_]]
        (pfail("") ~> 'a').parse("a") shouldBe a [Failure[_]]
        (unexpected("x") *> 'a').parse("a") shouldBe a [Failure[_]]
        (('a' ! (_ => "")) *> 'b').parse("ab") shouldBe a [Failure[_]]
        ('a'.unexpected(_ => "x") *> 'b').parse("ab") shouldBe a [Failure[_]]
    }

    "filtering parsers" should "function correctly" in {
        val p = item.filterOut {
            case c if c.isLower => s"'$c' should have been uppercase"
        }
        inside(p.parse("a")) {
            case Failure(TestError((1, 2), VanillaError(unex, exs, rs))) =>
                unex shouldBe empty
                exs shouldBe empty
                rs should contain only ("'a' should have been uppercase")
        }
        p.parse("A") shouldBe Success('A')

        val q = item.guardAgainst {
            case c if c.isLower => Seq(s"'$c' is not uppercase")
        }
        inside(q.parse("a")) { case Failure(TestError((1, 2), SpecialisedError(msgs))) => msgs should contain only ("'a' is not uppercase") }
        q.parse("A") shouldBe Success('A')
    }

    "the collectMsg combinator" should "act like a filter then a map" in {
        val p = item.collectMsg("oops") {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        p.parse("+") shouldBe Success(0)
        p.parse("C") shouldBe Success(3)
        inside(p.parse("a"))  { case Failure(TestError((1, 2), SpecialisedError(msgs))) => msgs should contain only ("oops") }

        val q = item.collectMsg(c => Seq(s"$c is not appropriate")) {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        q.parse("+") shouldBe Success(0)
        q.parse("C") shouldBe Success(3)
        inside(q.parse("a")) { case Failure(TestError((1, 2), SpecialisedError(msgs))) => msgs should contain only ("a is not appropriate") }
    }

    // Issue #70
    "filterOut" should "not corrupt the stack under a handler" in {
        val p = attempt(item.filterOut {
            case c if c.isLower => "no lowercase!"
        })
        p.parse("a") shouldBe a [Failure[_]]
    }

    lazy val r: Parsley[List[String]] = "correct error message" <::> r
    "label" should "affect base error messages" in {
        inside(('a'? "ay!").parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("b"))
                exs should contain only (Named("ay!"))
                rs shouldBe empty
        }
    }
    it should "work across a recursion boundary" in {
        def p = r.label("nothing but this :)")
        inside(p.parse("")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (EndOfInput)
                exs should contain only (Named("nothing but this :)"))
                rs shouldBe empty
        }
        inside(p.parse("correct error message")) {
            case Failure(TestError((1, 22), VanillaError(unex, exs, rs))) =>
                unex should contain (EndOfInput)
                exs should contain only (Raw("correct error message"))
                rs shouldBe empty
        }
    }

    it should "replace everything under the label" in {
        val s = (optional('a') *> optional('b')).label("hi") *> 'c'
        inside(s.parse("e")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Named("hi"), /*Raw("b"),*/ Raw("c"))
                rs shouldBe empty
        }
        val t = (optional('a') *> optional('b').label("bee")).label("hi") *> 'c'
        inside(t.parse("e")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Named("hi"), /*Named("bee"),*/ Raw("c"))
                rs shouldBe empty
        }
        inside(t.parse("ae")) {
            case Failure(TestError((1, 2), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Named("bee"), Raw("c"))
                rs shouldBe empty
        }
    }
    it should "not relabel hidden things" in {
        val s = (optional('a').hide *> optional('b')).label("hi") *> 'c'
        inside(s.parse("e")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Named("hi"), Raw("c"))
                rs shouldBe empty
        }
        inside(s.parse("ae")) {
            case Failure(TestError((1, 2), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Raw("b"), Raw("c"))
                rs shouldBe empty
        }
        val t = (optional('a').hide *> optional('b').label("bee")).label("hi") *> 'c'
        inside(t.parse("e")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Named("hi"), Raw("c"))
                rs shouldBe empty
        }
        inside(t.parse("ae")) {
            case Failure(TestError((1, 2), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("e"))
                exs should contain only (Named("bee"), Raw("c"))
                rs shouldBe empty
        }
    }

    "hide" should "not produce any visible output" in {
        inside('a'.hide.parse("")) {
            case Failure(TestError((1, 1), VanillaError(_, exs, _))) =>
                exs shouldBe empty
        }
        inside("a".hide.parse("")) {
            case Failure(TestError((1, 1), VanillaError(_, exs, _))) =>
                exs shouldBe empty
        }
        inside(digit.hide.parse("")) {
            case Failure(TestError((1, 1), VanillaError(_, exs, _))) =>
                exs shouldBe empty
        }
    }

    "explain" should "provide a message, but only on failure" in {
        inside(Parsley.empty.explain("oops!").parse("")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex shouldBe empty
                exs shouldBe empty
                rs should contain only ("oops!")
        }
        inside('a'.explain("requires an a").parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("b"))
                exs should contain only (Raw("a"))
                rs should contain only ("requires an a")
        }
        inside(('a'.explain("an a") <|> 'b'.explain("a b")).parse("c")){
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("c"))
                exs should contain only (Raw("a"), Raw("b"))
                rs should contain only ("an a", "a b")
        }
        inside(('a'.explain("should be absent") *> 'b').parse("a")) {
            case Failure(TestError((1, 2), VanillaError(unex, exs, rs))) =>
                unex should contain (EndOfInput)
                exs should contain only (Raw("b"))
                rs shouldBe empty
        }
    }
    it should "not have any effect when more input has been consumed since it was added" in {
        inside(('a'.explain("should be absent") <|> ('b' *> digit)).parse("b")) {
            case Failure(TestError((1, 2), VanillaError(unex, exs, rs))) =>
                unex should contain (EndOfInput)
                exs should contain only (Named("digit"))
                rs shouldBe empty
        }
    }

    "fail" should "yield a raw message" in {
        inside(pfail("hi").parse("b")) { case Failure(TestError((1, 1), SpecialisedError(msgs))) => msgs should contain only ("hi") }
    }

    "unexpected" should "yield changes to unexpected messages" in {
        inside(unexpected("bee").parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Named("bee"))
                exs shouldBe empty
                rs shouldBe empty
        }
    }
    it should "produce expected message under influence of ?, along with original message" in {
        inside(('a' <|> unexpected("bee") ? "something less cute").parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Named("bee"))
                exs should contain only (Raw("a"), Named("something less cute"))
                rs shouldBe empty
        }
    }

    "lookAhead" should "produce no hints following it" in {
        val p = 'a' <|> lookAhead(optional(digit) *> 'c') <|> 'b'
        inside(p.parse("d")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("d"))
                exs should contain only (Raw("a"), Raw("c"), Raw("b"), Named("digit"))
                rs shouldBe empty
        }
        val q = 'a' <|> lookAhead(optional(digit)) *> 'c' <|> 'b'
        inside(q.parse("d")){
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("d"))
                exs should contain only (Raw("a"), Raw("b"), Raw("c"))
                rs shouldBe empty
        }
        val r = 'a' <|> lookAhead(digit) *> 'c' <|> 'b'
        inside(r.parse("d")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("d"))
                exs should contain only (Raw("a"), Raw("b"), Named("digit"))
                rs shouldBe empty
        }
    }

    "notFollowedBy" should "produce no hints" in {
        val p = 'a' <|> notFollowedBy(optional(digit)) *> 'c' <|> 'b'
        inside(p.parse("d")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("d"))
                exs should contain only (Raw("a"), Raw("b"))
                rs shouldBe empty
        }
        val q = 'a' <|> notFollowedBy(digit) *> 'c' <|> 'b'
        inside(q.parse("d")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("d"))
                exs should contain only (Raw("a"), Raw("c"), Raw("b"))
                rs shouldBe empty
        }
    }

    "empty" should "produce unknown error messages" in {
        inside(Parsley.empty.parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex shouldBe empty
                exs shouldBe empty
                rs shouldBe empty
        }
    }
    it should "produce no unknown message under influence of ?" in {
        inside((Parsley.empty ? "something, at least").parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex shouldBe empty
                exs should contain only (Named("something, at least"))
                rs shouldBe empty
        }
    }
    it should "not produce an error message at the end of <|> chain" in {
        inside(('a' <|> Parsley.empty).parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("b"))
                exs should contain only (Raw("a"))
                rs shouldBe empty
        }
    }
    it should "produce an expected error under influence of ? in <|> chain" in {
        inside(('a' <|> Parsley.empty ? "something, at least").parse("b")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("b"))
                exs should contain only (Named("something, at least"), Raw("a"))
                rs shouldBe empty
        }
    }

    "eof" should "produce expected end of input" in {
        inside(eof.parse("a")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("a"))
                exs should contain only (EndOfInput)
                rs shouldBe empty
        }
    }
    it should "change message under influence of ?" in {
        inside((eof ? "something more").parse("a")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
                unex should contain (Raw("a"))
                exs should contain only (Named("something more"))
                rs shouldBe empty
        }
    }

    "amend" should "change error messages under it" in {
        val p = 'a' *> amend('b' *> 'c' *> 'd')
        inside(p.parse("ab")) { case Failure(TestError((1, 2), _)) => }
        inside(p.parse("abc")) { case Failure(TestError((1, 2), _)) => }
    }
    it should "not affect input consumption" in {
        (amend('a' *> 'b') <|> 'a').parse("a") shouldBe a [Failure[_]]
    }

    "amend" should "prevent the change of error messages under it" in {
        val p = 'a' *> amend('b' *> entrench('c') *> 'd')
        inside(p.parse("ab")) { case Failure(TestError((1, 3), _)) => }
        inside(p.parse("abc")) { case Failure(TestError((1, 2), _)) => }
        val q = 'a' *> amend('b' *> 'c' *> entrench('d'))
        inside(q.parse("ab")) { case Failure(TestError((1, 2), _)) => }
        inside(q.parse("abc")) { case Failure(TestError((1, 4), _)) => }
    }
    it should "not prevent the action of amend inside it" in {
        val p = 'a' *> amend('b' *> entrench(amend('c' *> 'd' *> entrench('e'))) *> 'f')
        inside(p.parse("ab")) { case Failure(TestError((1, 3), _)) => }
        inside(p.parse("abc")) { case Failure(TestError((1, 3), _)) => }
        inside(p.parse("abcd")) { case Failure(TestError((1, 5), _)) => }
        inside(p.parse("abcde")) { case Failure(TestError((1, 2), _)) => }
    }

    "oneOf" should "incorporate range notation into the error" in {
        inside(character.oneOf('0' to '9').parse("a")) {
            case Failure(TestError(_, VanillaError(_, expecteds, _))) =>
                expecteds should contain only Named("one of \"0\" to \"9\"")
        }
    }

    it should "incorporate sets of characters into the error" in {
        inside(character.oneOf(('0' to '9').toSet).parse("a")) {
            case Failure(TestError(_, VanillaError(_, expecteds, _))) =>
                expecteds should contain only Named("one of \"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", or \"9\"")
        }
    }

    "noneOf" should "incorporate range notation into the error" in {
        inside(character.noneOf('0' to '9').parse("8")) {
            case Failure(TestError(_, VanillaError(_, expecteds, _))) =>
                expecteds should contain only Named("anything outside of \"0\" to \"9\"")
        }
    }

    it should "incorporate sets of characters into the error" in {
        inside(character.noneOf(('0' to '9').toSet).parse("8")) {
            case Failure(TestError(_, VanillaError(_, expecteds, _))) =>
                expecteds should contain only Named("anything except \"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", or \"9\"")
        }
    }

    // Issue 107
    "hints" should "incorporate only with errors at the same offset depth" in {
        val p = attempt('a' ~> digit)
        val parser = optional('b'.label("b")) ~> p.label("foo")
        inside(parser.parse("aa")) {
            case Failure(TestError(_, VanillaError(_, expected, _))) =>
                expected should contain only (Named("digit"))
        }

        val q = amend('a' ~> digit)
        val qarser = optional('b'.label("b")) ~> q.label("foo")
        inside(qarser.parse("aa")) {
            case Failure(TestError(_, VanillaError(_, expected, _))) =>
                expected should contain allOf (Named("foo"), Named("b"))
        }
    }
}
