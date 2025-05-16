/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.Parsley.*
import parsley.character.item
import parsley.errors.combinator.ErrorMethods
import parsley.errors.SpecializedGen

class FilterTests extends ParsleyTest {
    "filtering parsers" should "function correctly" in {
        val p = item.filterOut {
            case c if c.isLower => s"'$c' should have been uppercase"
        }
        inside(p.parse("a")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex shouldBe empty
                exs shouldBe empty
                rs should contain only ("'a' should have been uppercase")
        }
        p.parse("A") shouldBe Success('A')

        val q = item.guardAgainst {
            case c if c.isLower => Seq(s"'$c' is not uppercase")
        }
        inside(q.parse("a")) { case Failure(TestError((1, 1), SpecializedError(msgs, 1))) => msgs should contain only ("'a' is not uppercase") }
        q.parse("A") shouldBe Success('A')

        val r = item.unexpectedWithReasonWhen {
            case c if c.isLower => ("lowercase letter", s"'$c' should have been uppercase")
        }
        inside(r.parse("a")) { case Failure(TestError((1, 1), VanillaError(unex, exs, reasons, 1))) =>
            unex should contain (Named("lowercase letter"))
            exs shouldBe empty
            reasons should contain.only("'a' should have been uppercase")
        }

        val s = item.unexpectedWhen {
            case c if c.isLower => "lowercase letter"
        }
        inside(s.parse("a")) { case Failure(TestError((1, 1), VanillaError(unex, exs, reasons, 1))) =>
            unex should contain (Named("lowercase letter"))
            exs shouldBe empty
            reasons shouldBe empty
        }
    }

    "the collect/mapFilter combinators" should "act like a filter then a map" in {
        val p = item.collectMsg("oops") {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        p.parse("+") shouldBe Success(0)
        p.parse("C") shouldBe Success(3)
        inside(p.parse("a"))  { case Failure(TestError((1, 1), SpecializedError(msgs, 1))) => msgs should contain only ("oops") }

        val q = item.collectMsg(c => Seq(s"$c is not appropriate")) {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        q.parse("+") shouldBe Success(0)
        q.parse("C") shouldBe Success(3)
        inside(q.parse("a")) { case Failure(TestError((1, 1), SpecializedError(msgs, 1))) => msgs should contain only ("a is not appropriate") }

        val errGen = new SpecializedGen[Char] { def messages(c: Char): Seq[String] = Seq(s"$c is not appropriate") }
        val r = item.mapFilterWith(errGen) {
            case '+' => Some(0)
            case c if c.isUpper => Some(c - 'A' + 1)
            case _ => None
        }
        r.parse("+") shouldBe Success(0)
        r.parse("C") shouldBe Success(3)
        inside(r.parse("a")) { case Failure(TestError((1, 1), SpecializedError(msgs, 1))) => msgs should contain only ("a is not appropriate") }
    }

    // Issue #70
    "filterOut" should "not corrupt the stack under a handler" in {
        val p = atomic(item.filterOut {
            case c if c.isLower => "no lowercase!"
        })
        p.parse("a") shouldBe a [Failure[_]]
    }

    // Issue #271
    "partial functions within filters" should "not be evaluated twice" ignore {
        def tripwire[B](r: =>B): () => B = {
            var called = false
            () => {
                require(!called, "tripwire tripped!")
                called = true
                r
            }
        }

        def p1(fail: Boolean) = {
            val t = tripwire(fail)
            item.filterOut {
                case _ if t() => "hello"
            }
        }
        def p2(fail: Boolean) = {
            val t = tripwire(fail)
            item.guardAgainst {
                case _ if t() => Seq("hello")
            }
        }
        def p3(fail: Boolean) = {
            val t = tripwire(fail)
            item.unexpectedWhen {
                case _ if t() => "hello"
            }
        }
        def p4(fail: Boolean) = {
            val t = tripwire(fail)
            item.unexpectedWithReasonWhen {
                case _ if t() => ("hello", "hi")
            }
        }
        //collectMsg
        def p5(fail: Boolean) = {
            val t = tripwire(!fail)
            item.collectMsg(_ => Seq("hello")) {
                case _ if t() => 4
            }
        }
        //mapFilterMsg
        def p6(fail: Boolean) = {
            val t = tripwire(!fail)
            item.mapFilterMsg { x =>
                if (t()) Right(x)
                else Left(Seq("hello"))
            }
        }

        info("filterOut")
        p1(true).parse("a") shouldBe a [Failure[_]]
        p1(false).parse("a") shouldBe a [Success[_]]
        info("guardAgainst")
        p2(true).parse("a") shouldBe a [Failure[_]]
        p2(false).parse("a") shouldBe a [Success[_]]
        info("unexpectedWhen")
        p3(true).parse("a") shouldBe a [Failure[_]]
        p3(false).parse("a") shouldBe a [Success[_]]
        info("unexpectedWithReasonWhen")
        p4(true).parse("a") shouldBe a [Failure[_]]
        p4(false).parse("a") shouldBe a [Success[_]]
        info("collectMsg")
        p5(true).parse("a") shouldBe a [Failure[_]]
        p5(false).parse("a") shouldBe a [Success[_]]
        info("guardAgainst")
        p6(true).parse("a") shouldBe a [Failure[_]]
        p6(false).parse("a") shouldBe a [Success[_]]
    }
}