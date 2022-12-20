/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal

import parsley.{ParsleyTest, Success, Failure, TestError, VanillaError}
import parsley.Parsley, Parsley._
import parsley.character.{char, satisfy, digit, string, stringOfSome}
import parsley.combinator.{attemptChoice, choice, some, optional}
import parsley.expr._
import parsley.implicits.character.charLift
import parsley.errors.combinator.ErrorMethods
import parsley.registers.Reg

import machine.instructions

class InternalTests extends ParsleyTest {
    "subroutines" should "function correctly and be picked up" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = 'a' *> p <* 'b' <* p <* 'c'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 1
        q.internal.instrs.last should be (instructions.Return)
        q.parse("a123b123c") should be (Success('3'))
    }

    they should "function correctly under error messages" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = p.label("err1") *> 'a' *> p.label("err1") <* 'b' <* p.label("err2") <* 'c' <* p.label("err2") <* 'd'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 1
        q.parse("123a123b123c123d") should be (Success('3'))
    }

    they should "not duplicate subroutines when error label is the same" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = 'a' *> p.label("err1") <* 'b' <* p.label("err1") <* 'c'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 1
        q.parse("a123b123c") should be (Success('3'))
    }

    // TODO: While this test works, exposing calls inner instructions is super dodgy.
    // This test was broken when calls were correctly factored out!
    /*they should "function properly when a recursion boundary is inside" in {
        lazy val q: Parsley[Unit] = (p *> p) <|> unit
        lazy val p: Parsley[Unit] = '(' *> q <* ')'
        q.internal.instrs(0).asInstanceOf[instructions.Call].instrs.count(_ == instructions.Return) shouldBe 1
        q.parse("(()())()") shouldBe a [Success[_]]
    }*/

    they should "work in the precedence parser with one op" in {
        val atom = some(digit).map(_.mkString.toInt)
        val expr = precedence[Int](atom)(
            Ops(InfixL)('+' #> (_ + _)))
        expr.internal.instrs.count(_ == instructions.Return) shouldBe 1
    }

    they should "appear frequently inside expression parsers" in {
        val atom = some(digit).map(_.mkString.toInt)
        val expr = precedence[Int](atom)(
            Ops(InfixL)('+' #> (_ + _)),
            Ops(InfixL)('*' #> (_ * _)),
            Ops(InfixL)('%' #> (_ % _)))
        expr.internal.instrs.count(_ == instructions.Return) shouldBe 3
    }

    // Issue 118
    "error alternatives for JumpTable" should "be complete across all branches" in {
        val strs = Seq("hello", "hi", "abc", "good", "g")
        val p = attemptChoice(strs.map(string): _*)
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        val dummy = Reg.make[Unit]
        val q = attemptChoice(strs.map(s => dummy.put(()) *> string(s)): _*)
        assume(q.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 0)
        info("parsing 'h'")
        p.parse("h") shouldBe q.parse("h")
        info("parsing 'b'")
        p.parse("b") shouldBe q.parse("b")
        info("parsing 'a'")
        p.parse("a") shouldBe q.parse("a")
    }
    they should "contain the default in case of no input" in {
        val p = attemptChoice(string("abc"), string("a"), stringOfSome(digit), string("dead"))
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        val dummy = Reg.make[Unit]
        val q = attemptChoice(dummy.put(()) *> string("abc"), dummy.put(()) *> string("a"),
                              dummy.put(()) *> stringOfSome(digit), dummy.put(()) *> string("dead"))
        assume(q.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 0)
        p.parse("") shouldBe q.parse("")
    }
    they should "contain the default for mid-points without backtracking" in {
        val p = choice(string("abc"), string("cee"), stringOfSome(digit), string("dead"))
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        val dummy = Reg.make[Unit]
        val q = choice(dummy.put(()) *> string("abc"), dummy.put(()) *> string("cee"),
                       dummy.put(()) *> stringOfSome(digit), dummy.put(()) *> string("dead"))
        assume(q.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 0)
        info("parsing 'c'")
        p.parse("c") shouldBe q.parse("c")
        info("parsing 'd'")
        p.parse("d") shouldBe q.parse("d")
    }
    they should "be complete when backtracking is disabled" in {
        val strs = Seq("hello", "hi", "abc", "good", "g")
        val p = choice(strs.map(string): _*)
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        val dummy = Reg.make[Unit]
        val q = choice(strs.map(s => dummy.put(()) *> string(s)): _*)
        assume(q.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 0)
        info("parsing 'h'")
        p.parse("h") shouldBe q.parse("h")
        info("parsing 'g'")
        p.parse("g") shouldBe q.parse("g")
        info("parsing 'a'")
        p.parse("a") shouldBe q.parse("a")
    }
    they should "merge properly when more input is consumed in a non-backtracking branch" in {
        val p = choice(string("abc"), char('b') *> stringOfSome(digit), string("cde"))
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        val dummy = Reg.make[Unit]
        val q = choice(dummy.put(()) *> string("abc"), dummy.put(()) *> char('b') *> stringOfSome(digit), dummy.put(()) *> string("cde"))
        assume(q.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 0)
        info("parsing nothing")
        p.parse("") shouldBe q.parse("")
        info("parsing 'a'")
        p.parse("a") shouldBe q.parse("a")
        info("parsing 'b'")
        p.parse("b") shouldBe q.parse("b")
        info("parsing 'c'")
        p.parse("c") shouldBe q.parse("c")
    }

    "JumpTable" must "not try and commute branches" in {
        val p = attempt(string("abc")) <|> string("b") <|> string("abd") <|> string("cbe")
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) >= 1)
        inside(p.parse("abe")) {
            case Failure(TestError(_, VanillaError(_, es, _))) => es.size shouldBe 3
        }
    }

    "tablification" should "not occur for optional" in {
        val p = optional(string("abc"))
        p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) shouldBe 0
    }
}
