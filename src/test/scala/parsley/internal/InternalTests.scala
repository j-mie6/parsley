/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal

import parsley.{ParsleyTest, Success, Failure, TestError, VanillaError, Raw, Named}
import parsley.Parsley, Parsley._
import parsley.character.{char, satisfy, digit, string, stringOfSome}
import parsley.combinator.{attemptChoice, some}
import parsley.expr._
import parsley.implicits.character.charLift
import parsley.errors.combinator.ErrorMethods
import machine.instructions

import scala.language.implicitConversions

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
    "error alternatives for JumpTable" should "be complete across all branches" ignore {
        val fullStrs@(str0 +: str1 +: strs) = Seq("hello", "hi", "abc", "good", "g")
        val p = attemptChoice(fullStrs.map(string): _*)
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        inside(p.parse("h")) {
            case Failure(TestError((1, 1), VanillaError(_, expecteds, _))) =>
                expecteds should contain allOf (Raw(str0), Raw(str1), strs.map(Raw): _*)
        }
        inside(p.parse("b")) {
            case Failure(TestError((1, 1), VanillaError(_, expecteds, _))) =>
                expecteds should contain allOf (Raw(str0), Raw(str1), strs.map(Raw): _*)
        }
        inside(p.parse("a")) {
            case Failure(TestError((1, 1), VanillaError(_, expecteds, _))) =>
                expecteds should contain allOf (Raw(str0), Raw(str1), strs.map(Raw): _*)
        }
    }
    they should "contain the default in case of no input" in {
        val p = attemptChoice(string("abc"), string("a"), string("dead"), stringOfSome(digit))
        assume(p.internal.instrs.count(_.isInstanceOf[instructions.JumpTable]) == 1)
        inside(p.parse("")) {
            case Failure(TestError((1, 1), VanillaError(_, expecteds, _))) =>
                expecteds should contain allOf (Raw("abc"), Raw("a"), Raw("dead"), Named("digit"))
        }
    }
}
