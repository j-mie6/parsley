/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal

import parsley.{ParsleyTest, Success}
import parsley.Parsley, Parsley._
import parsley.character.{char, satisfy, digit}
import parsley.combinator.some
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
}
