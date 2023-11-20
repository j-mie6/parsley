/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.scalaccompat.annotation.unused
import parsley.Parsley
import parsley.Parsley._ // scalastyle:ignore underscore.import
import parsley.character._ // scalastyle:ignore underscore.import
import parsley.combinator._ // scalastyle:ignore underscore.import
import parsley.debugger.combinator.{attachDebugger, named}
import parsley.debugger.internal.DebugContext
import parsley.expr._ // scalastyle:ignore underscore.import
import parsley.internal.deepembedding.frontend.debugger.Debugged

class DebuggerUsageSpec extends AnyFlatSpec with Matchers {
    import DebuggerUsageSpec.Arithmetic

    behavior of "the Debugged internal frontend class"

    it should "not allow nesting of Debugged nodes" in {
        val ctx = new DebugContext()
        try {
            val _ = new Debugged(
                new Debugged(fresh(()).internal, None, None)(ctx),
                None,
                None
            )(ctx)
            fail("Debugged nodes have been nested")
        } catch {
            case _: Throwable => info("assertion exception thrown, as expected")
        }
    }

    it should "preserve the prettified names of the parsers" in {
        val ctx = new DebugContext()
        new Debugged(named(fresh(()), "foo").internal, None, None)(ctx).prettyName shouldBe "foo"
        new Debugged(fresh(()).internal, None, None)(ctx).prettyName shouldBe "fresh"
        new Debugged(fresh(()).internal, None, Some("bar"))(ctx).prettyName shouldBe "bar"
    }

    behavior of "the debugger runtime"

    it should "preserve the result of parsers" in {
        val debugMath = attachDebugger(Arithmetic.prog)

        debugMath._2.parse("1+1").get.head shouldBe 2
        debugMath._2.parse("2*3").get.head shouldBe 6
        debugMath._2.parse("9-4").get.head shouldBe 5
        debugMath._2.parse("6/2").get.head shouldBe 3

        debugMath._2.parse("1+2+3+4+5\n2*3*4").get shouldBe List(15, 24) // scalastyle:ignore magic.number
    }

    it should "preserve the side-effecting behaviour of parsers" in {
        var hit1 = false
        var hit2 = false
        var hit3 = false

        val parser = fresh { hit1 = true }.impure *> fresh { hit2 = true }.impure <* fresh { hit3 = true }
        val debugged = attachDebugger(parser)

        val _ = debugged._2.parse(""): @unused

        assert(hit1 && hit2 && hit3)
    }

    it should "factor out inputs of child parsers" in {
        val parser = many(string("abc"))
        val (treeF, debugged) = attachDebugger(parser)

        val _ = debugged.parse("abcabc"): @unused
        val tree = treeF()

        tree.parseResults match {
            case Some(result) => result.rawInput shouldBe "{1}{2}"
            case None         => fail("No result recorded.")
        }
    }
}

object DebuggerUsageSpec {
    private [parsley] object Arithmetic {
        val int: Parsley[BigInt] =
            satisfy(_.isDigit)
              .foldLeft1(BigInt(0))((acc, c) => acc * 10 + c.asDigit)

        lazy val expr: Parsley[BigInt] =
            precedence[BigInt](
                int,
                char('(') ~> expr <~ char(')')
            )(
                Ops(InfixL)(
                    char('*') #> (_ * _),
                    char('/') #> (_ / _)
                ),
                Ops(InfixL)(
                    char('+') #> (_ + _),
                    char('-') #> (_ - _)
                )
            )

        lazy val prog: Parsley[List[BigInt]] =
            many(many(satisfy("\r\n".contains(_))) ~> expr)
    }
}
