/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.syntax.character.{charLift, stringLift}
import parsley.character
import parsley.Parsley
import ExprGen._
import parsley.Success
import parsley.Failure
import parsley.ParsleyTest

class ExpressionSemanticPreservationSpec extends ParsleyTest {
    val originalExpr: Parsley[Int] = originalPrecedence(
        OriginalOps[Int](InfixN)("==" #> ((a, b) => if (a == b) 1 else 0)) +:
        OriginalOps[Int](InfixL)('+' #> (_ + _), '-' #> (_ - _)) +:
        OriginalOps[Int](InfixL)('*' #> (_ * _)) +:
        OriginalOps[Int](Prefix)('-' #> (-_)) +:
        OriginalAtoms[Int](character.digit.map(_.asDigit), '(' ~> originalExpr <~ ')')
    )

    val newExpr: Parsley[Int] = precedence[Int](
        Ops[Int](InfixN)("==" #> ((a, b) => if (a == b) 1 else 0)) +:
        Ops[Int](InfixL)('+' #> (_ + _), '-' #> (_ - _)) +:
        Ops[Int](InfixL)('*' #> (_ * _)) +:
        Ops[Int](Prefix)('-' #> (-_)) +:
        Atoms[Int](character.digit.map(_.asDigit), '(' ~> newExpr <~ ')')
    )

    def compositeAndShuntAreTheSame[A](input: String) = {
        newExpr.parse(input) shouldBe originalExpr.parse(input)
    }

    "the new precedence implementation" should "successfully parse basic expressions and not vary based on optimisations" in {
        val cases: List[String] = List(
            "1+1",
            "1*1",
            "1+2*3",
            "1+-2"
        )

        for (input <- cases) {
            compositeAndShuntAreTheSame(input)
        }
    }

    it should "fail to parse basic expressions and not vary based on optimisations" in {
        val cases: List[String] = List(
            "1+",
            "1*",
            "1+2*",
            "1+2*3+",
            "1+2*3*",
            "",
            "1++2",
            "+1"
        )

        for (input <- cases) {
            compositeAndShuntAreTheSame(input)
        }
    }

    it should "parse expressions with parentheses and not vary based on optimisations" in {
        val cases: List[String] = List(
            "(1+1)",
            "(1*1)",
            "(1+2*3)",
            "(1+-2)",
            "((1+1))",
            "((1*1))",
            "((1+2*3))",
            "((1+-2))"
        )

        for (input <- cases) {
            compositeAndShuntAreTheSame(input)
        }
    }

    it should "parse chained non-associative operators and not vary based on optimisations" in {
        val cases: List[String] = List(
            "1==2==3",
            "1==2*3==4+5",
        )

        for (input <- cases) {
            compositeAndShuntAreTheSame(input)
        }
    }

    it should "parse random expressions and not vary based on optimisations" in {
        // val opsDefsMedium = List(
        //     OpsDef(InfixL, List(("+", Nil), ("-", Nil))),
        //     OpsDef(InfixL, List(("*", Nil), ("/", Nil), ("%", Nil))),
        //     OpsDef(InfixN, List(("==", Nil), ("!=", Nil))),
        //     OpsDef(Prefix, List(("-", Nil), ("len", Nil), ("ord", Nil)))
        // )

        // val opsDefsLarge = List(
        //     OpsDef(InfixR, List(("|", Nil))),
        //     OpsDef(InfixR, List(("&&", Nil))),
        //     OpsDef(InfixN, List(("==", Nil), ("!=", Nil))),
        //     OpsDef(InfixN, List(("<", Nil), (">", Nil), ("<=", Nil), (">=", Nil))),
        //     OpsDef(InfixL, List(("+", Nil), ("-", Nil))),
        //     OpsDef(InfixL, List(("*", Nil), ("/", Nil), ("%", Nil))),
        //     OpsDef(Prefix, List(("!", Nil), ("-", Nil), ("len", Nil), ("ord", Nil), ("chr", Nil)))
        // )

        // val inputsMedium = inputsGen(opsDefsMedium, 1300, failureRate = 0).sample.get
        // println(inputsMedium)

        // val inputsLarge = inputsGen(opsDefsLarge, 1400, failureRate = 0).sample.get
        // println(inputsLarge)

        for (_ <- 0 until 5000) {
            val (originalExpr, newExpr, opsDefs) = exprPairGen.sample.get
            val inputs = inputsGen(opsDefs, 500).sample.get
            for (input <- inputs) {
                val originalResult = originalExpr.parse(input)
                val newResult = newExpr.parse(input)

                originalResult match {
                    case Success(_) => originalResult shouldBe newResult
                    case Failure(_) => inside (newResult) {
                        case f: Failure[_] =>
                            f.pos shouldBe originalResult.pos
                    }
                }
            }
        }
    }
}