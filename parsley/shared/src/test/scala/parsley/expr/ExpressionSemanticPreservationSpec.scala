package parsley.expr

import parsley.syntax.character.{charLift, stringLift}
import parsley.character
import parsley.Parsley
import ExprGen._
import parsley.Success
import parsley.Failure
import parsley.ParsleyTest
import parsley.VanillaError
import parsley.SpecializedError

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
        for (_ <- 0 until 1000) {
            val (originalExpr, newExpr, opsDefs) = exprPairGen.sample.get
            val inputs = inputsGen(opsDefs, 500).sample.get
            for (input <- inputs) {
                val originalResult = originalExpr.parse(input)
                val newResult = newExpr.parse(input)

                originalResult match {
                    case Success(_) => originalResult shouldBe newResult
                    case Failure(_) => newResult shouldBe a [Failure[_]]
                }

                // originalResult match {
                //     case Success(_) => originalResult shouldBe newResult
                //     case Failure(originalError) => newResult match {
                //         case Failure(newError) => {
                //             originalError.pos shouldBe newError.pos
                //             originalError.lines match {
                //                 case VanillaError(unexpected, expecteds, reasons, width) => newError.lines match {
                //                     case VanillaError(newUnexpected, newExpecteds, newReasons, newWidth) => {
                //                         if (unexpected != newUnexpected) {
                //                             println(opsDefs)
                //                             println(input)
                //                             println(originalResult)
                //                             println(newResult)
                //                         }
                //                         unexpected shouldBe newUnexpected
                //                         // expecteds shouldBe newExpecteds
                //                         reasons shouldBe newReasons
                //                         width shouldBe newWidth
                //                     }
                //                     case _ => fail("Expected VanillaError but got something else")
                //                 }
                //                 case SpecializedError(msgs, width) => newError.lines match {
                //                     case SpecializedError(newMsgs, newWidth) => {
                //                         msgs shouldBe newMsgs
                //                         width shouldBe newWidth
                //                     }
                //                     case _ => fail("Expected SpecializedError but got something else")
                //                 }
                //             }
                //         }
                //         case Success(_) => fail("Expected failure but got success")
                //     }
                // }
            }
        }
    }
}