package parsley.expr

import parsley.syntax.character.{charLift, stringLift}
import parsley.character
import parsley.Parsley
import ExprGen._
import parsley.errors.ErrorBuilder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.Success
import parsley.Failure

class ExpressionSemanticPreservationSpec extends AnyFlatSpec with Matchers {
    implicit val eb: ErrorBuilder[String] = ErrorBuilder.stringError
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
        for (_ <- 0 until 500) {
            val (originalExpr, newExpr, opsDefs) = exprPairGen.sample.get
            val successInputs = successInputsGen(opsDefs).sample.get
            for (input <- successInputs) {
                println(opsDefs)
                println(input)
                println(originalExpr.parse(input))
                println(newExpr.parse(input))
                val originalResult = originalExpr.parse(input)
                val newResult = newExpr.parse(input)

                originalResult match {
                    case Success(_) => originalResult shouldBe newResult
                    case Failure(_) => newResult shouldBe a [Failure[_]]
                }
                // originalExpr.parse(input) shouldBe newExpr.parse(input)
            }
        }
    }
}