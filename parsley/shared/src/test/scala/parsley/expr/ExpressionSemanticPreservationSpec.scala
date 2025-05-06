package parsley.expr


import org.scalatest.matchers._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import parsley.syntax.character.{charLift, stringLift}
import parsley.character
import parsley.Parsley

class ExpressionSemanticPreservationSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {
    implicit val config: PropertyCheckConfiguration = new PropertyCheckConfiguration(minSuccessful = 50)

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

    property("successfully parsing basic expressions should not vary based on optimisations") {
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

    property("failing to parse basic expressions should not vary based on optimisations") {
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

    property("parsing expressions with parentheses should not vary based on optimisations") {
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

    property("parsing chained non-associative operators should not vary based on optimisations") {
        val cases: List[String] = List(
            "1==2==3",
            "1==2*3==4+5",
        )

        for (input <- cases) {
            compositeAndShuntAreTheSame(input)
        }
    }
}