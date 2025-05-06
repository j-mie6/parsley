package parsley.expr


import org.scalatest.matchers._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import parsley.syntax.character.charLift
import parsley.character

class ExpressionSemanticPreservationSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {
    implicit val config: PropertyCheckConfiguration = new PropertyCheckConfiguration(minSuccessful = 50)

    def compositeAndShuntAreTheSame[A](originalPrec: OriginalPrec[A], newPrec: Prec[A])(input: String) = {
        val original = originalPrecedence(originalPrec)
        val shunt = precedence(newPrec)
        shunt.parse(input) shouldBe original.parse(input)
    }

    property("successfully parsing basic expressions should not vary based on optimisations") {
        val originalPrec = OriginalOps[Int](InfixL)('+' #> (_ + _)) +:
            OriginalOps[Int](InfixL)('*' #> (_ * _)) +:
            OriginalAtoms[Int](character.digit.map(_.asDigit))
        val prec = Ops[Int](InfixL)('+' #> (_ + _)) +:
            Ops[Int](InfixL)('*' #> (_ * _)) +:
            Atoms[Int](character.digit.map(_.asDigit))
        
        val cases: List[String] = List(
            "1+1",
            "1*1",
            "1+2*3",
        )

        for (input <- cases) {
            compositeAndShuntAreTheSame(originalPrec, prec)(input)
        }
    }
}