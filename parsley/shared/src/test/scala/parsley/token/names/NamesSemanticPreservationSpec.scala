package parsley.token.names

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import parsley.Parsley
import parsley.token.descriptions._
import parsley.token.errors.ErrorConfig
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import parsley.token.LexemeImpl._
import parsley.character.spaces

import parsley.token.descriptions.DescGen
import DescGen._
//import DescShrink._

class NamesSemanticPreservationSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {
    implicit val config: PropertyCheckConfiguration = new PropertyCheckConfiguration(minSuccessful = 50)
    val errConfig = new ErrorConfig
    def makeOptNames(nameDesc: NameDesc, symDesc: SymbolDesc): Names = new LexemeNames(new ConcreteNames(nameDesc, symDesc, errConfig), spaces)
    def makeUnoptNames(nameDesc: NameDesc, symDesc: SymbolDesc): Names = new LexemeNames(new OriginalNames(nameDesc, symDesc, errConfig), spaces)

    def optAndUnOptAreSame(f: Names => Parsley[String])(nameDesc: NameDesc, symbolDesc: SymbolDesc, input: String) =
        whenever (!input.contains('\u0000')) {
            val opt = makeOptNames(nameDesc, symbolDesc)
            val unopt = makeUnoptNames(nameDesc, symbolDesc)
            f(opt).parse(input) shouldBe f(unopt).parse(input)
        }

    property("reading identifiers should not vary based on optimisations") {
        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               Arbitrary.arbitrary[String] -> "input")(optAndUnOptAreSame(_.identifier))

        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               keywordGen -> "input")(optAndUnOptAreSame(_.identifier))

        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               keywordGen -> "keyword", Gen.alphaNumChar -> "letter") { (nameDesc, symbolDesc, key, letter) =>
            optAndUnOptAreSame(_.identifier)(nameDesc, symbolDesc, s"$key$letter")
        }
    }

    property("reading user-defined operators should not vary based on optimisations") {
        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               Arbitrary.arbitrary[String] -> "input")(optAndUnOptAreSame(_.identifier))

        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               operatorGen -> "input")(optAndUnOptAreSame(_.userDefinedOperator))
    }
}
