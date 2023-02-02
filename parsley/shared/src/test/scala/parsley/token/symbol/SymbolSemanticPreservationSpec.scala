package parsley.token.symbol

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import parsley.token.LexemeImpl._

import parsley.Parsley
import parsley.token.descriptions._
import parsley.token.errors.ErrorConfig
import parsley.token.predicate._, implicits.Basic.charToBasic
import parsley.token.symbol._
import parsley.character.spaces
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class SymbolSemanticPreservationSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {
    val errConfig = new ErrorConfig
    def makeOptSymbol(nameDesc: NameDesc, symDesc: SymbolDesc): Symbol = new LexemeSymbol(new ConcreteSymbol(nameDesc, symDesc, errConfig), spaces, errConfig)
    def makeUnoptSymbol(nameDesc: NameDesc, symDesc: SymbolDesc): Symbol = new LexemeSymbol(new OriginalSymbol(nameDesc, symDesc, errConfig), spaces, errConfig)

    // Random testing time, we're going to create random valid configurations, and then test the
    // parsers generated with random input. Doesn't matter what they do, so long as they say the
    // same things. It is probably a good idea to provide some non-random input cases too.

    val identifierLetterGen = Gen.oneOf[CharPredicate](
        Basic(_.isLetter),
        Basic(_.isLetterOrDigit),
        NotRequired,
        Unicode(Character.isLetter(_)),
        Unicode(Character.isLetterOrDigit(_)),
        '$'
    )
    val opCharGen = Gen.nonEmptyContainerOf[Set, Char](Gen.oneOf('+', '*', '/', 'a'))
    val opUniGen = Gen.nonEmptyContainerOf[Set, Int](Gen.oneOf(0x1F642, 0x1F643, '£', '$'))
    val operatorLetterGen = Gen.frequency(
        3 -> opCharGen.map(Basic),
        3 -> Gen.oneOf(opCharGen.map(_.map(_.toInt)), opUniGen).map(Unicode),
        1 -> Gen.const(NotRequired)
    )
    val keywordGen = Gen.oneOf("if", "foo", "while", "key", "a", "bar7")
    val keywordsGen = Gen.containerOf[Set, String](keywordGen)
    val operatorGen = Gen.oneOf("+", "*", "**", "***", "::", "*:*", "ba")
    val operatorsGen = Gen.containerOf[Set, String](operatorGen)

    val nameDescGen = for {
        identifierLetter <- identifierLetterGen
        operatorLetter <- operatorLetterGen
    } yield NameDesc.plain.copy(identifierLetter = identifierLetter, operatorLetter = operatorLetter)

    val symbolDescGen = for {
        ks <- keywordsGen
        ops <- operatorsGen
        caseSensitive <- Arbitrary.arbitrary[Boolean]
    } yield SymbolDesc(ks, ops, caseSensitive)

    implicit val config = new PropertyCheckConfiguration(minSuccessful = 50)

    def optAndUnOptAreSame(f: (Symbol, String) => Parsley[Unit])(nameDesc: NameDesc, symbolDesc: SymbolDesc, sym: String, input: String) =
        whenever (!input.contains('\u0000')) {
            val opt = makeOptSymbol(nameDesc, symbolDesc)
            val unopt = makeUnoptSymbol(nameDesc, symbolDesc)
            f(opt, sym).parse(input) shouldBe f(unopt, sym).parse(input)
        }

    property("reading keywords should not vary based on optimisations") {
        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               keywordGen -> "keyword", Arbitrary.arbitrary[String] -> "input")(optAndUnOptAreSame(_.softKeyword(_)))

        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               keywordGen -> "keyword", keywordGen -> "input")(optAndUnOptAreSame(_.softKeyword(_)))

        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               keywordGen -> "keyword", Gen.alphaNumChar -> "letter") { (nameDesc, symbolDesc, key, letter) =>
            optAndUnOptAreSame(_.softKeyword(_))(nameDesc, symbolDesc, key, s"$key$letter")
        }
    }

    property("reading operators should not vary based on optimisations") {
        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               operatorGen -> "operator", Arbitrary.arbString.arbitrary -> "input")(optAndUnOptAreSame(_.softOperator(_)))

        forAll(nameDescGen -> "name description", symbolDescGen -> "symbol description",
               operatorGen -> "operator", operatorGen -> "input")(optAndUnOptAreSame(_.softOperator(_)))
    }
}
