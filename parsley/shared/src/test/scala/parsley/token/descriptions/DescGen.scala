package parsley.token.descriptions

import parsley.token.predicate._
import parsley.token.predicate.implicits.Basic.charToBasic

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import parsley.token.descriptions.text._

object DescGen {
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

    val codepointGen = Gen.choose(0, java.lang.Character.MAX_CODE_POINT)

    val literalGen = Gen.containerOf[Set, Char](Gen.oneOf('\'', '\"', '\\'))
    val singleGen = Gen.mapOf(Gen.zip(Gen.alphaChar, codepointGen))
    val multiGen = Gen.mapOf(Gen.zip(Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString), codepointGen))

    //TODO: Expand with more configuration fields as more of the escape is optimised
    val escDescGen = for {
        literals <- literalGen
        singles <- singleGen
        multis <- multiGen
        if !singles.keys.exists(c => multis.contains(s"$c"))
    } yield EscapeDesc.plain.copy(literals = literals, singleMap = singles, multiMap = multis)
}
