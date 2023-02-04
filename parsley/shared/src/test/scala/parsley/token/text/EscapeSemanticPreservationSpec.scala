package parsley.token.text

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import parsley.combinator.eof
import parsley.token.descriptions.text._
import parsley.token.errors.ErrorConfig
import parsley.token.text._
import org.scalacheck.Gen
import org.scalacheck.Shrink

class EscapeSemanticPreservationSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {
    implicit val config: PropertyCheckConfiguration = new PropertyCheckConfiguration(minSuccessful = 50)
    val errConfig = new ErrorConfig
    val generic = new parsley.token.numeric.Generic(errConfig)
    def makeOptEscape(escDesc: EscapeDesc) = new Escape(escDesc, errConfig, generic)
    def makeUnoptEscape(escDesc: EscapeDesc) = new OriginalEscape(escDesc, errConfig, generic)

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

    implicit val escDescShrink: Shrink[EscapeDesc] = Shrink {
        case desc@EscapeDesc(_, literals, singles, multis, _, _, _, _, _, _) =>
            val shrinkLiterals = for (ls <- Shrink.shrink(literals)) yield desc.copy(literals = ls)
            val shrinkSingles = Shrink.shrink(singles).collect {
                case ss if ss.forall(kv => Character.isValidCodePoint(kv._2)) => desc.copy(singleMap = ss)
            }
            val shrinkMultis = Shrink.shrink(multis).collect {
                case ms if ms.forall(kv => Character.isValidCodePoint(kv._2)) &&
                           !singles.keys.exists(c => ms.contains(s"$c")) => desc.copy(multiMap = ms.filter(_._1.nonEmpty))
            }
            shrinkMultis ++ shrinkSingles ++ shrinkLiterals
    }

    val escInputGen = Gen.alphaNumStr.map(s => s"\\$s")

    property("reading escape characters should not vary based on optimisations") {
        forAll(escDescGen -> "escDesc", escInputGen -> "input") { (escDesc, input) =>
            val optEscape = makeOptEscape(escDesc)
            val unoptEscape = makeUnoptEscape(escDesc)
            optEscape.escapeChar.parse(input) shouldBe unoptEscape.escapeChar.parse(input)
            (optEscape.escapeChar <* eof).parse(input) shouldBe (unoptEscape.escapeChar <* eof).parse(input)
        }

        forAll(escDescGen -> "escDesc", Gen.asciiPrintableStr -> "input") { (escDesc, input) =>
            val optEscape = makeOptEscape(escDesc)
            val unoptEscape = makeUnoptEscape(escDesc)
            optEscape.escapeChar.parse(input) shouldBe unoptEscape.escapeChar.parse(input)
        }
    }
}
