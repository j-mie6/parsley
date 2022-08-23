/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import parsley.errors.{DefaultErrorBuilder, ErrorBuilder, tokenextractors}
import org.scalatest.Inside

case class TestError(pos: (Int, Int), lines: TestErrorLines)

sealed trait TestErrorLines
case class VanillaError(unexpected: Option[TestErrorItem], expecteds: Set[TestErrorItem], reasons: Set[String]) extends TestErrorLines
case class SpecialisedError(msgs: Set[String]) extends TestErrorLines

sealed trait TestErrorItem
case class Raw(item: String) extends TestErrorItem
case class Named(item: String) extends TestErrorItem
case object EndOfInput extends TestErrorItem

class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): TestError = TestError(pos, lines)

    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Unit
    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = TestErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
        VanillaError(unexpected, expected, reasons)
    }
    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs)

    type ExpectedItems = Set[Item]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    type Messages = Set[Message]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    type UnexpectedLine = Option[Item]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
    type ExpectedLine = ExpectedItems
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = Unit
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): Unit = ()

    override val numLinesBefore: Int = 2
    override val numLinesAfter: Int = 2

    type Item = TestErrorItem
    type Raw = parsley.Raw
    type Named = parsley.Named
    type EndOfInput = parsley.EndOfInput.type
    override def raw(item: String): Raw = Raw(item)
    override def named(item: String): Named = Named(item)
    override val endOfInput: EndOfInput = EndOfInput
}

abstract class ParsleyTest extends AnyFlatSpec with Matchers with Assertions with Inside {
    val trivialError = Symbol("trivialError")
    val expectedEmpty = Symbol("expectedEmpty")

    implicit val eb: ErrorBuilder[TestError] = new TestErrorBuilder
}
