package parsley.internal.machine.errors

import parsley.internal.errors.{TrivialError, FailError, ErrorItem, EndOfInput}

import scala.collection.mutable

import TrivialState._

private [errors] final class TrivialState(offset: Int, outOfRange: Boolean) {
    private var line: Int = _
    private var col: Int = _
    private val expecteds = mutable.Set.empty[ErrorItem]
    private var unexpected: UnexpectItem = NoItem
    private val reasons = mutable.Set.empty[String]
    private var acceptingExpected = true

    def pos_=(line: Int, col: Int): Unit = {
        this.line = line
        this.col = col
    }
    def updateUnexpected(size: Int): Unit = if (outOfRange) updateUnexpected(Other(EndOfInput)) else updateUnexpected(Raw(size))
    def updateUnexpected(item: ErrorItem): Unit = updateUnexpected(Other(item))
    private def updateUnexpected(other: UnexpectItem): Unit = this.unexpected = unexpected.pickHigher(other)
    def +=(expected: Option[ErrorItem]): Unit = if (acceptingExpected) (for (e <- expected) this.expecteds += e)
    def +=(expected: ErrorItem): Unit = if (acceptingExpected) this.expecteds += expected
    def ++=(expecteds: Iterable[ErrorItem]): Unit = if (acceptingExpected) this.expecteds ++= expecteds
    def +=(reason: String): Unit = this.reasons += reason
    def mkError(implicit builder: ErrorItemBuilder): TrivialError = {
        new TrivialError(offset, line, col, unexpected.toErrorItem(offset), expecteds.toSet, reasons.toSet)
    }
    def ignoreExpected(action: =>Unit): Unit = {
        acceptingExpected = false
        action
        acceptingExpected = true
    }
}
object TrivialState {
    private [TrivialState] sealed trait UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = (this, other) match {
            case (Other(u1), Other(u2)) => Other(ErrorItem.higherPriority(u1, u2))
            case (higher: Other, _) => higher
            case (_, higher: Other) => higher
            case (u, NoItem) => u
            case (u1: Raw, u2: Raw) if u1.size > u2.size => u1
            case (_, u: Raw) => u
        }
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem]
    }
    private [TrivialState] case class Raw(size: Int) extends UnexpectItem {
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder) = Some(builder(offset, size))
    }
    private [TrivialState] case class Other(underlying: ErrorItem) extends UnexpectItem {
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder) = Some(underlying)
    }
    private [TrivialState] case object NoItem extends UnexpectItem {
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder) = None
    }
}

private [errors] final class FancyState(offset: Int) {
    private var line: Int = _
    private var col: Int = _
    private val msgs = mutable.Set.empty[String]

    def pos_=(line: Int, col: Int): Unit = {
        this.line = line
        this.col = col
    }

    def +=(msg: String): Unit = this.msgs += msg
    def mkError: FailError = {
        new FailError(offset, line, col, msgs.toSet)
    }
}