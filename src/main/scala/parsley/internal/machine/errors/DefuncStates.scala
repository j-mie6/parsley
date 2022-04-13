package parsley.internal.machine.errors

import scala.collection.mutable

import parsley.internal.errors.{EndOfInput, ErrorItem, FancyError, TrivialError}

import TrivialState.{NoItem, Other, Raw, UnexpectItem}

private [errors] final class TrivialState(offset: Int, outOfRange: Boolean) {
    private var line: Int = _
    private var col: Int = _
    private val expecteds = mutable.Set.empty[ErrorItem]
    private var unexpected: UnexpectItem = NoItem
    private val reasons = mutable.Set.empty[String]
    private var _acceptingExpected = 0
    private def acceptingExpected = _acceptingExpected == 0

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
        _acceptingExpected += 1
        action
        _acceptingExpected -= 1
    }
}
private [errors] object TrivialState {
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
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = Some(builder(offset, size))
    }
    private [TrivialState] case class Other(underlying: ErrorItem) extends UnexpectItem {
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = Some(underlying)
    }
    private [TrivialState] case object NoItem extends UnexpectItem {
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = None
    }
}

private [errors] final class FancyState(offset: Int) {
    private var line: Int = _
    private var col: Int = _
    private val msgs = mutable.ListBuffer.empty[String]

    def pos_=(line: Int, col: Int): Unit = {
        this.line = line
        this.col = col
    }

    def ++=(msg: Seq[String]): Unit = this.msgs ++= msg
    def mkError: FancyError = {
        new FancyError(offset, line, col, msgs.toList.distinct)
    }
}

private [errors] final class HintState {
    private val hints = mutable.Set.empty[ErrorItem]
    private var _unexpectSize = 0

    def +=(hint: ErrorItem): Unit = this.hints += hint
    def ++=(hints: Set[ErrorItem]): Unit = this.hints ++= hints

    def unexpectSize: Int = _unexpectSize
    def updateSize(sz: Int): Unit = _unexpectSize = Math.max(_unexpectSize, sz)

    def mkSet: Set[ErrorItem] = this.hints.toSet
}
