/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import scala.collection.mutable

import parsley.internal.errors.{EndOfInput, ErrorItem, FancyError, TrivialError}

import TrivialErrorBuilder.{NoItem, Other, Raw, UnexpectItem}

/** When building a true `TrivialError` from a `TrivialDefuncError`, this state is used to keep track of the
  * components that will make up the final error. This allows for more efficient construction
  *
  * @param offset
  * @param outOfRange
  */
private [errors] final class TrivialErrorBuilder(offset: Int, outOfRange: Boolean) {
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
    def updateUnexpected(size: Int): Unit = if (outOfRange) updateUnexpected(new Other(EndOfInput)) else updateUnexpected(new Raw(size))
    def updateUnexpected(item: ErrorItem): Unit = updateUnexpected(new Other(item))
    private def updateUnexpected(other: UnexpectItem): Unit = this.unexpected = unexpected.pickHigher(other)
    def +=(expected: Option[ErrorItem]): Unit = this ++= expected
    def +=(expected: ErrorItem): Unit = if (acceptingExpected) this.expecteds += expected
    def ++=(expecteds: Iterable[ErrorItem]): Unit = if (acceptingExpected) this.expecteds ++= expecteds
    def +=(reason: String): Unit = this.reasons += reason
    def mkError(implicit builder: ErrorItemBuilder): TrivialError = {
        new TrivialError(offset, line, col, unexpected.toErrorItem(offset), expecteds.toSet, reasons.toSet)
    }
    def whenAcceptingExpected(action: =>Unit): Unit = if (acceptingExpected) action
    def ignoreExpected(action: =>Unit): Unit = {
        _acceptingExpected += 1
        action
        _acceptingExpected -= 1
    }

    def makeHintCollector: HintCollector = new HintCollector(expecteds)
}
private [errors] object TrivialErrorBuilder {
    private [TrivialErrorBuilder] sealed abstract class UnexpectItem {
        def pickHigher(other: UnexpectItem): UnexpectItem
        private [TrivialErrorBuilder] def pickRaw(other: Raw): UnexpectItem
        private [TrivialErrorBuilder] def pickOther(other: Other): UnexpectItem
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem]
    }
    private [TrivialErrorBuilder] final class Raw(val size: Int) extends UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = other.pickRaw(this)
        private [TrivialErrorBuilder] final override def pickRaw(other: Raw) = if (this.size > other.size) this else other
        private [TrivialErrorBuilder] final override def pickOther(other: Other) = other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = Some(builder(offset, size))
    }
    private [TrivialErrorBuilder] final class Other(val underlying: ErrorItem) extends UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = other.pickOther(this)
        private [TrivialErrorBuilder] final override def pickRaw(other: Raw) = this
        private [TrivialErrorBuilder] final override def pickOther(other: Other) = if (this.underlying.higherPriority(other.underlying)) this else other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = Some(underlying)
    }
    private [TrivialErrorBuilder] object NoItem extends UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = other
        private [TrivialErrorBuilder] final override def pickRaw(other: Raw) = other
        private [TrivialErrorBuilder] final override def pickOther(other: Other) = other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = None
    }
}

private [errors] final class FancyErrorBuilder(offset: Int) {
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

private [errors] final class HintCollector(hints: mutable.Set[ErrorItem]) {
    def this() = this(mutable.Set.empty)

    private var _unexpectSize = 0

    def +=(hint: ErrorItem): Unit = this.hints += hint
    def ++=(hints: Iterable[ErrorItem]): Unit = this.hints ++= hints

    def unexpectSize: Int = _unexpectSize
    def updateSize(sz: Int): Unit = _unexpectSize = Math.max(_unexpectSize, sz)

    def mkSet: Set[ErrorItem] = this.hints.toSet
}
