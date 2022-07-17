/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import scala.collection.mutable

import parsley.internal.errors.{EndOfInput, ErrorItem, FancyError, TrivialError}

import TrivialErrorBuilder.{NoItem, Other, Raw, UnexpectItem}

/** When building a true `TrivialError` from a `TrivialDefuncError`, this builder is used to keep track of the
  * components that will make up the final error. This allows for more efficient construction of the object,
  * as it can internally use mutable collections, before finally freezing them into an immutable form.
  *
  * @param offset the offset that the error being built occured at
  * @param outOfRange whether or not this error occured at the end of the input or not
  */
private [errors] final class TrivialErrorBuilder(offset: Int, outOfRange: Boolean) {
    private var line: Int = _
    private var col: Int = _
    private val expecteds = mutable.Set.empty[ErrorItem]
    private var unexpected: UnexpectItem = NoItem
    private val reasons = mutable.Set.empty[String]
    private var _acceptingExpected = 0
    private def acceptingExpected = _acceptingExpected == 0

    /** Updates the position of the error message.
      *
      * @param line the new line number
      * @param col the new column number
      */
    def pos_=(line: Int, col: Int): Unit = {
        this.line = line
        this.col = col
    }
    /** Updates the width of the raw unexpected token generated by the error: this may expand its width,
      * but will not shrink it. If a raw unexpected token is not going to be generated, this has no
      * effect.
      *
      * If this error is `outOfRange`, `EndOfInput` is generated instead.
      *
      * @param width
      */
    def updateUnexpected(width: Int): Unit = if (outOfRange) updateUnexpected(new Other(EndOfInput)) else updateUnexpected(new Raw(width))
    /** Updates the unexpected token generated by the error, so long as this new item takes precedence
      * over the old one.
      *
      * @param item
      */
    def updateUnexpected(item: ErrorItem): Unit = updateUnexpected(new Other(item))
    private def updateUnexpected(other: UnexpectItem): Unit = this.unexpected = unexpected.pickHigher(other)
    /** If this error is accepting new expected items, adds the given item into the error, should one exist.
      *
      * @param expected the possible item to add
      */
    def +=(expected: Option[ErrorItem]): Unit = this ++= expected
    /** If this error is accepting new expected items, adds the given item into the error
      *
      * @param expected the item to add
      */
    def +=(expected: ErrorItem): Unit = if (acceptingExpected) this.expecteds += expected
    /** If this error is accepting new expected items, adds all the given items into the error
      *
      * @param expecteds the items to add
      */
    def ++=(expecteds: Iterable[ErrorItem]): Unit = if (acceptingExpected) this.expecteds ++= expecteds
    /** Adds the given reason onto the end of the errors reasons.
      *
      * @param reason the reason to add
      */
    def +=(reason: String): Unit = this.reasons += reason
    /** Builds the final error within the context of some builder for error items.
      *
      * @param itemBuilder a builder for constructing unexpected error items from the raw input of the parse
      * @return the final error message
      */
    def mkError(implicit itemBuilder: ErrorItemBuilder): TrivialError = {
        new TrivialError(offset, line, col, unexpected.toErrorItem(offset), expecteds.toSet, reasons.toSet)
    }
    /** Performs some given action only when this builder is currently accepting new expected items
      *
      * @param action the action to perform
      */
    def whenAcceptingExpected(action: =>Unit): Unit = if (acceptingExpected) action
    /** For the duration of the given action, all expected items that are provided to the builder
      * will be ignored.
      *
      * @param action the action to perform, during which no expected items can be accepted
      */
    def ignoreExpected(action: =>Unit): Unit = {
        _acceptingExpected += 1
        action
        _acceptingExpected -= 1
    }

    /** Makes a `HintCollector` for collecting any hints from a `DefuncHints`
      * directly into this builder's expected error items.
      */
    def makeHintCollector: HintCollector = new HintCollector(expecteds)
}
private [errors] object TrivialErrorBuilder {
    private [TrivialErrorBuilder] sealed abstract class UnexpectItem {
        def pickHigher(other: UnexpectItem): UnexpectItem
        protected [TrivialErrorBuilder] def pickRaw(other: Raw): UnexpectItem
        protected [TrivialErrorBuilder] def pickOther(other: Other): Other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem]
    }
    private [TrivialErrorBuilder] final class Raw(val size: Int) extends UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = other.pickRaw(this)
        final override def pickRaw(other: Raw): Raw = if (this.size > other.size) this else other
        final override def pickOther(other: Other): Other = other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = Some(builder(offset, size))
    }
    private [TrivialErrorBuilder] final class Other(val underlying: ErrorItem) extends UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = other.pickOther(this)
        final override def pickRaw(other: Raw): Other = this
        final override def pickOther(other: Other): Other = if (this.underlying.higherPriority(other.underlying)) this else other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = Some(underlying)
    }
    private [TrivialErrorBuilder] object NoItem extends UnexpectItem {
        final def pickHigher(other: UnexpectItem): UnexpectItem = other
        final override def pickRaw(other: Raw): Raw = other
        final override def pickOther(other: Other): Other = other
        def toErrorItem(offset: Int)(implicit builder: ErrorItemBuilder): Option[ErrorItem] = None
    }
}

/** When building a true `FancyError` from a `FancyDefuncError`, this builder is used to keep track of the
  * components that will make up the final error. This allows for more efficient construction of the object,
  * as it can internally use mutable collections, before finally freezing them into an immutable form.
  *
  * @param offset the offset that the error being built occured at
  */
private [errors] final class FancyErrorBuilder(offset: Int) {
    private var line: Int = _
    private var col: Int = _
    private val msgs = mutable.ListBuffer.empty[String]

    /** Updates the position of the error message.
      *
      * @param line the new line number
      * @param col the new column number
      */
    def pos_=(line: Int, col: Int): Unit = {
        this.line = line
        this.col = col
    }

    /** Adds a collection of new error message lines into this error.
      *
      * @param msgs the messages to add
      */
    def ++=(msgs: Seq[String]): Unit = this.msgs ++= msgs
    /** Builds the final error. */
    def mkError: FancyError = {
        new FancyError(offset, line, col, msgs.toList.distinct)
    }
}

/** This collects up hints generated by the ghosts of old error messages,
  * which are usually piped directly into a builder's expected messages.
  *
  * @param hints the place to collect the items into
  */
private [errors] final class HintCollector(hints: mutable.Set[ErrorItem]) {
    /** Creates a new collector that starts empty */
    def this() = this(mutable.Set.empty)

    private var width = 0

    /** Adds a new hint into the collector. */
    def +=(hint: ErrorItem): Unit = this.hints += hint
    /** Adds several hints into the collector. */
    def ++=(hints: Iterable[ErrorItem]): Unit = this.hints ++= hints

    /** Gets the width of the unexpected token that may have accompanied the original hints */
    def unexpectWidth: Int = width
    /** Updates the width of the unexpected token that may have accompanied these hints: this
      * can only get wider.
      */
    def updateWidth(sz: Int): Unit = width = Math.max(width, sz)

    /** Generates an immutable snapshot of this collector */
    def mkSet: Set[ErrorItem] = this.hints.toSet
}