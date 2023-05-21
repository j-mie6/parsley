/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.XAssert._

import parsley.internal.errors.{ExpectDesc, ExpectItem, FancyError, ParseError, TrivialError, UnexpectDesc}

// This file contains the defunctionalised forms of the error messages.
// Essentially, whenever an error is created in the machine, it should make use of one of
// these case classes. This means that every error message created will be done in a single
// O(1) allocation, avoiding anything to do with the underlying sets, options etc.

/** This structure represents a collection of operations that build up a
  * `ParseError`. This is done to allow for the operations to be performed
  * lazily, or potentially not at all if the error message is discarded
  * or a merge occurs with a different error kind. It also allows for the
  * conversion to `ParseError` to make use of mutable collections, which
  * would otherwise be unsound.
  */
private [parsley] sealed abstract class DefuncError {
    // Stores the bit-packed flags below, starting from bit 0 upwards
    private [errors] val flags: Byte
    /** Is this error a trivial error, or is it fancy? */
    private [machine] final def isTrivialError: Boolean = (flags & DefuncError.TrivialErrorMask) != 0
    /** Does this error, if its trivial, have any expected items? */
    private [machine] final def isExpectedEmpty: Boolean = (flags & DefuncError.ExpectedEmptyMask) != 0
    /** Is this error protected from amendment via the `amend` combinator? */
    private [machine] final def entrenched: Boolean = (flags & DefuncError.EntrenchedMask) != 0
    /** Is this error created while parsing a lexical token? */
    private [machine] final def lexicalError: Boolean = (flags & DefuncError.LexicalErrorMask) != 0
    /** The offset at which this error occured */
    private [machine] val offset: Int
    /** This function forces the lazy defunctionalised structure into a final `ParseError` value. */
    private [machine] def asParseError(implicit itemBuilder: ErrorItemBuilder): ParseError

    // Operations: these are the smart constructors for the hint operations, which will reduce the number of objects in the binary
    // they all perform some form of simplification step to avoid unnecesary allocations

    /** This operation merges two errors.
      *
      * The errors must have the same offset, with the deeper of the two
      * taking precedence if not. Otherwise, if they are equal offset, errors
      * can only be merged when they are both the same kind: if they are not,
      * fancy errors take precedence over trivial errors.
      *
      * @param err the error to merge with this one
      * @return either the merged error, or one of the two originals
      */
    private [machine] def merge(err: DefuncError): DefuncError
    /** This operation adds the currently available hints as expected
      * items into an error message. This is only applicable for trivial
      * errors.
      *
      * @param hints the hints to add to the message
      * @return the new error with hints
      * @note hints should only be incorporated when they are at the same offset
      */
    private [machine] def withHints(hints: DefuncHints): DefuncError
    /** This operation adds a reason to a trivial error.
      *
      * @param reason the reason to add to the message
      * @return the error with the reason incorporated
      * @note reasons are kept in-order
      */
    private [parsley] def withReason(reason: String): DefuncError
    /** This operation replaces the expected labels in this error message
      * by the given label. This can only happen when the offset of
      * this error message matches the given offset: this should be the
      * offset on entry to the `label` ''combinator'''.
      *
      * If the label given is `""`, then this label is not emitted into
      * the error message, having the effect of removing the underlying
      * labels.
      *
      * @param label the name to replace the expected labels with
      * @param offset the offset that the label is applicable at
      * @return an error message that incorporates the relevant errors
      */
    private [machine] def label(label: String, offset: Int): DefuncError
    /** This operation changes the offset, line, and column number that
      * an error appears to occur at. The effect of this operation is
      * suppressed by `entrench`, however.
      *
      * @param offset the new offset of the error
      * @param line the new line of the error
      * @param col the new column of the error
      * @return a new error that has been amended
      */
    private [machine] def amend(offset: Int, line: Int, col: Int): DefuncError
    /** This operation resists the changes attempted by an `amend` operation
      * ''above'' it, without affecting the messages below.
      *
      * @return an entrenched error message
      */
    private [machine] def entrench: DefuncError
    /** This operation undoes the `amend` protection provided by an underlying entrenched error.
      *
      * @return a non-entrenched error message
      */
    private [machine] def dislodge: DefuncError
    /** This operation sets this error message to be considered as a lexical
      * error message, which means that it will not perform lexical extraction
      * within the builder, instead opting to extract a token via raw input.
      * This only happens if the error occured at an offset ''greater'' than
      * the provided offset (which is the beginning of where the token started
      * parsing).
      *
      * @param offset the offset the error must have occured deeper than
      * @return a lexical error message
      */
    private [machine] def markAsLexical(offset: Int): DefuncError
}
private [errors] object DefuncError {
    private [errors] final val TrivialErrorMask: Byte = 1 << 0
    private [errors] final val ExpectedEmptyMask: Byte = 1 << 1
    private [errors] final val EntrenchedMask: Byte = 1 << 2
    private [errors] final val LexicalErrorMask: Byte = 1 << 3
}

/** Represents partially evaluated trivial errors */
private [errors] sealed abstract class TrivialDefuncError extends DefuncError {
    private [machine] final override def asParseError(implicit itemBuilder: ErrorItemBuilder): TrivialError = {
        val errorBuilder = new TrivialErrorBuilder(offset, !itemBuilder.inRange(offset), lexicalError)
        makeTrivial(errorBuilder)
        errorBuilder.mkError
    }
    private [errors] def makeTrivial(builder: TrivialErrorBuilder): Unit

    /** This method collects up the error labels of this error message into the given `HintState`.
      *
      * This is important for `DefuncHints`, which generates a hints set for previously recovered
      * errors. This is a more efficient way of computing this information as it ignores the other
      * data processed during the formation of a regular error message.
      *
      * @param state the hint state that is collecting up the expected items
      * @note this function should be tail-recursive!
      */
    // TODO: Factor all the duplicated cases out?
    private [errors] final def collectHints(collector: HintCollector): Unit = this match {
        case self: BaseError          =>
            collector ++= self.expectedIterable
            collector.updateWidth(self.unexpectedWidth)
        case self: WithLabel          => if (self.label.nonEmpty) collector += ExpectDesc(self.label)
        case self: WithReason         => self.err.collectHints(collector)
        case self: WithHints          =>
            self.hints.collect(collector)
            self.err.collectHints(collector)
        case self: TrivialMergedErrors =>
            self.err1.collectHints(collector)
            self.err2.collectHints(collector)
        case self: TrivialAmended     => self.err.collectHints(collector)
        case self: TrivialEntrenched  => self.err.collectHints(collector)
        case self: TrivialDislodged   => self.err.collectHints(collector)
        case self: TrivialLexical     => self.err.collectHints(collector)
    }

    private [machine] final override def merge(err: DefuncError): DefuncError = {
        val cmp = Integer.compareUnsigned(this.offset, err.offset)
        if (cmp > 0) this
        else if (cmp < 0) err
        else err match {
            case err: TrivialDefuncError => new TrivialMergedErrors(this, err)
            case err                     => err
        }
    }

    private [machine] final override def withHints(hints: DefuncHints): TrivialDefuncError = {
        if (hints.nonEmpty) new WithHints(this, hints)
        else this
    }
    private [parsley] final override def withReason(reason: String): TrivialDefuncError = new WithReason(this, reason)
    private [machine] final override def label(label: String, offset: Int): TrivialDefuncError = {
        if (this.offset == offset) new WithLabel(this, label)
        else this
    }
    private [machine] final override def amend(offset: Int, line: Int, col: Int): TrivialDefuncError = {
        if (!this.entrenched) new TrivialAmended(offset, line, col, this)
        else this
    }
    private [machine] final override def entrench: TrivialDefuncError = this match {
        case self: TrivialDislodged => new TrivialEntrenched(self.err)
        case self if !self.entrenched => new TrivialEntrenched(this)
        case self => self
    }
    private [machine] final override def dislodge: TrivialDefuncError = this match {
        case self: TrivialEntrenched => self.err
        case self if self.entrenched => new TrivialDislodged(this)
        case self => self
    }
    private [machine] final override def markAsLexical(offset: Int): TrivialDefuncError = {
        if (Integer.compareUnsigned(this.offset, offset) > 0) new TrivialLexical(this)
        else this
    }
}

/** Represents partially evaluated fancy errors */
private [errors] sealed abstract class FancyDefuncError extends DefuncError {
    private [machine] final override def asParseError(implicit itemBuilder: ErrorItemBuilder): FancyError = {
        val builder = new FancyErrorBuilder(offset, lexicalError)
        makeFancy(builder)
        builder.mkError
    }
    private [errors] def makeFancy(builder: FancyErrorBuilder): Unit

    private [machine] final override def merge(err: DefuncError): DefuncError = {
        val cmp = Integer.compareUnsigned(this.offset, err.offset)
        if (cmp > 0) this
        else if (cmp < 0) err
        else err match {
            case err: FancyDefuncError => new FancyMergedErrors(this, err)
            case _                     => this
        }
    }

    private [machine] final override def withHints(hints: DefuncHints): FancyDefuncError = this
    private [parsley] final override def withReason(reason: String): FancyDefuncError = this
    private [machine] final override def label(label: String, offset: Int): FancyDefuncError = this
    private [machine] final override def amend(offset: Int, line: Int, col: Int): FancyDefuncError = {
        if (!this.entrenched) new FancyAmended(offset, line, col, this)
        else this
    }
    private [machine] final override def entrench: FancyDefuncError = this match {
        case self: FancyDislodged => new FancyEntrenched(self.err)
        case self if !self.entrenched => new FancyEntrenched(this)
        case self => self
    }
    private [machine] final override def dislodge: FancyDefuncError = this match {
        case self: FancyEntrenched => self.err
        case self if self.entrenched => new FancyDislodged(this)
        case self => self
    }
    private [machine] final override def markAsLexical(offset: Int): FancyDefuncError = {
        if (Integer.compareUnsigned(this.offset, offset) > 0) new FancyLexical(this)
        else this
    }
}

/** This is the common supertype of all "regular" trivial errors: those that result from failures as opposed to operations on errors. */
private [errors] sealed abstract class BaseError extends TrivialDefuncError {
    /** The line number the error occurred at */
    private [errors] val line: Int
    /** The column number the error occurred at */
    private [errors] val col: Int
    /** The size of the unexpected token demanded by this error */
    private [errors] def unexpectedWidth: Int
    // def expected: IterableOnce[ErrorItem] // TODO: when 2.12 is dropped this will work better
    /** The error items produced by this error */
    private [errors] def expectedIterable: Iterable[ExpectItem]

    /** Adds the reasons and errors (or any other work) after the position and unexpected updates.
      * By default, this will just add the expected messages found in the `expectedIterable`.
      *
      * @note the default can, and should, be overriden for efficiency or to add more information
      */
    private [errors] def addLabelsAndReasons(builder: TrivialErrorBuilder): Unit = builder ++= expectedIterable

    /** Default implementation of `makeTrivial`, which updates the position of the error, and the
      * size of the unexpected token. This calls `addLabelsAndReasons` to complete any work.
      *
      * @note the default implementation should be overriden for EmptyErrors, which must not update the unexpected!
      */
    private [errors] override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        builder.pos_=(line, col)
        builder.updateUnexpected(unexpectedWidth)
        addLabelsAndReasons(builder)
    }
}

private [machine] final class ClassicExpectedError(val offset: Int, val line: Int, val col: Int, val expected: Option[ExpectItem], val unexpectedWidth: Int)
    extends BaseError {
    override final val flags = if (expected.isEmpty) (DefuncError.ExpectedEmptyMask | DefuncError.TrivialErrorMask).toByte else DefuncError.TrivialErrorMask
    override def expectedIterable: Iterable[ExpectItem] = expected
}
private [machine] final class ClassicExpectedErrorWithReason(val offset: Int, val line: Int, val col: Int,
                                                             val expected: Option[ExpectItem], val reason: String, val unexpectedWidth: Int) extends BaseError {
    override final val flags = if (expected.isEmpty) (DefuncError.ExpectedEmptyMask | DefuncError.TrivialErrorMask).toByte else DefuncError.TrivialErrorMask
    override def expectedIterable: Iterable[ExpectItem] = expected
    override def addLabelsAndReasons(builder: TrivialErrorBuilder): Unit = {
        builder += expected
        builder += reason
    }
}
private [parsley] final class ClassicUnexpectedError(val offset: Int, val line: Int, val col: Int, val expected: Option[ExpectItem],
                                                     val unexpected: UnexpectDesc) extends BaseError {
    override final val flags = if (expected.isEmpty) (DefuncError.ExpectedEmptyMask | DefuncError.TrivialErrorMask).toByte else DefuncError.TrivialErrorMask
    override def expectedIterable: Iterable[ExpectItem] = expected
    override private [errors] def unexpectedWidth: Int = unexpected.width
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        builder.pos_=(line, col)
        builder += expected
        builder.updateUnexpected(unexpected)
    }
}
private [parsley] final class ClassicFancyError(val offset: Int, val line: Int, val col: Int, caretWidth: Int, val msgs: String*) extends FancyDefuncError {
    override final val flags = DefuncError.ExpectedEmptyMask
    override def makeFancy(builder: FancyErrorBuilder): Unit = {
        builder.pos_=(line, col)
        builder ++= msgs
        builder.updateCaretWidth(caretWidth)
    }
}
private [parsley] final class EmptyError(val offset: Int, val line: Int, val col: Int, val unexpectedWidth: Int) extends BaseError {
    override final val flags = (DefuncError.ExpectedEmptyMask | DefuncError.TrivialErrorMask).toByte
    override def expectedIterable: Iterable[ExpectItem] = None
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        builder.pos_=(line, col)
        builder.updateEmptyUnexpected(unexpectedWidth)
    }
}
private [parsley] final class EmptyErrorWithReason(val offset: Int, val line: Int, val col: Int, val reason: String, val unexpectedWidth: Int)
    extends BaseError {
    override final val flags: Byte = (DefuncError.ExpectedEmptyMask | DefuncError.TrivialErrorMask).toByte
    override def expectedIterable: Iterable[ExpectItem] = None
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        builder.pos_=(line, col)
        builder.updateEmptyUnexpected(unexpectedWidth)
        builder += reason
    }
}
private [machine] final class MultiExpectedError(val offset: Int, val line: Int, val col: Int, val expected: Set[ExpectItem], val unexpectedWidth: Int)
    extends BaseError {
    override final val flags = if (expected.isEmpty) (DefuncError.ExpectedEmptyMask | DefuncError.TrivialErrorMask).toByte else DefuncError.TrivialErrorMask
    override def expectedIterable: Iterable[ExpectItem] = expected
}

private [errors] final class TrivialMergedErrors private [errors] (val err1: TrivialDefuncError, val err2: TrivialDefuncError) extends TrivialDefuncError {
    override final val flags = (err1.flags & err2.flags).toByte
    assume(err1.offset == err2.offset, "two errors only merge when they have matching offsets")
    val offset = err1.offset //Math.max(err1.offset, err2.offset)
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        err1.makeTrivial(builder)
        err2.makeTrivial(builder)
    }
}

private [errors] final class FancyMergedErrors private [errors] (val err1: FancyDefuncError, val err2: FancyDefuncError) extends FancyDefuncError {
    override final val flags = (err1.flags & err2.flags).toByte
    assume(err1.offset == err2.offset, "two errors only merge when they have matching offsets")
    override val offset = err1.offset //Math.max(err1.offset, err2.offset)
    override def makeFancy(builder: FancyErrorBuilder): Unit = {
        err1.makeFancy(builder)
        err2.makeFancy(builder)
    }
}

private [errors] final class WithHints private [errors] (val err: TrivialDefuncError, val hints: DefuncHints) extends TrivialDefuncError {
    assume(!hints.isEmpty, "WithHints will always have non-empty hints")
    override final val flags = (err.flags & ~DefuncError.ExpectedEmptyMask).toByte //err.isExpectedEmpty && hints.isEmpty
    override val offset = err.offset
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        err.makeTrivial(builder)
        builder.whenAcceptingExpected {
            for (size <- hints.updateExpectedsAndGetSize(builder)) builder.updateUnexpected(size)
        }
    }
}

private [errors] final class WithReason private [errors] (val err: TrivialDefuncError, val reason: String) extends TrivialDefuncError {
    override final val flags = err.flags
    val offset = err.offset
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        err.makeTrivial(builder)
        builder += reason
    }
}

private [errors] final class WithLabel private [errors] (val err: TrivialDefuncError, val label: String) extends TrivialDefuncError {
    override final val flags = {
        if (label.isEmpty) (err.flags |  DefuncError.ExpectedEmptyMask).toByte
        else               (err.flags & ~DefuncError.ExpectedEmptyMask).toByte
    }
    val offset = err.offset
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        builder.ignoreExpected {
            err.makeTrivial(builder)
        }
        if (label.nonEmpty) builder += ExpectDesc(label)
    }
}

private [errors] final class TrivialAmended private [errors] (val offset: Int, val line: Int, val col: Int, val err: TrivialDefuncError)
    extends TrivialDefuncError {
    assume(!err.entrenched, "an amendment will only occur on unentrenched errors")
    override final val flags = err.flags
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = {
        err.makeTrivial(builder)
        // this should happen after the sub-error has been proceeded
        builder.pos_=(line, col)
    }
}

private [errors] final class FancyAmended private [errors] (val offset: Int, val line: Int, val col: Int, val err: FancyDefuncError) extends FancyDefuncError {
    assume(!err.entrenched, "an amendment will only occur on unentrenched errors")
    override final val flags = err.flags
    override def makeFancy(builder: FancyErrorBuilder): Unit = {
        err.makeFancy(builder)
        builder.pos_=(line, col)
    }
}

private [errors] final class TrivialEntrenched private [errors] (val err: TrivialDefuncError) extends TrivialDefuncError {
    override final val flags = (err.flags | DefuncError.EntrenchedMask).toByte
    override val offset = err.offset
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = err.makeTrivial(builder)
}

private [errors] final class TrivialDislodged private [errors] (val err: TrivialDefuncError) extends TrivialDefuncError {
    override final val flags = (err.flags & ~DefuncError.EntrenchedMask).toByte
    override val offset = err.offset
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = err.makeTrivial(builder)
}

private [errors] final class FancyEntrenched private [errors] (val err: FancyDefuncError) extends FancyDefuncError {
    override final val flags = (err.flags | DefuncError.EntrenchedMask).toByte
    override val offset = err.offset
    override def makeFancy(builder: FancyErrorBuilder): Unit = err.makeFancy(builder)
}

private [errors] final class FancyDislodged private [errors] (val err: FancyDefuncError) extends FancyDefuncError {
    override final val flags = (err.flags & ~DefuncError.EntrenchedMask).toByte
    override val offset = err.offset
    override def makeFancy(builder: FancyErrorBuilder): Unit = err.makeFancy(builder)
}

private [errors] final class TrivialLexical private [errors] (val err: TrivialDefuncError) extends TrivialDefuncError {
    override final val flags = (err.flags | DefuncError.LexicalErrorMask).toByte
    override val offset = err.offset
    override def makeTrivial(builder: TrivialErrorBuilder): Unit = err.makeTrivial(builder)
}

private [errors] final class FancyLexical private [errors] (val err: FancyDefuncError) extends FancyDefuncError {
    override final val flags = (err.flags | DefuncError.LexicalErrorMask).toByte
    override val offset = err.offset
    override def makeFancy(builder: FancyErrorBuilder): Unit = err.makeFancy(builder)
}
