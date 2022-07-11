/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.XAssert._

import parsley.internal.errors.{Desc, ErrorItem, FancyError, ParseError, TrivialError}

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
private [machine] sealed abstract class DefuncError {
    /** Is this error a trivial error, or is it fancy? */
    private [machine] def isTrivialError: Boolean = this.isInstanceOf[TrivialDefuncError]
    /** Does this error, if its trivial, have any expected items? */
    private [machine] def isExpectedEmpty: Boolean
    /** Is this error protected from amendment via the `amend` combinator? */
    private [machine] def entrenched: Boolean = false
    /** The offset at which this error occured */
    private [machine] val offset: Int
    /** This function forced the lazy defunctionalised structure into a final `ParseError` value */
    private [machine] def asParseError(implicit builder: ErrorItemBuilder): ParseError

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
    private [machine] def withReason(reason: String): DefuncError
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
}

/** Represents partially evaluated trivial errors */
private [errors] sealed abstract class TrivialDefuncError extends DefuncError {
    private [machine] final override def asParseError(implicit builder: ErrorItemBuilder): TrivialError = {
        val state = new TrivialState(offset, !builder.inRange(offset))
        makeTrivial(state)
        state.mkError
    }
    private [errors] def makeTrivial(state: TrivialState): Unit

    /** This method collects up the error labels of this error message into the given `HintState`.
      *
      * This is important for `DefuncHints`, which generates a hints set for previously recovered
      * errors. This is a more efficient way of computing this information as it ignores the other
      * data processed during the formation of a regular error message.
      *
      * @param state the hint state that is collecting up the expected items
      * @note this function should be tail-recursive!
      */
    private [errors] final def collectHints(state: HintState): Unit = this match {
        case self: BaseError          =>
            state ++= self.expectedIterable
            state.updateSize(self.unexpectedWidth)
        case self: WithLabel          => if (self.label.nonEmpty) state += Desc(self.label)
        case self: WithReason         => self.err.collectHints(state)
        case self: WithHints          =>
            self.hints.collect(0, state)
            self.err.collectHints(state)
        case self: TrivialMergedErrors =>
            self.err1.collectHints(state)
            self.err2.collectHints(state)
        case self: TrivialAmended     => self.err.collectHints(state)
        case self: TrivialEntrenched  => self.err.collectHints(state)
    }

    private [machine] final override def merge(err: DefuncError): DefuncError = {
        val thisOffset = this.offset
        val otherOffset = err.offset
        if (thisOffset > otherOffset) this
        else if (otherOffset > thisOffset) err
        else err match {
            case err: TrivialDefuncError => new TrivialMergedErrors(this, err)
            case err                     => err
        }
    }

    private [machine] final override def withHints(hints: DefuncHints): TrivialDefuncError = {
        if (hints.nonEmpty) new WithHints(this, hints)
        else this
    }
    private [machine] final override def withReason(reason: String): TrivialDefuncError = new WithReason(this, reason)
    private [machine] final override def label(label: String, offset: Int): TrivialDefuncError = {
        if (this.offset == offset) new WithLabel(this, label)
        else this
    }
    private [machine] final override def amend(offset: Int, line: Int, col: Int): TrivialDefuncError = {
        if (!this.entrenched) new TrivialAmended(offset, line, col, this)
        else this
    }
    private [machine] final override def entrench: TrivialDefuncError = {
        if (!this.entrenched) new TrivialEntrenched(this)
        else this
    }
}

/** Represents partially evaluated fancy errors */
private [errors] sealed abstract class FancyDefuncError extends DefuncError {
    // fancy errors don't have expected messages
    private [machine] final override def isExpectedEmpty = true
    private [machine] final override def asParseError(implicit builder: ErrorItemBuilder): FancyError = {
        val state = new FancyState(offset)
        makeFancy(state)
        state.mkError
    }
    private [errors] def makeFancy(state: FancyState): Unit

    private [machine] final override def merge(err: DefuncError): DefuncError = {
        val thisOffset = this.offset
        val otherOffset = err.offset
        if (thisOffset > otherOffset) this
        else if (otherOffset > thisOffset) err
        else err match {
            case err: FancyDefuncError => new FancyMergedErrors(this, err)
            case _                     => this
        }
    }

    private [machine] final override def withHints(hints: DefuncHints): FancyDefuncError = this
    private [machine] final override def withReason(reason: String): FancyDefuncError = this
    private [machine] final override def label(label: String, offset: Int): FancyDefuncError = this
    private [machine] final override def amend(offset: Int, line: Int, col: Int): FancyDefuncError = {
        if (!this.entrenched) new FancyAmended(offset, line, col, this)
        else this
    }
    private [machine] final override def entrench: FancyDefuncError = {
        if (!this.entrenched) new FancyEntrenched(this)
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
    // def expected: IterableOnce[ErrorItem] // when 2.12 is dropped this will work better
    /** The error items produced by this error */
    private [errors] def expectedIterable: Iterable[ErrorItem]

    /** Adds the reasons and errors (or any other work) after the position and unexpected updates.
      * By default, this will just add the expected messages found in the `expectedIterable`.
      *
      * @note the default can, and should, be overriden for efficiency or to add more information
      */
    private [errors] def addLabelsAndReasons(state: TrivialState): Unit = state ++= expectedIterable

    /** Default implementation of `makeTrivial`, which updates the position of the error, and the
      * size of the unexpected token. This calls `addLabelsAndReasons` to complete any work.
      *
      * @note the default implementation should be overriden for EmptyErrors, which must not update the unexpected!
      */
    private [errors] override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state.updateUnexpected(unexpectedWidth)
        addLabelsAndReasons(state)
    }
}

private [machine] final class ClassicExpectedError(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem]) extends BaseError {
    override def isExpectedEmpty: Boolean = expected.isEmpty
    override def unexpectedWidth = 1
    override def expectedIterable: Iterable[ErrorItem] = expected
    //override def addLabelsAndReasons(state: TrivialState): Unit = state += expected
}
private [machine] final class ClassicExpectedErrorWithReason(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem], val reason: String)
    extends BaseError {
    override def isExpectedEmpty: Boolean = expected.isEmpty
    override def unexpectedWidth = 1
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def addLabelsAndReasons(state: TrivialState): Unit = {
        state += expected
        state += reason
    }
}
private [machine] final class ClassicUnexpectedError(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem], val unexpected: ErrorItem)
    extends BaseError {
    override def isExpectedEmpty: Boolean = expected.isEmpty
    override def unexpectedWidth = 1
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def addLabelsAndReasons(state: TrivialState): Unit = {
        state += expected
        state.updateUnexpected(unexpected)
    }
}
private [machine] final class ClassicFancyError(val offset: Int, val line: Int, val col: Int, val msgs: String*) extends FancyDefuncError {
    override def makeFancy(state: FancyState): Unit = {
        state.pos_=(line, col)
        state ++= msgs
    }
}
private [machine] final class EmptyError(val offset: Int, val line: Int, val col: Int) extends BaseError {
    override def isExpectedEmpty: Boolean = true
    override def unexpectedWidth = 0
    override def expectedIterable: Iterable[ErrorItem] = None
    override def makeTrivial(state: TrivialState): Unit = state.pos_=(line, col)
}
private [machine] final class TokenError(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem], val unexpectedWidth: Int)
    extends BaseError {
    override def isExpectedEmpty: Boolean = expected.isEmpty
    override def expectedIterable: Iterable[ErrorItem] = expected
    //override def addLabelsAndReasons(state: TrivialState): Unit = state += expected
}
private [machine] final class EmptyErrorWithReason(val offset: Int, val line: Int, val col: Int, val reason: String) extends BaseError {
    override def isExpectedEmpty: Boolean = true
    override def unexpectedWidth = 0
    override def expectedIterable: Iterable[ErrorItem] = None
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += reason
    }
}
private [machine] final class MultiExpectedError(val offset: Int, val line: Int, val col: Int, val expected: Set[ErrorItem], val unexpectedWidth: Int)
    extends BaseError {
    override def isExpectedEmpty: Boolean = expected.isEmpty
    override def expectedIterable: Iterable[ErrorItem] = expected
}

private [errors] final class TrivialMergedErrors private [errors] (val err1: TrivialDefuncError, val err2: TrivialDefuncError) extends TrivialDefuncError {
    override val isExpectedEmpty: Boolean = err1.isExpectedEmpty && err2.isExpectedEmpty
    override val entrenched: Boolean = err1.entrenched && err2.entrenched
    assume(err1.offset == err2.offset, "two errors only merge when they have matching offsets")
    val offset = err1.offset //Math.max(err1.offset, err2.offset)
    override def makeTrivial(state: TrivialState): Unit = {
        err1.makeTrivial(state)
        err2.makeTrivial(state)
    }
}

private [errors] final class FancyMergedErrors private [errors] (val err1: FancyDefuncError, val err2: FancyDefuncError) extends FancyDefuncError {
    override val entrenched: Boolean = err1.entrenched && err2.entrenched
    assume(err1.offset == err2.offset, "two errors only merge when they have matching offsets")
    override val offset = err1.offset //Math.max(err1.offset, err2.offset)
    override def makeFancy(state: FancyState): Unit = {
        err1.makeFancy(state)
        err2.makeFancy(state)
    }
}

private [errors] final class WithHints private [errors] (val err: TrivialDefuncError, val hints: DefuncHints) extends TrivialDefuncError {
    assume(!hints.isEmpty, "WithHints will always have non-empty hints")
    override def isExpectedEmpty: Boolean = false //err.isExpectedEmpty && hints.isEmpty
    override val offset = err.offset
    override val entrenched: Boolean = err.entrenched
    override def makeTrivial(state: TrivialState): Unit = {
        err.makeTrivial(state)
        state.whenAcceptingExpected {
            val size = hints.updateExpectedsAndGetSize(state)
            state.updateUnexpected(size)
        }
    }
}

private [errors] final class WithReason private [errors] (val err: TrivialDefuncError, val reason: String) extends TrivialDefuncError {
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override val entrenched: Boolean = err.entrenched
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = {
        err.makeTrivial(state)
        state += reason
    }
}

private [errors] final class WithLabel private [errors] (val err: TrivialDefuncError, val label: String) extends TrivialDefuncError {
    override def isExpectedEmpty: Boolean = label.isEmpty
    override val entrenched: Boolean = err.entrenched
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = {
        state.ignoreExpected {
            err.makeTrivial(state)
        }
        if (label.nonEmpty) state += Desc(label)
    }
}

private [errors] final class TrivialAmended private [errors] (val offset: Int, val line: Int, val col: Int, val err: TrivialDefuncError)
    extends TrivialDefuncError {
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        err.makeTrivial(state)
        assume(!err.entrenched, "an amendment will only occur on unentrenched errors")
        state.pos_=(line, col)
    }
}

private [errors] final class FancyAmended private [errors] (val offset: Int, val line: Int, val col: Int, val err: FancyDefuncError) extends FancyDefuncError {
    override def makeFancy(state: FancyState): Unit = {
        err.makeFancy(state)
        assume(!err.entrenched, "an amendment will only occur on unentrenched errors")
        state.pos_=(line, col)
    }
}

private [errors] final class TrivialEntrenched private [errors] (val err: TrivialDefuncError) extends TrivialDefuncError {
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override val entrenched: Boolean = true
    override val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = err.makeTrivial(state)
}

private [errors] final class FancyEntrenched private [errors] (val err: FancyDefuncError) extends FancyDefuncError {
    override val entrenched: Boolean = true
    override val offset = err.offset
    override def makeFancy(state: FancyState): Unit = err.makeFancy(state)
}
