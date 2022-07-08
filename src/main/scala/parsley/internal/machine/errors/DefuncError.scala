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

// TODO: We could improve the memory footprint and the performance of this potentially by breaking the operations into fully trivial and fully fancy
// variants, removing the traits, mixins, and the `isTrivialError` values (and the `expectedEmpty` on fancy errors)
// this would also remove the unchecked matches and make everything fully total.
private [errors] sealed trait MakesTrivial { this: DefuncError =>
    val isTrivialError = true
    final def makeTrivial(implicit builder: ErrorItemBuilder): TrivialError = {
        val state = new TrivialState(offset, !builder.inRange(offset))
        makeTrivial(state)
        state.mkError
    }
    def makeTrivial(state: TrivialState): Unit
}
private [errors] sealed trait MakesFancy { this: DefuncError =>
    val isTrivialError = false
    val isExpectedEmpty = true
    final def makeFancy: FancyError = {
        val state = new FancyState(offset)
        makeFancy(state)
        state.mkError
    }
    def makeFancy(state: FancyState): Unit
}

/** This structure represents a collection of operations that build up a
  * `ParseError`. This is done to allow for the operations to be performed
  * lazily, or potentially not at all if the error message is discarded
  * or a merge occurs with a different error kind. It also allows for the
  * conversion to `ParseError` to make use of mutable collections, which
  * would otherwise be unsound.
  */
private [machine] sealed abstract class DefuncError {
    /** Is this error a trivial error, or is it fancy? */
    private [machine] val isTrivialError: Boolean
    /** Does this error, if its trivial, have any expected items? */
    private [machine] val isExpectedEmpty: Boolean
    /** Is this error protected from amendment via the `amend` combinator? */
    private [machine] val entrenched: Boolean = false
    /** The offset at which this error occured */
    private [machine] val offset: Int
    /** This function forced the lazy defunctionalised structure into a final `ParseError` value */
    private [machine] final def asParseError(implicit builder: ErrorItemBuilder): ParseError = (this: @unchecked) match {
        case terr: MakesTrivial if terr.isTrivialError => terr.makeTrivial
        case ferr: MakesFancy                          => ferr.makeFancy
    }
    // This is used by `DefuncHints` to turn any labels that would occur in this error message
    // into hints. This is done like this to allow for other parts of the error building process
    // to be ignored. This is only valid for trivial errors
    private [errors] final def collectHints(state: HintState): Unit = (this: @unchecked) match {
        case self: BaseError          =>
            state ++= self.expectedIterable
            state.updateSize(self.size)
        case self: WithLabel          => if (self.label.nonEmpty) state += Desc(self.label)
        case self: WithReason         => self.err.collectHints(state)
        case self: WithHints          =>
            self.hints.collect(0, state)
            self.err.collectHints(state)
        case self: MergedErrors       =>
            self.err1.collectHints(state)
            self.err2.collectHints(state)
        case self: Amended            => self.err.collectHints(state)
        case self: Entrenched         => self.err.collectHints(state)
    }

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
    private [machine] final def merge(err: DefuncError): DefuncError = {
        val thisOffset = this.offset
        val otherOffset = err.offset
        if (thisOffset > otherOffset) this
        else if (otherOffset > thisOffset) err
        else {
            val thisTrivial = this.isTrivialError
            val otherTrivial = err.isTrivialError
            if (!thisTrivial && otherTrivial) this
            else if (thisTrivial && !otherTrivial) err
            else new MergedErrors(this, err)
        }
    }
    private [machine] final def withHints(hints: DefuncHints): DefuncError = {
        if (hints.isEmpty || !this.isTrivialError) this
        else new WithHints(this.asInstanceOf[DefuncError with MakesTrivial], hints)
    }
    private [machine] final def withReason(reason: String): DefuncError = {
        if (this.isTrivialError) new WithReason(this.asInstanceOf[DefuncError with MakesTrivial], reason)
        else this
    }
    private [machine] final def label(label: String): DefuncError = {
        if (this.isTrivialError) new WithLabel(this.asInstanceOf[DefuncError with MakesTrivial], label)
        else this
    }
    private [machine] final def amend(offset: Int, line: Int, col: Int): DefuncError = {
        if (!this.entrenched) new Amended(offset, line, col, this)
        else this
    }
    private [machine] final def entrench: DefuncError = {
        if (!this.entrenched) new Entrenched(this)
        else this
    }
}

/** This is the common supertype of all "regular" trivial errors: those that result from failures as opposed to operations on errors. */
private [errors] sealed abstract class BaseError extends DefuncError with MakesTrivial {
    /** The size of the unexpected token demanded by this error */
    def size: Int
    // def expected: IterableOnce[ErrorItem] // when 2.12 is dropped this will work better
    /** The error items produced by this error */
    def expectedIterable: Iterable[ErrorItem]
}

private [machine] final class ClassicExpectedError(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem]) extends BaseError {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def size = 1
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state ++= expected
        state.updateUnexpected(1)
    }
}
private [machine] final class ClassicExpectedErrorWithReason(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem], val reason: String)
    extends BaseError {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def size = 1
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state += reason
        state.updateUnexpected(1)
    }
}
private [machine] final class ClassicUnexpectedError(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem], val unexpected: ErrorItem)
    extends BaseError {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def size = 1
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state.updateUnexpected(unexpected)
    }
}
private [machine] final class ClassicFancyError(val offset: Int, val line: Int, val col: Int, val msgs: String*) extends DefuncError with MakesFancy {
    override def makeFancy(state: FancyState): Unit = {
        state.pos_=(line, col)
        state ++= msgs
    }
}
private [machine] final class EmptyError(val offset: Int, val line: Int, val col: Int) extends BaseError {
    val isExpectedEmpty: Boolean = true
    override def size = 1
    override def expectedIterable: Iterable[ErrorItem] = None
    override def makeTrivial(state: TrivialState): Unit = state.pos_=(line, col)
}
private [machine] final class TokenError(val offset: Int, val line: Int, val col: Int, val expected: Option[ErrorItem], val size: Int) extends BaseError {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state.updateUnexpected(size)
    }
}
private [machine] final class EmptyErrorWithReason(val offset: Int, val line: Int, val col: Int, val reason: String) extends BaseError {
    val isExpectedEmpty: Boolean = true
    override def size = 1
    override def expectedIterable: Iterable[ErrorItem] = None
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += reason
    }
}
private [machine] final class MultiExpectedError(val offset: Int, val line: Int, val col: Int, val expected: Set[ErrorItem], val size: Int) extends BaseError {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def expectedIterable: Iterable[ErrorItem] = expected
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state ++= expected
        state.updateUnexpected(size)
    }
}

private [errors] final class MergedErrors private [errors] (val err1: DefuncError, val err2: DefuncError) extends DefuncError with MakesTrivial with MakesFancy {
    assume(err1.isTrivialError == err2.isTrivialError, "two errors only merge when they have the same kind")
    override val isTrivialError: Boolean = err1.isTrivialError
    assume(!(!isTrivialError) || err1.isExpectedEmpty && err2.isExpectedEmpty, "being a fancy error implies that your expected is empty")
    override val isExpectedEmpty: Boolean = /*!isTrivialError || */err1.isExpectedEmpty && err2.isExpectedEmpty
    override val entrenched: Boolean = err1.entrenched && err2.entrenched
    assume(err1.offset == err2.offset, "two errors only merge when they have matching offsets")
    val offset = err1.offset //Math.max(err1.offset, err2.offset)
    override def makeTrivial(state: TrivialState): Unit = {
        err1.asInstanceOf[MakesTrivial].makeTrivial(state)
        err2.asInstanceOf[MakesTrivial].makeTrivial(state)
    }
    override def makeFancy(state: FancyState): Unit = {
        err1.asInstanceOf[MakesFancy].makeFancy(state)
        err2.asInstanceOf[MakesFancy].makeFancy(state)
    }
}

private [errors] final class WithHints private [errors] (val err: DefuncError with MakesTrivial, val hints: DefuncHints) extends DefuncError with MakesTrivial {
    assume(!hints.isEmpty, "WithHints will always have non-empty hints")
    val isExpectedEmpty: Boolean = false //err.isExpectedEmpty && hints.isEmpty
    val offset = err.offset
    override val entrenched: Boolean = err.entrenched
    override def makeTrivial(state: TrivialState): Unit = {
        err.makeTrivial(state)
        state.whenAcceptingExpected {
            val size = hints.updateExpectedsAndGetSize(state)
            state.updateUnexpected(size)
        }
    }
}

private [errors] final class WithReason private [errors] (val err: DefuncError with MakesTrivial, val reason: String) extends DefuncError with MakesTrivial {
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override val entrenched: Boolean = err.entrenched
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = {
        err.makeTrivial(state)
        state += reason
    }
}

private [errors] final class WithLabel private [errors] (val err: DefuncError with MakesTrivial, val label: String) extends DefuncError with MakesTrivial {
    val isExpectedEmpty: Boolean = label.isEmpty
    override val entrenched: Boolean = err.entrenched
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = {
        state.ignoreExpected {
            err.makeTrivial(state)
        }
        if (label.nonEmpty) state += Desc(label)
    }
}

private [errors] final class Amended private [errors] (val offset: Int, val line: Int, val col: Int, val err: DefuncError)
    extends DefuncError with MakesTrivial with MakesFancy {
    override val isTrivialError: Boolean = err.isTrivialError
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        err.asInstanceOf[MakesTrivial].makeTrivial(state)
        state.pos_=(line, col)
    }
    override def makeFancy(state: FancyState): Unit = {
        err.asInstanceOf[MakesFancy].makeFancy(state)
        state.pos_=(line, col)
    }
}

private [errors] final class Entrenched private [errors] (val err: DefuncError) extends DefuncError with MakesTrivial with MakesFancy {
    override val isTrivialError: Boolean = err.isTrivialError
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override val entrenched: Boolean = true
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = err.asInstanceOf[MakesTrivial].makeTrivial(state)
    override def makeFancy(state: FancyState): Unit = err.asInstanceOf[MakesFancy].makeFancy(state)
}
