/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.internal.errors.{Desc, ErrorItem, FancyError, ParseError, TrivialError}

/* This file contains the defunctionalised forms of the error messages.
 * Essentially, whenever an error is created in the machine, it should make use of one of
 * these case classes. This means that every error message created will be done in a single
 * O(1) allocation, avoiding anything to do with the underlying sets, options etc.
 */
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

private [machine] sealed abstract class DefuncError {
    private [machine] val isTrivialError: Boolean
    private [machine] val isExpectedEmpty: Boolean
    private [machine] val entrenched: Boolean = false
    private [machine] val offset: Int
    private [machine] final def asParseError(implicit builder: ErrorItemBuilder): ParseError = (this: @unchecked) match {
        case terr: MakesTrivial if terr.isTrivialError => terr.makeTrivial
        case ferr: MakesFancy                          => ferr.makeFancy
    }
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
    private [machine] final def merge(err: DefuncError): DefuncError = {
        if (this.offset > err.offset) this
        else if (err.offset > this.offset) err
        else if (!this.isTrivialError && err.isTrivialError) this
        else if (this.isTrivialError && !err.isTrivialError) err
        else new MergedErrors(this, err)
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

/** This is the common supertype of all "regular" trivial errors: those that result from failures as opposed to operations on errors.
  */
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
    // So long as the MergedErrors factory checks for parity and offset checks this is fine
    override val isTrivialError: Boolean = err1.isTrivialError
    override val isExpectedEmpty: Boolean = !isTrivialError || err1.isExpectedEmpty && err2.isExpectedEmpty
    override val entrenched: Boolean = err1.entrenched && err2.entrenched
    // So long as the MergedErrors factory checks that they are equal we can pick arbitrarily
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
    // So long as the WithHints factory ensures hints is nonEmpty this is false
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
