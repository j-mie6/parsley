package parsley.internal.machine.errors

import parsley.internal.errors.{ParseError, TrivialError, FancyError, ErrorItem, Desc}

import scala.collection.mutable
import scala.annotation.tailrec

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
        case ferr: MakesFancy => ferr.makeFancy
    }
    @tailrec private [errors] final def collectHints()(implicit state: HintState): Unit = (this: @unchecked) match {
        case BaseError(expected) =>
            for (item <- expected) state += item
            state.updateSize(1)
        case self: TokenError =>
            for (item <- self.expected) state += item
            state.updateSize(self.size)
        case self: MultiExpectedError =>
            state.updateSize(self.size)
            state ++= self.expected
        case self: WithLabel => if (self.label.nonEmpty) state += Desc(self.label)
        case self: WithReason => self.err.collectHints()
        case self: WithHints =>
            self.hints.collect(0)
            self.err.collectHints()
        case self: MergedErrors =>
            self.err1.collectHintsNonTail()
            self.err2.collectHints()
        case self: Amended => self.err.collectHints()
        case self: Entrenched => self.err.collectHints()
    }
    final private def collectHintsNonTail()(implicit state: HintState): Unit = collectHints()
}

private [errors] object BaseError {
    def unapply(err: DefuncError): Option[Option[ErrorItem]] = err match {
        case err: ClassicExpectedError => Some(err.expected)
        case err: ClassicExpectedErrorWithReason => Some(err.expected)
        case err: ClassicUnexpectedError => Some(err.expected)
        case err: EmptyError => Some(None)
        case err: EmptyErrorWithReason => Some(None)
        case _ => None
    }
}

private [machine] case class ClassicExpectedError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem]) extends DefuncError with MakesTrivial {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state.updateUnexpected(1)
    }
}
private [machine] case class ClassicExpectedErrorWithReason(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], reason: String)
    extends DefuncError with MakesTrivial  {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state += reason
        state.updateUnexpected(1)
    }
}
private [machine] case class ClassicUnexpectedError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], unexpected: ErrorItem)
    extends DefuncError with MakesTrivial {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state.updateUnexpected(unexpected)
    }
}
private [machine] case class ClassicFancyError(offset: Int, line: Int, col: Int, msgs: String*) extends DefuncError with MakesFancy {
    override def makeFancy(state: FancyState): Unit = {
        state.pos_=(line, col)
        state ++= msgs
    }
}
private [machine] case class EmptyError(offset: Int, line: Int, col: Int) extends DefuncError with MakesTrivial  {
    val isExpectedEmpty: Boolean = true
    override def makeTrivial(state: TrivialState): Unit = state.pos_=(line, col)
}
private [machine] case class TokenError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], size: Int) extends DefuncError with MakesTrivial  {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += expected
        state.updateUnexpected(size)
    }
}
private [machine] case class EmptyErrorWithReason(offset: Int, line: Int, col: Int, reason: String)
    extends DefuncError with MakesTrivial  {
    val isExpectedEmpty: Boolean = true
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state += reason
    }
}
private [machine] case class MultiExpectedError(offset: Int, line: Int, col: Int, expected: Set[ErrorItem], size: Int) extends DefuncError with MakesTrivial {
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def makeTrivial(state: TrivialState): Unit = {
        state.pos_=(line, col)
        state ++= expected
        state.updateUnexpected(size)
    }
}

private [errors] case class MergedErrors private (err1: DefuncError, err2: DefuncError) extends DefuncError with MakesTrivial with MakesFancy {
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
private [machine] object MergedErrors {
    def apply(err1: DefuncError, err2: DefuncError): DefuncError = {
        if (err1.offset > err2.offset) err1
        else if (err2.offset > err1.offset) err2
        else if (!err1.isTrivialError && err2.isTrivialError) err1
        else if (err1.isTrivialError && !err2.isTrivialError) err2
        else new MergedErrors(err1, err2)
    }
}

private [errors] case class WithHints private (err: DefuncError, hints: DefuncHints) extends DefuncError with MakesTrivial {
    // So long as the WithHints factory ensures hints is nonEmpty this is false
    val isExpectedEmpty: Boolean = false //err.isExpectedEmpty && hints.isEmpty
    val offset = err.offset
    override val entrenched: Boolean = err.entrenched
    override def makeTrivial(state: TrivialState): Unit = {
        err.asInstanceOf[MakesTrivial].makeTrivial(state)
        val (expecteds, size) = hints.toExpectedsAndSize
        state ++= expecteds
        state.updateUnexpected(size)
    }
}

private [machine] object WithHints {
    def apply(err: DefuncError, hints: DefuncHints): DefuncError = {
        if (hints.isEmpty || !err.isTrivialError) err
        else new WithHints(err, hints)
    }
}

private [errors] case class WithReason private (err: DefuncError, reason: String) extends DefuncError with MakesTrivial {
    override val isTrivialError: Boolean = err.isTrivialError
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override val entrenched: Boolean = err.entrenched
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = {
        err.asInstanceOf[MakesTrivial].makeTrivial(state)
        state += reason
    }
}
private [machine] object WithReason {
    def apply(err: DefuncError, reason: String): DefuncError = {
        if (err.isTrivialError) new WithReason(err, reason)
        else err
    }
}

private [errors] case class WithLabel private (err: DefuncError, label: String) extends DefuncError with MakesTrivial {
    val isExpectedEmpty: Boolean = label.isEmpty
    override val entrenched: Boolean = err.entrenched
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = {
        state.ignoreExpected {
            err.asInstanceOf[MakesTrivial].makeTrivial(state)
        }
        if (label.nonEmpty) state += Desc(label)
    }
}
private [machine] object WithLabel {
    def apply(err: DefuncError, label: String): DefuncError = {
        if (err.isTrivialError) new WithLabel(err, label)
        else err
    }
}

private [errors] case class Amended private (offset: Int, line: Int, col: Int, err: DefuncError) extends DefuncError with MakesTrivial with MakesFancy {
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
private [machine] object Amended {
    def apply(offset: Int, line: Int, col: Int, err: DefuncError): DefuncError = {
        if (!err.entrenched) new Amended(offset, line, col, err)
        else err
    }
}

private [errors] case class Entrenched private (err: DefuncError) extends DefuncError with MakesTrivial with MakesFancy {
    override val isTrivialError: Boolean = err.isTrivialError
    override val isExpectedEmpty: Boolean = err.isExpectedEmpty
    override val entrenched: Boolean = true
    val offset = err.offset
    override def makeTrivial(state: TrivialState): Unit = err.asInstanceOf[MakesTrivial].makeTrivial(state)
    override def makeFancy(state: FancyState): Unit = err.asInstanceOf[MakesFancy].makeFancy(state)
}
private [machine] object Entrenched {
    def apply(err: DefuncError): DefuncError = {
        if (!err.entrenched) new Entrenched(err)
        else err
    }
}