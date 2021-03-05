package parsley.internal.machine.errors

import parsley.internal.errors.{ParseError, TrivialError, FailError, ErrorItem, Desc}

import scala.collection.mutable
import scala.annotation.tailrec

/* This file contains the defunctionalised forms of the error messages.
 * Essentially, whenever an error is created in the machine, it should make use of one of
 * these case classes. This means that every error message created will be done in a single
 * O(1) allocation, avoiding anything to do with the underlying sets, options etc.
 */
private [machine] sealed abstract class DefuncError {
    val isTrivialError: Boolean
    val isExpectedEmpty: Boolean
    val offset: Int
    private [machine] def asParseError(implicit builder: ErrorItemBuilder): ParseError
    @tailrec private [errors] final def collectHints(set: mutable.Set[ErrorItem]): Unit = (this: @unchecked) match {
        case BaseError(expected) => for (item <- expected) set += item
        case self: MultiExpectedError => set ++= self.expected
        case self: WithLabel => if (self.label.nonEmpty) set += Desc(self.label)
        case self: WithReason => self.err.collectHints(set)
        case self: WithHints =>
            self.hints.collect(set, 0)
            self.err.collectHints(set)
        case self: MergedErrors =>
            self.err1.collectHintsNonTail(set)
            self.err2.collectHints(set)
    }
    final private def collectHintsNonTail(set: mutable.Set[ErrorItem]): Unit = collectHints(set)
    protected final def expectedSet(errorItem: Option[ErrorItem]): Set[ErrorItem] = errorItem match {
        case None => ParseError.NoItems
        case Some(item) => Set(item)
    }
}

private [errors] object BaseError {
    def unapply(err: DefuncError): Option[Option[ErrorItem]] = err match {
        case err: ClassicExpectedError => Some(err.expected)
        case err: ClassicExpectedErrorWithReason => Some(err.expected)
        case err: ClassicUnexpectedError => Some(err.expected)
        case err: EmptyError => Some(err.expected)
        case err: EmptyErrorWithReason => Some(err.expected)
        case err: StringTokError => Some(err.expected)
        case _ => None
    }
}

private [machine] case class ClassicExpectedError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem]) extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(builder(offset)), expectedSet(expected), ParseError.NoReason)
    }
}
private [machine] case class ClassicExpectedErrorWithReason(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], reason: String)
    extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(builder(offset)), expectedSet(expected), Set(reason))
    }
}
private [machine] case class ClassicUnexpectedError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], unexpected: ErrorItem) extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(unexpected), expectedSet(expected), ParseError.NoReason)
    }
}
private [machine] case class ClassicFancyError(offset: Int, line: Int, col: Int, msg: String) extends DefuncError {
    val isTrivialError: Boolean = false
    val isExpectedEmpty: Boolean = true
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        FailError(offset, line, col, Set(msg))
    }
}
private [machine] case class EmptyError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem]) extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, None, expectedSet(expected), ParseError.NoReason)
    }
}
private [machine] case class StringTokError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], size: Int) extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(builder(offset, size)), expectedSet(expected), ParseError.NoReason)
    }
}
private [machine] case class EmptyErrorWithReason(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], reason: String) extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, None, expectedSet(expected), Set(reason))
    }
}
private [machine] case class MultiExpectedError(offset: Int, line: Int, col: Int, expected: Set[ErrorItem]) extends DefuncError {
    val isTrivialError: Boolean = true
    val isExpectedEmpty: Boolean = expected.isEmpty
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(builder(offset)), expected, ParseError.NoReason)
    }
}

private [errors] case class MergedErrors private (err1: DefuncError, err2: DefuncError) extends DefuncError {
    // So long as the MergedErrors factory checks for parity and offset checks this is fine
    val isTrivialError: Boolean = err1.isTrivialError
    val isExpectedEmpty: Boolean = !isTrivialError || err1.isExpectedEmpty && err2.isExpectedEmpty
    // So long as the MergedErrors factory checks that they are equal we can pick arbitrarily
    val offset = err1.offset //Math.max(err1.offset, err2.offset)
    // This has been optimised assuming the factory is doing its job
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        val err1_ = err1.asParseError
        ((err1_, err2.asParseError): @unchecked) match {
            case (_this: FailError, _that: FailError) => FailError(offset, err1_.line, err1_.col, _this.msgs union _that.msgs)
            case (TrivialError(_, _, _, u1, es1, rs1), TrivialError(_, _, _, u2, es2, rs2)) =>
                val u = (u1, u2) match {
                    case (Some(u1), Some(u2)) => Some(ErrorItem.higherPriority(u1, u2))
                    case _ => u1.orElse(u2)
                }
                TrivialError(offset, err1_.line, err1_.col, u, es1 union es2, rs1 union rs2)
        }
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

private [errors] case class WithHints private (err: DefuncError, hints: DefuncHints) extends DefuncError {
    // So long as the WithHints factory ensures the err is trivial this is true
    val isTrivialError: Boolean = true //err.isTrivialError
    // So long as the WithHints factory ensures hints is nonEmpty this is false
    val isExpectedEmpty: Boolean = false //err.isExpectedEmpty && hints.isEmpty
    val offset = err.offset
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        err.asParseError.withHints(hints.toSet)
    }
}

private [machine] object WithHints {
    def apply(err: DefuncError, hints: DefuncHints): DefuncError = {
        if (hints.isEmpty || !err.isTrivialError) err
        else new WithHints(err, hints)
    }
}

private [machine] case class WithReason(err: DefuncError, reason: String) extends DefuncError {
    val isTrivialError: Boolean = err.isTrivialError
    val isExpectedEmpty: Boolean = err.isExpectedEmpty
    val offset = err.offset
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        err.asParseError.giveReason(reason)
    }
}

private [errors] case class WithLabel private (err: DefuncError, label: String) extends DefuncError {
    // So long as the WithLabel factory ensures the err is trivial this is true
    val isTrivialError: Boolean = true //err.isTrivialError
    val isExpectedEmpty: Boolean = label.isEmpty
    val offset = err.offset
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        (err.asParseError: @unchecked) match {
            //  - otherwise if this is a hide, the expected set is discarded
            case err: TrivialError if label.isEmpty => err.copy(expecteds = Set.empty)
            //  - otherwise expected set is replaced by singleton containing this label
            case err: TrivialError                  => err.copy(expecteds = Set(Desc(label)))
        }
    }
}
private [machine] object WithLabel {
    def apply(err: DefuncError, label: String): DefuncError = {
        if (err.isTrivialError) new WithLabel(err, label)
        else err
    }
}