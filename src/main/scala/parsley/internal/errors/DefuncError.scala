package parsley.internal.errors

/* This file contains the defunctionalised forms of the error messages.
 * Essentially, whenever an error is created in the machine, it should make use of one of
 * these case classes. This means that every error message created will be done in a single
 * O(1) allocation, avoiding anything to do with the underlying sets, options etc.
 */
private [internal] sealed trait DefuncError {
    def asParseError(implicit builder: ErrorItemBuilder): ParseError
    protected final def expectedSet(errorItem: Option[ErrorItem]): Set[ErrorItem] = errorItem match {
        case None => ParseError.NoItems
        case Some(item) => Set(item)
    }
}
private [internal] case class ClassicExpectedError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], reason: Option[String]) extends DefuncError {
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(builder(offset)), expectedSet(expected), reason.fold(ParseError.NoReason)(Set(_)))
    }
}
private [internal] case class ClassicUnexpectedError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], unexpected: ErrorItem) extends DefuncError {
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(unexpected), expectedSet(expected), ParseError.NoReason)
    }
}
private [internal] case class ClassicFancyError(offset: Int, line: Int, col: Int, msg: String) extends DefuncError {
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        FailError(offset, line, col, Set(msg))
    }
}
private [internal] case class EmptyError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem]) extends DefuncError {
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, None, expectedSet(expected), ParseError.NoReason)
    }
}
private [internal] case class StringTokError(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], size: Int) extends DefuncError {
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, Some(builder(offset, size)), expectedSet(expected), ParseError.NoReason)
    }
}
private [internal] case class EmptyErrorWithReason(offset: Int, line: Int, col: Int, expected: Option[ErrorItem], reason: String) extends DefuncError {
    override def asParseError(implicit builder: ErrorItemBuilder): ParseError = {
        TrivialError(offset, line, col, None, expectedSet(expected), Set(reason))
    }
}
