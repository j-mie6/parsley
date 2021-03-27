package parsley.internal.errors

import parsley.errors.ErrorBuilder

private [internal] sealed trait ParseError {
    val offset: Int
    val col: Int
    val line: Int

    protected def format[Err](line: String, caret: Int)(implicit builder: ErrorBuilder[Err]): builder.ErrorInfoLines
    private [internal] final def format[Err](sourceName: Option[String])(implicit helper: LineBuilder, builder: ErrorBuilder[Err]): Err = {
        val (errLine, caret) = helper.getLineWithCaret(offset)
        val lines = format(errLine, caret)
        builder.format(builder.pos(line, col), builder.source(sourceName), lines)
    }
}
// The reasons here are lightweight, two errors can merge their messages, but messages do not get converted to hints
private [internal] case class TrivialError(offset: Int, line: Int, col: Int,
                                           unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String])
    extends ParseError {
    def format[Err](line: String, caret: Int)(implicit builder: ErrorBuilder[Err]): builder.ErrorInfoLines = {
        builder.vanillaError(
            builder.unexpected(unexpected.map(_.format)),
            builder.expected(builder.combineExpectedItems(expecteds.map(_.format))),
            builder.combineMessages(reasons.map(builder.reason(_))),
            builder.lineInfo(line, caret))
    }
}
private [internal] case class FailError(offset: Int, line: Int, col: Int, msgs: Set[String]) extends ParseError {
    def format[Err](line: String, caret: Int)(implicit builder: ErrorBuilder[Err]): builder.ErrorInfoLines = {
        builder.specialisedError(
            builder.combineMessages(msgs.map(builder.message(_))),
            builder.lineInfo(line, caret))
    }
}
