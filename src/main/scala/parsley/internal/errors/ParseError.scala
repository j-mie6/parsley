/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.errors

import parsley.errors.ErrorBuilder

private [internal] sealed trait ParseError {
    val offset: Int
    val col: Int
    val line: Int

    protected def format(line: String, beforeLines: List[String], afterLines: List[String], caret: Int)
                        (implicit builder: ErrorBuilder[_]): builder.ErrorInfoLines
    private [internal] final def format[Err](sourceName: Option[String])(implicit helper: LineBuilder, builder: ErrorBuilder[Err]): Err = {
        val Some((errLine, caret)) = helper.getLineWithCaret(offset)
        val beforeLines = helper.getLinesBefore(offset, builder.numLinesBefore)
        val afterLines = helper.getLinesAfter(offset, builder.numLinesAfter)
        val lines = format(errLine, beforeLines, afterLines, caret)
        builder.format(builder.pos(line, col), builder.source(sourceName), lines)
    }
}
// The reasons here are lightweight, two errors can merge their messages, but messages do not get converted to hints
private [internal] case class TrivialError(offset: Int, line: Int, col: Int,
                                           unexpected: Option[UnexpectItem], expecteds: Set[ExpectItem], reasons: Set[String], lexicalError: Boolean)
    extends ParseError {
    def format(line: String, beforeLines: List[String], afterLines: List[String], caret: Int)(implicit builder: ErrorBuilder[_]): builder.ErrorInfoLines = {
        val unexpectedTok = unexpected.map(_.formatUnexpect(lexicalError))
        val caretSize = unexpectedTok.fold(1)(_._2.toCaretLength(this.line, this.col, line.length, afterLines.map(_.length)))
        builder.vanillaError(
            builder.unexpected(unexpectedTok.map(_._1)),
            builder.expected(builder.combineExpectedItems(expecteds.map(_.formatExpect))),
            builder.combineMessages(reasons.map(builder.reason(_)).toSeq),
            builder.lineInfo(line, beforeLines, afterLines, caret, caretSize))
    }
}
private [internal] case class FancyError(offset: Int, line: Int, col: Int, msgs: List[String], lexicalError: Boolean) extends ParseError {
    def format(line: String, beforeLines: List[String], afterLines: List[String], caret: Int)(implicit builder: ErrorBuilder[_]): builder.ErrorInfoLines = {
        builder.specialisedError(
            builder.combineMessages(msgs.map(builder.message(_))),
            builder.lineInfo(line, beforeLines, afterLines, caret, 1))
    }
}
