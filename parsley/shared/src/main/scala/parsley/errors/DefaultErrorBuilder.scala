/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
/** This class us used to build Parsley's default error messages.
  *
  * While it compiles with the `ErrorBuilder` typeclass, it should not
  * be considered a stable contract: the formatting can be changed at any
  * time and without notice. The API, however, will remain stable.
  *
  * @since 3.0.0
  * @note this class is abstract as it does not implement `unexpectedToken`: when creating an instance mix-in an appropriate
  *       token extractor from `parsley.errors.tokenextractors`.
  * @group formatting
  */
abstract class DefaultErrorBuilder extends ErrorBuilder[String] {
    /** @inheritdoc */
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): String = DefaultErrorBuilder.format(pos, source, lines)

    //override def format(pos: Position, source: Source, ctxs: NestedContexts, lines: ErrorInfoLines): String = {
    //    DefaultErrorBuilder.blockError(header = s"${DefaultErrorBuilder.mergeScopes(source, ctxs)}$pos", lines, indent = 2)"
    //}

    /** @inheritdoc */
    type Position = String
    /** @inheritdoc */
    type Source = Option[String]
    //type Context = Option[String]
    /** @inheritdoc */
    override def pos(line: Int, col: Int): Position = DefaultErrorBuilder.pos(line, col)
    /** @inheritdoc */
    override def source(sourceName: Option[String]): Source = DefaultErrorBuilder.source(sourceName)
    //override def contexualScope(context: String): Context = ???

    //type NestedContexts = Option[String]
    /*override def nestContexts(contexts: List[Context]): NestedContexts = ???*/

    /** @inheritdoc */
    type ErrorInfoLines = Seq[String]
    /** @inheritdoc */
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, lines: LineInfo): ErrorInfoLines = {
        DefaultErrorBuilder.vanillaError(unexpected, expected, reasons, lines)
    }
    /** @inheritdoc */
    override def specialisedError(msgs: Messages, lines: LineInfo): ErrorInfoLines = DefaultErrorBuilder.specialisedError(msgs, lines)

    /** @inheritdoc */
    type ExpectedItems = Option[String]
    /** @inheritdoc */
    type Messages = Seq[Message]
    /** @inheritdoc */
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = DefaultErrorBuilder.disjunct(alts)
    /** @inheritdoc */
    override def combineMessages(alts: Seq[Message]): Messages = DefaultErrorBuilder.combineMessages(alts)

    /** @inheritdoc */
    type UnexpectedLine = Option[String]
    /** @inheritdoc */
    type ExpectedLine = Option[String]
    /** @inheritdoc */
    type Message = String
    /** @inheritdoc */
    type LineInfo = Seq[String]
    /** @inheritdoc */
    override def unexpected(item: Option[Item]): UnexpectedLine = DefaultErrorBuilder.unexpected(item)
    /** @inheritdoc */
    override def expected(alts: ExpectedItems): ExpectedLine = DefaultErrorBuilder.expected(alts)
    /** @inheritdoc */
    override def reason(reason: String): Message = DefaultErrorBuilder.reason(reason)
    /** @inheritdoc */
    override def message(msg: String): Message = DefaultErrorBuilder.message(msg)

    /** @inheritdoc */
    override val numLinesBefore = DefaultErrorBuilder.NumLinesBefore
    /** @inheritdoc */
    override val numLinesAfter = DefaultErrorBuilder.NumLinesAfter
    /** @inheritdoc */
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo = {
        DefaultErrorBuilder.lineInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth)
    }

    /** @inheritdoc */
    type Item = String
    /** @inheritdoc */
    type Raw = String
    /** @inheritdoc */
    type Named = String
    /** @inheritdoc */
    type EndOfInput = String
    /** @inheritdoc */
    override def raw(item: String): Raw = DefaultErrorBuilder.raw(item)
    /** @inheritdoc */
    override def named(item: String): Named = DefaultErrorBuilder.named(item)
    /** @inheritdoc */
    override val endOfInput: EndOfInput = DefaultErrorBuilder.EndOfInput
}
/** Helper functions used to build the `DefaultErrorBuilder` error messages.
  *
  * @since 4.3.0
  */
object DefaultErrorBuilder {
    final val Unknown = "unknown parse error"
    final val EndOfInput = "end of input"
    final val ErrorLineStart = ">"
    final val NumLinesBefore = 1
    final val NumLinesAfter = 1

    def format(pos: String, source: Option[String], lines: Seq[String]): String = {
        blockError(header = s"${source.fold("")(name => s"In $name ")}$pos", lines, indent = 2)
    }
    def source(sourceName: Option[String]): Option[String] = sourceName.map(name => s"file '$name'")
    def vanillaError(unexpected: Option[String], expected: Option[String], reasons: Iterable[String], lines: Seq[String]) = {
        DefaultErrorBuilder.combineInfoWithLines(Seq.concat(unexpected, expected, reasons), lines)
    }
    def specialisedError(msgs: Seq[String], lines: Seq[String]): Seq[String] = DefaultErrorBuilder.combineInfoWithLines(msgs, lines)

    def blockError(header: String, lines: Iterable[String], indent: Int) = s"$header:\n${indentAndUnlines(lines, indent)}"
    def indentAndUnlines(lines: Iterable[String], indent: Int) = lines.mkString(" " * indent, "\n" + " " * indent, "")

    def pos(line: Int, col: Int) = s"(line ${Integer.toUnsignedString(line)}, column ${Integer.toUnsignedString(col)})"

    def disjunct(alts: Iterable[String]): Option[String] = disjunct(alts, oxfordComma = true)
    def disjunct(alts: Iterable[String], oxfordComma: Boolean): Option[String] = helpers.disjunct(alts.toList.filter(_.nonEmpty), oxfordComma)

    def combineMessages(alts: Seq[String]): Seq[String] = alts.filter(_.nonEmpty)

    def combineInfoWithLines(info: Seq[String], lines: Seq[String]): Seq[String] = {
        if (info.isEmpty) Unknown +: lines
        else info ++: lines
    }

    def unexpected(item: Option[String]): Option[String] = item.map("unexpected " + _)
    def expected(alts: Option[String]): Option[String] = alts.map("expected " + _)
    def reason(reason: String): String = reason
    def message(msg: String): String = msg

    def raw(item: String) = helpers.renderRawString(item)
    def named(item: String) = item

    def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): Seq[String] = {
        Seq.concat(linesBefore.map(inputLine), Seq(inputLine(line), caretLine(errorPointsAt, errorWidth)), linesAfter.map(inputLine))
    }

    def inputLine(line: String) = s"${DefaultErrorBuilder.ErrorLineStart}$line"
    def caretLine(caretAt: Int, caretWidth: Int) = s"${" " * (ErrorLineStart.length + caretAt)}${"^" * caretWidth}"

    /*def mergeScopes(source: Option[String], ctxs: Option[String]): String = (source, ctxs) match {
        case (None, None) => ""
        case (Some(name), None) => s"In $name "
        case (None, Some(ctxs)) => s"In $ctxs "
        case (Some(name), Some(ctxs)) => s"In $name, $ctxs "
    }*/

    /*def nestContexts(contexts: List[Option[String]]): Option[String] = {
        val nonEmptyContexts = contexts.flatten
        if (nonEmptyContexts.nonEmpty) Some(nonEmptyContexts.mkString(", "))
        else None
    }*/
    /*def contexualScope(context: String): Option[String] = Some(context)*/
}
// $COVERAGE-ON$
