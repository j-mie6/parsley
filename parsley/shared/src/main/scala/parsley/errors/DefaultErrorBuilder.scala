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
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): String = {
        s"${source.fold("")(name => s"In $name ")}$pos:\n${lines.mkString("  ", "\n  ", "")}"
    }

    //override def format(pos: Position, source: Source, ctxs: NestedContexts, lines: ErrorInfoLines): String = {
    //    s"${mergeScopes(source, ctxs)}$pos:\n${lines.mkString("  ", "\n  ", "")}"
    //}

    /*protected def mergeScopes(source: Source, ctxs: NestedContexts): String = (source, ctxs) match {
        case (None, None) => ""
        case (Some(name), None) => s"In $name "
        case (None, Some(ctxs)) => s"In $ctxs "
        case (Some(name), Some(ctxs)) => s"In $name, $ctxs "
    }*/

    /** @inheritdoc */
    type Position = String
    /** @inheritdoc */
    type Source = Option[String]
    //type Context = Option[String]
    /** @inheritdoc */
    override def pos(line: Int, col: Int): Position = s"(line ${Integer.toUnsignedString(line)}, column ${Integer.toUnsignedString(col)})"
    /** @inheritdoc */
    override def source(sourceName: Option[String]): Source = sourceName.map(name => s"file '$name'")
    //override def contexualScope(context: String): Context = Some(context)

    //type NestedContexts = Option[String]
    /*override def nestContexts(contexts: List[Context]): NestedContexts = {
        val nonEmptyContexts = contexts.flatten
        if (nonEmptyContexts.nonEmpty) Some(nonEmptyContexts.mkString(", "))
        else None
    }*/

    /** @inheritdoc */
    type ErrorInfoLines = Seq[String]
    /** @inheritdoc */
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, lines: LineInfo): ErrorInfoLines = {
        val reasons_ = reasons.collect {
            case reason if reason.nonEmpty => Some(reason)
        }
        DefaultErrorBuilder.combineInfoWithLines((unexpected +: expected +: reasons_).flatten, lines)
    }
    /** @inheritdoc */
    override def specialisedError(msgs: Messages, lines: LineInfo): ErrorInfoLines = DefaultErrorBuilder.combineInfoWithLines(msgs, lines)

    /** @inheritdoc */
    type ExpectedItems = Option[String]
    /** @inheritdoc */
    type Messages = Seq[Message]
    /** @inheritdoc */
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = DefaultErrorBuilder.disjunct(alts)
    /** @inheritdoc */
    override def combineMessages(alts: Seq[Message]): Messages = alts.filter(_.nonEmpty)

    /** @inheritdoc */
    type UnexpectedLine = Option[String]
    /** @inheritdoc */
    type ExpectedLine = Option[String]
    /** @inheritdoc */
    type Message = String
    /** @inheritdoc */
    type LineInfo = Seq[String]
    /** @inheritdoc */
    override def unexpected(item: Option[Item]): UnexpectedLine = item.map("unexpected " + _)
    /** @inheritdoc */
    override def expected(alts: ExpectedItems): ExpectedLine = alts.map("expected " + _)
    /** @inheritdoc */
    override def reason(reason: String): Message = reason
    /** @inheritdoc */
    override def message(msg: String): Message = msg

    /** @inheritdoc */
    override val numLinesBefore = 1
    /** @inheritdoc */
    override val numLinesAfter = 1
    /** @inheritdoc */
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo = {
        linesBefore.map(line => s"$errorLineStart$line") ++:
        Seq(s"$errorLineStart$line", s"${" " * errorLineStart.length}${errorPointer(errorPointsAt, errorWidth)}") ++:
        linesAfter.map(line => s"$errorLineStart$line")
    }

    private val errorLineStart = ">"
    private def errorPointer(caretAt: Int, caretWidth: Int) = s"${" " * caretAt}${"^" * caretWidth}"

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
    override def named(item: String): Named = item
    /** @inheritdoc */
    override val endOfInput: EndOfInput = DefaultErrorBuilder.EndOfInput
}
/** Helper functions used to build the `DefaultErrorBuilder` error messages.
  *
  * @since 4.3.0
  */
object DefaultErrorBuilder {
    private final val Unknown = "unknown parse error"
    private final val EndOfInput = "end of input"

    private def disjunct(alts: Iterable[String]): Option[String] = disjunct(alts, oxfordComma = true)
    private def disjunct(alts: Iterable[String], oxfordComma: Boolean): Option[String] = helpers.disjunct(alts.toList.filter(_.nonEmpty), oxfordComma)

    private def combineInfoWithLines(info: Seq[String], lines: Seq[String]): Seq[String] = {
        if (info.isEmpty) Unknown +: lines
        else info ++: lines
    }

    private def raw(item: String) = helpers.renderRawString(item)
}
// $COVERAGE-ON$
