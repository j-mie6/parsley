package parsley.errors

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
/**
  * This is the class used to build Parsley's default error messages.
  * While it compiles with the `ErrorBuilder` typeclass, it should not
  * be considered a stable contract: the formatting can be changed at any
  * time and without notice. The API, however, will remain stable.
  * @since 3.0.0
  */
class DefaultErrorBuilder extends ErrorBuilder[String] {
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

    type Position = String
    type Source = Option[String]
    //type Context = Option[String]
    override def pos(line: Int, col: Int): Position = s"(line $line, column $col)"
    override def source(sourceName: Option[String]): Source = sourceName.map(name => s"file '$name'")
    //override def contexualScope(context: String): Context = Some(context)

    //type NestedContexts = Option[String]
    /*override def nestContexts(contexts: List[Context]): NestedContexts = {
        val nonEmptyContexts = contexts.flatten
        if (nonEmptyContexts.nonEmpty) Some(nonEmptyContexts.mkString(", "))
        else None
    }*/

    type ErrorInfoLines = Seq[String]
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, lines: LineInfo): ErrorInfoLines = {
        val reasons_ = reasons.collect {
            case reason if reason.nonEmpty => Some(reason)
        }
        combineOrUnknown((unexpected +: expected +: reasons_).flatten, lines)
    }
    override def specialisedError(msgs: Messages, lines: LineInfo): ErrorInfoLines = combineOrUnknown(msgs, lines)

    private def combineOrUnknown(info: Seq[String], lines: Seq[String]): ErrorInfoLines = {
        if (info.isEmpty) DefaultErrorBuilder.Unknown +: lines
        else info ++: lines
    }

    type ExpectedItems = Option[String]
    type Messages = Seq[Message]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = {
        helpers.combineAsList(alts.toList.filter(_.nonEmpty))
    }
    override def combineMessages(alts: Seq[Message]): Messages = alts.filter(_.nonEmpty)

    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[String]
    type Message = String
    type LineInfo = Seq[String]
    override def unexpected(item: Option[Item]): UnexpectedLine = item.map("unexpected " + _)
    override def expected(alts: ExpectedItems): ExpectedLine = alts.map("expected " + _)
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    override val numLinesBefore = 1
    override val numLinesAfter = 1
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo = {
        linesBefore.map(line => s"$errorLineStart$line") ++:
        Seq(s"$errorLineStart$line", s"${" " * errorLineStart.length}${errorPointer(errorPointsAt)}") ++:
        linesAfter.map(line => s"$errorLineStart$line")
    }

    private val errorLineStart = ">"
    private def errorPointer(caretAt: Int) = s"${" " * caretAt}^"

    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String
    override def raw(item: String): Raw = helpers.renderRawString(item)
    override def named(item: String): Named = item
    override val endOfInput: EndOfInput = "end of input"
}
private object DefaultErrorBuilder {
    private val Unknown = "unknown parse error"
}
// $COVERAGE-ON$
