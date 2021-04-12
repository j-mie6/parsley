package parsley.errors

import scala.util.matching.Regex

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
class DefaultErrorBuilder extends ErrorBuilder[String] with revisions.Revision2 {
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
        if (info.isEmpty) Unknown +: lines
        else info ++: lines
    }

    type ExpectedItems = Option[String]
    type Messages = Seq[Message]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts.toList.sorted.reverse.filter(_.nonEmpty) match {
        case Nil => None
        case List(alt) => Some(alt)
        case List(alt1, alt2) => Some(s"$alt2 or $alt1")
        // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
        case any@(alt::alts) if any.exists(_.contains(",")) => Some(s"${alts.reverse.mkString("; ")}; or $alt")
        case alt::alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
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
    override def raw(item: String): Raw = item match {
        case "\n"            => "newline"
        case "\t"            => "tab"
        case " "             => "space"
        case Unprintable(up) => f"unprintable character (\\u${up.head.toInt}%04X)"
        // Do we want this only in unexpecteds?
        case cs              => "\"" + cs.takeWhile(c => c != '\n' && c != ' ') + "\""
    }
    override def named(item: String): Named = item
    override val endOfInput: EndOfInput = "end of input"

    private val Unprintable: Regex = "(\\p{C})".r
    private val Unknown = "unknown parse error"
}
// $COVERAGE-ON$