package parsley.errors

import java.io.File
import scala.util.matching.Regex

trait ErrorBuilder[Err] {
    private [errors] final type _Err = Err

    def format(pos: Position, source: Context, ctxs: NestedContexts, lines: ErrorInfoLines): Err

    type Position
    type Context
    def pos(line: Int, col: Int): Position
    def source(sourceName: Option[File]): Context
    def contexualScope(context: String): Context

    type NestedContexts
    def nestContexts(contexts: List[Context]): NestedContexts

    type ErrorInfoLines
    def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines
    def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines

    type ExpectedItems
    type Messages
    def combineExpectedItems(alts: Set[Item]): ExpectedItems
    def combineMessages(alts: Set[Message]): Messages

    type UnexpectedLine
    type ExpectedLine
    type Message
    type LineInfo
    def unexpected(item: Option[Item]): UnexpectedLine
    def expected(alts: ExpectedItems): ExpectedLine
    def reason(reason: String): Message
    def message(msg: String): Message
    def lineInfo(line: String, errorPointsAt: Int): LineInfo

    type Item
    type Raw <: Item
    type Named <: Item
    type EndOfInput <: Item
    def raw(item: String): Raw
    def named(item: String): Named
    val endOfInput: EndOfInput
}

object ErrorBuilder {
    implicit val stringError: ErrorBuilder[String] = new ErrorBuilder[String] {

        override def format(pos: Position, source: Context, ctxs: NestedContexts, lines: ErrorInfoLines): String = {
            s"${mergeScopes(source, ctxs)}$pos:\n${lines.mkString("  ", "\n  ", "")}"
        }

        protected def mergeScopes(source: Context, ctxs: NestedContexts): String = (source, ctxs) match {
            case (None, None) => ""
            case (Some(name), None) => s"In $name"
            case (None, Some(ctxs)) => s"In $ctxs"
            case (Some(name), Some(ctxs)) => s"In $name, $ctxs"
        }

        type Position = String
        type Context = Option[String]
        override def pos(line: Int, col: Int): Position = s"(line $line, column $col)"
        override def source(sourceName: Option[File]): Context = sourceName.map(name => s"file '${name.getName}' ")
        override def contexualScope(context: String): Context = Some(context)

        type NestedContexts = Option[String]
        override def nestContexts(contexts: List[Context]): NestedContexts = {
            val nonEmptyContexts = contexts.flatten
            if (nonEmptyContexts.nonEmpty) Some(nonEmptyContexts.mkString(", "))
            else None
        }

        type ErrorInfoLines = List[String]
        override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
            val (src, caret) = line
            val reasons_ = reasons.collect {
                case reason if reason.nonEmpty => Some(reason)
            }
            combineOrUnknown((unexpected :: expected :: reasons_).flatten, src, caret)
        }
        override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = {
            val (src, caret) = line
            combineOrUnknown(msgs, src, caret)
        }

        private def combineOrUnknown(info: List[String], line: String, caret: String): ErrorInfoLines = {
            if (info.isEmpty) List(Unknown, line, caret)
            else info ++ List(line, caret)
        }

        type ExpectedItems = Option[String]
        type Messages = List[Message]
        override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts.toList.sorted.reverse.filter(_.nonEmpty) match {
            case Nil => None
            case List(alt) => Some(alt)
            case List(alt1, alt2) => Some(s"$alt2 or $alt1")
            // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
            case any@(alt::alts) if any.exists(_.contains(",")) => Some(s"${alts.reverse.mkString("; ")}; or $alt")
            case alt::alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
        }
        override def combineMessages(alts: Set[Message]): Messages = alts.filter(_.nonEmpty).toList

        type UnexpectedLine = Option[String]
        type ExpectedLine = Option[String]
        type Message = String
        type LineInfo = (String, String)
        override def unexpected(item: Option[Item]): UnexpectedLine = item.map("unexpected " + _)
        override def expected(alts: ExpectedItems): ExpectedLine = alts.map("expected " + _)
        override def reason(reason: String): Message = reason
        override def message(msg: String): Message = msg
        override def lineInfo(line: String, errorPointsAt: Int): LineInfo = (s"$errorLineStart$line", s"$errorLineStart${errorPointer(errorPointsAt)}")

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
}
