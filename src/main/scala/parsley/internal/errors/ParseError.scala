package parsley.internal.errors

import ParseError.Unknown

private [internal] sealed trait ParseError {
    val offset: Int
    val col: Int
    val line: Int

    def withHints(hints: Set[ErrorItem]): ParseError
    def giveReason(reason: String): ParseError
    private [internal] def pretty(sourceName: Option[String])(implicit helper: LineBuilder): String

    protected final def posStr(sourceName: Option[String]): String = {
        val scopeName = sourceName.fold("")(name => s"In file '$name' ")
        s"$scopeName(line $line, column $col)"
    }

    protected final def disjunct(alts: List[String]): Option[String] = alts.sorted.reverse.filter(_.nonEmpty) match {
        case Nil => None
        case List(alt) => Some(alt)
        case List(alt1, alt2) => Some(s"$alt2 or $alt1")
        // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
        case any@(alt::alts) if any.exists(_.contains(",")) => Some(s"${alts.reverse.mkString("; ")}; or $alt")
        case alt::alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
    }

    protected final def assemble(sourceName: Option[String], infoLines: List[String])(implicit helper: LineBuilder): String = {
        val topStr = posStr(sourceName)
        val (line, caret) = helper.getLineWithCaret(offset)
        val info = infoLines.filter(_.nonEmpty).mkString("\n  ")
        s"$topStr:\n  ${if (info.isEmpty) Unknown else info}\n  >${line}\n  >${" " * caret}^"
    }
}
// The reasons here are lightweight, two errors can merge their messages, but messages do not get converted to hints
private [internal] case class TrivialError(offset: Int, line: Int, col: Int,
                                           unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String])
    extends ParseError {
    def withHints(hints: Set[ErrorItem]): ParseError = copy(expecteds = expecteds union hints)
    def giveReason(reason: String): ParseError = copy(reasons = reasons + reason)

    def pretty(sourceName: Option[String])(implicit helper: LineBuilder): String = {
        assemble(sourceName, List(unexpectedInfo, expectedInfo).flatten ::: reasons.toList)
    }

    private def unexpectedInfo: Option[String] = unexpected.map(u => s"unexpected ${u.msg}")
    private def expectedInfo: Option[String] = disjunct(expecteds.map(_.msg).toList).map(es => s"expected $es")
}
private [internal] case class FailError(offset: Int, line: Int, col: Int, msgs: Set[String]) extends ParseError {
    def withHints(hints: Set[ErrorItem]): ParseError = this
    def giveReason(reason: String): ParseError = this
    def pretty(sourceName: Option[String])(implicit helper: LineBuilder): String = {
        assemble(sourceName, msgs.toList)
    }
}

private [internal] object ParseError {
    val Unknown = "unknown parse error"
    val NoReason = Set.empty[String]
    val NoItems = Set.empty[ErrorItem]
}
