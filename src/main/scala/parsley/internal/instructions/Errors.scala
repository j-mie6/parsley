package parsley.internal.instructions

sealed trait ParseError {
    val offset: Int
    val col: Int
    val line: Int
}
case class TrivialError(offset: Int, col: Int, line: Int, unexpected: Option[ErrorItem], expecteds: Set[ErrorItem]) extends ParseError
case class FailMessage(offset: Int, col: Int, line: Int, msgs: Set[String]) extends ParseError

object ParseError {
    def unexpected(msg: String, offset: Int, col: Int, line: Int) = TrivialError(offset, col, line, Some(Desc(msg)), Set.empty)
    def fail(msg: String, offset: Int, col: Int, line: Int) = FailMessage(offset, col, line, Set(msg))
}

sealed trait ErrorItem
case class Raw(cs: String) extends ErrorItem
case class Desc(msg: String) extends ErrorItem
case object EndOfInput extends ErrorItem

final class Hint(val hint: Set[ErrorItem]) extends AnyVal {

}