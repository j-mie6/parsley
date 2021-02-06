package parsley.internal.instructions

sealed trait ParseError {
    val offset: Int
    val col: Int
    val line: Int

    def merge(that: ParseError): ParseError = {
        if (this.offset < that.offset) that
        else if (this.offset > that.offset) this
        else (this, that) match {
            case (_: FailError, _: TrivialError) => this
            case (_: TrivialError, _: FailError) => that
            case (_this: FailError, _that: FailError) => FailError(offset, line, col, _this.msgs union _that.msgs)
            case (TrivialError(_, _, _, u1, es1), TrivialError(_, _, _, u2, es2)) =>
                val u = (u1, u2) match {
                    case (Some(u1), Some(u2)) => Some(ErrorItem.higherPriority(u1, u2))
                    case _ => u1.orElse(u2)
                }
                TrivialError(offset, line, col, u, es1 union es2)
        }
    }

    def withHints(hints: Iterable[Hint]): ParseError = this match {
        case err: TrivialError => err.copy(expecteds = hints.foldLeft(err.expecteds)((es, h) => es union h.hint))
        case _ => this
    }
}
case class TrivialError(offset: Int, line: Int, col: Int, unexpected: Option[ErrorItem], expecteds: Set[ErrorItem]) extends ParseError
case class FailError(offset: Int, line: Int, col: Int, msgs: Set[String]) extends ParseError

object ParseError {
    def unexpected(msg: String, offset: Int, line: Int, col: Int) = TrivialError(offset, line, col, Some(Desc(msg)), Set.empty)
    def fail(msg: String, offset: Int, line: Int, col: Int) = FailError(offset, line, col, Set(msg))
}

sealed trait ErrorItem {
    val msg: String
}
object ErrorItem {
    def higherPriority(e1: ErrorItem, e2: ErrorItem): ErrorItem = (e1, e2) match {
        case (EndOfInput, _) => EndOfInput
        case (_, EndOfInput) => EndOfInput
        case (e: Desc, _) => e
        case (_, e: Desc) => e
        case (Raw(r1), Raw(r2)) => if (r1.length >= r2.length) e1 else e2
    }
}
case class Raw(cs: String) extends ErrorItem {
    override val msg = "\"" + cs + "\""
}
object Raw {
    def apply(c: Char): Raw = new Raw(s"$c")
}
case class Desc(msg: String) extends ErrorItem
case object EndOfInput extends ErrorItem {
    override val msg = "end of input"
}

final class Hint(val hint: Set[ErrorItem]) extends AnyVal {
    override def toString: String = hint.toString
}