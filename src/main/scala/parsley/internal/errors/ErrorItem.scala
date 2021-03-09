package parsley.internal.errors

import Raw.Unprintable
import scala.util.matching.Regex

private [internal] sealed trait ErrorItem {
    val msg: String
}
private [internal] object ErrorItem {
    def higherPriority(e1: ErrorItem, e2: ErrorItem): ErrorItem = (e1, e2) match {
        case (EndOfInput, _) => EndOfInput
        case (_, EndOfInput) => EndOfInput
        case (e: Desc, _) => e
        case (_, e: Desc) => e
        case (Raw(r1), Raw(r2)) => if (r1.length >= r2.length) e1 else e2
    }
}
private [internal] case class Raw(cs: String) extends ErrorItem {
    // This could be marked threadUnsafe in Scala 3?
    override lazy val msg = cs match {
        case "\n"            => "newline"
        case "\t"            => "tab"
        case " "             => "space"
        case Unprintable(up) => f"unprintable character (\\u${up.head.toInt}%04X)"
        // Do we want this only in unexpecteds?
        case cs              => "\"" + cs.takeWhile(c => c != '\n' && c != ' ') + "\""
    }
}
private [internal] object Raw {
    val Unprintable: Regex = "(\\p{C})".r
    def apply(c: Char): Raw = new Raw(s"$c")
}
private [internal] case class Desc(msg: String) extends ErrorItem
private [internal] case object EndOfInput extends ErrorItem {
    override val msg = "end of input"
}