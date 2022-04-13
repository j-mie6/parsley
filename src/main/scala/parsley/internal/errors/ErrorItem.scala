package parsley.internal.errors

import parsley.errors.ErrorBuilder

private [internal] sealed trait ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item
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
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.raw(cs)
}
private [internal] object Raw {
    def apply(c: Char): Raw = new Raw(s"$c")
}
private [internal] case class Desc(msg: String) extends ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.named(msg)
}
private [internal] case object EndOfInput extends ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.endOfInput
}
