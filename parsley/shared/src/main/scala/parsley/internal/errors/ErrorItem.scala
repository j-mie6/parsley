/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.errors

import parsley.XAssert._
import parsley.errors, errors.{ErrorBuilder, Token, TokenSpan}

private [internal] sealed abstract class ErrorItem
private [internal] sealed trait UnexpectItem extends ErrorItem {
    private [internal] def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan)
    private [internal] def higherPriority(other: UnexpectItem): Boolean
    protected [errors] def lowerThanRaw(other: UnexpectRaw): Boolean
    protected [errors] def lowerThanDesc(other: UnexpectDesc): Boolean
}
private [parsley] sealed trait ExpectItem extends ErrorItem {
    private [internal] def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item
}

private [internal] final case class UnexpectRaw(cs: Iterable[Char], amountOfInputParserWanted: Int) extends UnexpectItem {
    assert(cs.nonEmpty, "we promise that unexpectedToken never receives empty input")
    private [internal] def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan) = {
        builder.unexpectedToken(cs, amountOfInputParserWanted, lexicalError) match {
            case t@Token.Raw(tok) => (builder.raw(tok), t.span)
            case Token.Named(name, span) => (builder.named(name), span)
        }
    }
    private [internal] override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanRaw(this)
    protected [errors] override def lowerThanRaw(other: UnexpectRaw): Boolean = this.amountOfInputParserWanted < other.amountOfInputParserWanted
    protected [errors] override def lowerThanDesc(other: UnexpectDesc): Boolean = true
}

private [parsley] final case class ExpectRaw(cs: String) extends ExpectItem {
    private [internal] def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.raw(cs)
}
private [parsley] object ExpectRaw {
    def apply(c: Char): ExpectRaw = new ExpectRaw(s"$c")
}
private [parsley] final case class ExpectDesc(msg: String) extends ExpectItem {
    assert(msg.nonEmpty, "Desc cannot contain empty things!")
    private [internal] def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.named(msg)
}

private [internal] final case class UnexpectDesc(msg: String, width: Int) extends UnexpectItem {
    assert(msg.nonEmpty, "Desc cannot contain empty things!")
    // FIXME: When this is formatted, the width should really be normalised to the number of code points... this information is not readily available
    private [internal] def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan) = (builder.named(msg), TokenSpan.Width(width))
    private [internal] override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanDesc(this)
    protected [errors] override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    protected [errors] override def lowerThanDesc(other: UnexpectDesc): Boolean = this.width < other.width
}
private [internal] case object EndOfInput extends UnexpectItem with ExpectItem {
    private [internal] def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.endOfInput
    private [internal] def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan) = (builder.endOfInput, TokenSpan.Width(1))
    private [internal] override def higherPriority(other: UnexpectItem): Boolean = true
    protected [errors] override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    protected [errors] override def lowerThanDesc(other: UnexpectDesc): Boolean = false
}
