/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.errors

import parsley.XAssert._
import parsley.errors, errors.{ErrorBuilder, Token, TokenSpan}

private [internal] sealed abstract class ErrorItem
private [internal] sealed trait UnexpectItem extends ErrorItem {
    def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan)
    def higherPriority(other: UnexpectItem): Boolean
    protected [errors] def lowerThanRaw(other: UnexpectRaw): Boolean
    protected [errors] def lowerThanDesc(other: UnexpectDesc): Boolean
}
private [internal] sealed trait ExpectItem extends ErrorItem {
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item
    def higherPriority(other: ExpectItem): Boolean
    protected [errors] def lowerThanRaw(other: ExpectRaw): Boolean
    protected [errors] def lowerThanDesc: Boolean
}

private [internal] final case class UnexpectRaw(cs: IndexedSeq[Char], amountOfInputParserWanted: Int) extends UnexpectItem {
    assert(cs.nonEmpty, "we promise that unexpectedToken never receives empty input")
    def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan) = {
        builder.unexpectedToken(cs, amountOfInputParserWanted, lexicalError) match {
            case t@Token.Raw(tok) => (builder.raw(tok), t.span)
            case Token.Named(name, span) => (builder.named(name), span)
        }
    }
    override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanRaw(this)
    override def lowerThanRaw(other: UnexpectRaw): Boolean = this.amountOfInputParserWanted < other.amountOfInputParserWanted
    override def lowerThanDesc(other: UnexpectDesc): Boolean = true
}

private [internal] final case class ExpectRaw(cs: String) extends ExpectItem {
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.raw(cs)
    override def higherPriority(other: ExpectItem): Boolean = other.lowerThanRaw(this)
    override def lowerThanRaw(other: ExpectRaw): Boolean = this.cs.length < other.cs.length
    override def lowerThanDesc: Boolean = true
}
private [internal] object ExpectRaw {
    def apply(c: Char): ExpectRaw = new ExpectRaw(s"$c")
}
private [internal] final case class ExpectDesc(msg: String) extends ExpectItem {
    assert(msg.nonEmpty, "Desc cannot contain empty things!")
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.named(msg)
    override def higherPriority(other: ExpectItem): Boolean = other.lowerThanDesc
    override def lowerThanRaw(other: ExpectRaw): Boolean = false
    override def lowerThanDesc: Boolean = true
}
private [internal] final case class UnexpectDesc(msg: String, width: Int) extends UnexpectItem {
    assert(msg.nonEmpty, "Desc cannot contain empty things!")
    // FIXME: When this is formatted, the width should really be normalised to the number of code points... this information is not readily available
    def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan) = (builder.named(msg), TokenSpan.Width(width))
    override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanDesc(this)
    override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    override def lowerThanDesc(other: UnexpectDesc): Boolean = this.width < other.width
}
private [internal] case object EndOfInput extends UnexpectItem with ExpectItem {
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.endOfInput
    def formatUnexpect(lexicalError: Boolean)(implicit builder: ErrorBuilder[_]): (builder.Item, TokenSpan) = (builder.endOfInput, TokenSpan.Width(1))
    override def higherPriority(other: ExpectItem): Boolean = true
    override def higherPriority(other: UnexpectItem): Boolean = true
    override def lowerThanRaw(other: ExpectRaw): Boolean = false
    override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    override def lowerThanDesc: Boolean = false
    override def lowerThanDesc(other: UnexpectDesc): Boolean = false
}
