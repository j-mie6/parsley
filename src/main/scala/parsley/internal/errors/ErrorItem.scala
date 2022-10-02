/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.errors

import parsley.XAssert._
import parsley.errors, errors.ErrorBuilder

private [internal] sealed abstract class ErrorItem {
    protected [errors] def lowerThanDesc: Boolean
}

private [internal] sealed trait UnexpectItem extends ErrorItem {
    def formatUnexpect(implicit builder: ErrorBuilder[_]): (builder.Item, errors.TokenSpan)
    def higherPriority(other: UnexpectItem): Boolean
    protected [errors] def lowerThanRaw(other: UnexpectRaw): Boolean
}
private [internal] sealed trait ExpectItem extends ErrorItem {
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item
    def higherPriority(other: ExpectItem): Boolean
    protected [errors] def lowerThanRaw(other: ExpectRaw): Boolean
}

private [internal] final case class UnexpectRaw(cs: Iterable[Char], amountOfInputParserWanted: Int) extends UnexpectItem {
    def formatUnexpect(implicit builder: ErrorBuilder[_]): (builder.Item, errors.TokenSpan) = {
        builder.unexpectedToken(cs, amountOfInputParserWanted, false) match {
            case t@errors.Raw(tok) => (builder.raw(tok), t.span)
            case errors.Named(name, span) => (builder.named(name), span)
            case t@errors.EndOfInput => (builder.endOfInput, t.span)
        }
    }
    override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanRaw(this)
    override def lowerThanRaw(other: UnexpectRaw): Boolean = this.amountOfInputParserWanted < other.amountOfInputParserWanted
    override def lowerThanDesc: Boolean = true
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
private [internal] final case class Desc(msg: String) extends UnexpectItem with ExpectItem {
    assume(msg.nonEmpty, "Desc cannot contain empty things!")
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.named(msg)
    def formatUnexpect(implicit builder: ErrorBuilder[_]): (builder.Item, errors.TokenSpan) = (builder.named(msg), errors.Width(1))
    override def higherPriority(other: ExpectItem): Boolean = other.lowerThanDesc
    override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanDesc
    override def lowerThanRaw(other: ExpectRaw): Boolean = false
    override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    override def lowerThanDesc: Boolean = true
}
private [internal] case object EndOfInput extends UnexpectItem with ExpectItem {
    def formatExpect(implicit builder: ErrorBuilder[_]): builder.Item = builder.endOfInput
    def formatUnexpect(implicit builder: ErrorBuilder[_]): (builder.Item, errors.TokenSpan) = (builder.endOfInput, errors.Width(1))
    override def higherPriority(other: ExpectItem): Boolean = true
    override def higherPriority(other: UnexpectItem): Boolean = true
    override def lowerThanRaw(other: ExpectRaw): Boolean = false
    override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    override def lowerThanDesc: Boolean = false
}
