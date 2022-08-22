/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.errors

import parsley.errors.ErrorBuilder

private [internal] sealed abstract class ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item
    protected [errors] def lowerThanDesc: Boolean
}

private [internal] sealed trait UnexpectItem extends ErrorItem {
    def higherPriority(other: UnexpectItem): Boolean
    protected [errors] def lowerThanRaw(other: UnexpectRaw): Boolean
}
private [internal] sealed trait ExpectItem extends ErrorItem {
    def higherPriority(other: ExpectItem): Boolean
    protected [errors] def lowerThanRaw(other: ExpectRaw): Boolean
}

private [internal] final case class UnexpectRaw(cs: Iterable[Char], knownConsumedWidth: Int) extends UnexpectItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.raw(builder.unexpectedToken(cs, knownConsumedWidth))
    override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanRaw(this)
    override def lowerThanRaw(other: UnexpectRaw): Boolean = this.knownConsumedWidth < other.knownConsumedWidth
    override def lowerThanDesc: Boolean = true
}

private [internal] final case class ExpectRaw(cs: String) extends ExpectItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.raw(cs)
    override def higherPriority(other: ExpectItem): Boolean = other.lowerThanRaw(this)
    override def lowerThanRaw(other: ExpectRaw): Boolean = this.cs.length < other.cs.length
    override def lowerThanDesc: Boolean = true
}
private [internal] object ExpectRaw {
    def apply(c: Char): ExpectRaw = new ExpectRaw(s"$c")
}
private [internal] final case class Desc(msg: String) extends UnexpectItem with ExpectItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.named(msg)
    override def higherPriority(other: ExpectItem): Boolean = other.lowerThanDesc
    override def higherPriority(other: UnexpectItem): Boolean = other.lowerThanDesc
    override def lowerThanRaw(other: ExpectRaw): Boolean = false
    override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    override def lowerThanDesc: Boolean = true
}
private [internal] case object EndOfInput extends UnexpectItem with ExpectItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.endOfInput
    override def higherPriority(other: ExpectItem): Boolean = true
    override def higherPriority(other: UnexpectItem): Boolean = true
    override def lowerThanRaw(other: ExpectRaw): Boolean = false
    override def lowerThanRaw(other: UnexpectRaw): Boolean = false
    override def lowerThanDesc: Boolean = false
}
