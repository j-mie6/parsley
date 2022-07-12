/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.errors

import parsley.errors.ErrorBuilder

private [internal] sealed abstract class ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item
    def higherPriority(other: ErrorItem): Boolean
    protected [errors] def lowerThanRaw(other: Raw): Boolean
    protected [errors] def lowerThanDesc: Boolean
}
/*
private [internal] object ErrorItem {
    def higherPriority(e1: ErrorItem, e2: ErrorItem): ErrorItem = (e1, e2) match {
        case (EndOfInput, _) => EndOfInput
        case (_, EndOfInput) => EndOfInput
        case (e: Desc, _) => e
        case (_, e: Desc) => e
        case (Raw(r1), Raw(r2)) => if (r1.length >= r2.length) e1 else e2
    }
}*/

private [internal] final case class Raw(cs: String) extends ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.raw(cs)
    override def higherPriority(other: ErrorItem): Boolean = other.lowerThanRaw(this)
    override def lowerThanRaw(other: Raw): Boolean = this.cs.length < other.cs.length
    override def lowerThanDesc: Boolean = true
}
private [internal] object Raw {
    def apply(c: Char): Raw = new Raw(s"$c")
}
private [internal] final case class Desc(msg: String) extends ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.named(msg)
    override def higherPriority(other: ErrorItem): Boolean = other.lowerThanDesc
    override def lowerThanRaw(other: Raw): Boolean = false
    override def lowerThanDesc: Boolean = true
}
private [internal] case object EndOfInput extends ErrorItem {
    def format(implicit builder: ErrorBuilder[_]): builder.Item = builder.endOfInput
    override def higherPriority(other: ErrorItem): Boolean = true
    override def lowerThanRaw(other: Raw): Boolean = false
    override def lowerThanDesc: Boolean = false
}
