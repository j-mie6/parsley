/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class ErrorLabel[A](p: LazyParsley[A], private [ErrorLabel] val label: String) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorLabel(p, label)
}
private [parsley] final class ErrorExplain[A](p: LazyParsley[A], reason: String) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorExplain(p, reason)
}

private [parsley] final class ErrorAmend[A](p: LazyParsley[A], partial: Boolean) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorAmend(p, partial)
}
private [parsley] final class ErrorEntrench[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorEntrench(p)
}
private [parsley] final class ErrorDislodge[A](n: Int, p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorDislodge(n, p)
}

private [parsley] final class ErrorLexical[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorLexical(p)
}

private [parsley] final class VerifiedError[A](p: LazyParsley[A], msggen: Either[A => Seq[String], Option[A => String]]) extends Unary[A, Nothing](p) {
    override def make(p: StrictParsley[A]): StrictParsley[Nothing] = new backend.VerifiedError(p, msggen)
}
