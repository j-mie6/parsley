/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.debug.Breakpoint
import parsley.errors.ErrorBuilder
import parsley.registers.Reg

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Attempt[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Attempt(p)
}
private [parsley] final class Look[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Look(p)
}
private [parsley] final class NotFollowedBy[A](p: LazyParsley[A]) extends Unary[A, Unit](p) {
    override def make(p: StrictParsley[A]): StrictParsley[Unit] = new backend.NotFollowedBy(p)
}
private [parsley] final class Put[S](val reg: Reg[S], _p: LazyParsley[S]) extends Unary[S, Unit](_p) with UsesRegister {
    override def make(p: StrictParsley[S]): StrictParsley[Unit] = new backend.Put(reg, p)
}
private [parsley] final class NewReg[S, A](val reg: Reg[S], init: LazyParsley[S], body: =>LazyParsley[A])
    extends Binary[S, A, A](init, body) with UsesRegister {
    override def make(init: StrictParsley[S], body: StrictParsley[A]): StrictParsley[A] = new backend.NewReg(reg, init, body)
}
// $COVERAGE-OFF$
private [parsley] final class Debug[A](p: LazyParsley[A], name: String, ascii: Boolean, break: Breakpoint) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Debug(p, name, ascii, break)
}
private [parsley] final class DebugError[A](p: LazyParsley[A], name: String, ascii: Boolean, errBuilder: ErrorBuilder[_]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.DebugError(p, name, ascii, errBuilder)
}
// $COVERAGE-ON$
