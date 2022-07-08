/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [parsley] final class <*>[A, B](pf: LazyParsley[A => B], px: =>LazyParsley[A]) extends Binary[A => B, A, B](pf, px) {
    override def make(pf: StrictParsley[A => B], px: StrictParsley[A]): StrictParsley[B] = new backend.<*>(pf, px)
}

private [parsley] final class >>=[A, B](p: LazyParsley[A], private [>>=] val f: A => LazyParsley[B]) extends Unary[A, B](p) {
    override def make(p: StrictParsley[A]): StrictParsley[B] = new backend.>>=(p, f)
}

private [parsley] final class *>[A](_p: LazyParsley[_], _q: =>LazyParsley[A]) extends Binary[Any, A, A](_p, _q) {
    override def make(p: StrictParsley[Any], q: StrictParsley[A]): StrictParsley[A] = backend.*>(p, q)
}
private [parsley] final class <*[A](_p: LazyParsley[A], _q: =>LazyParsley[_]) extends Binary[A, Any, A](_p, _q) {
    override def make(p: StrictParsley[A], q: StrictParsley[Any]): StrictParsley[A] = backend.<*(p, q)
}
