/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.errors.{Desc, ErrorItem, Raw}

private [parsley] final class <|>[A](p: LazyParsley[A], q: =>LazyParsley[A]) extends Binary[A, A, A](p, q) {
    override def make(p: StrictParsley[A], q: StrictParsley[A]): StrictParsley[A] = backend.<|>(p, q)
}
