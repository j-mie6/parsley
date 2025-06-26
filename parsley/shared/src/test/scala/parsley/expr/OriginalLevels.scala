/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.Parsley

sealed abstract class OriginalPrec[+A] private [expr] {
    final def :+[Aʹ >: A, B](ops: OriginalOps[Aʹ, B]): OriginalPrec[B] = new OriginalLevel(this, ops)
    final def +:[Aʹ >: A, B](ops: OriginalOps[Aʹ, B]): OriginalPrec[B] = new OriginalLevel(this, ops)
}
private [expr] case class OriginalLevel[A, B](lvls: OriginalPrec[A], ops: OriginalOps[A, B]) extends OriginalPrec[B]

case class OriginalAtoms[+A](atom0: Parsley[A], atoms: Parsley[A]*) extends OriginalPrec[A]