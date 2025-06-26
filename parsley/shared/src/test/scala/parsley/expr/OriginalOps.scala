/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.Parsley

abstract class OriginalOps[-A, B] {
    private [expr] def chain(p: Parsley[A]): Parsley[B]
}

object OriginalOps {
    def apply[A](fixity: Fixity)(op0: Parsley[fixity.Op[A, A]], ops: Parsley[fixity.Op[A, A]]*): OriginalOps[A, A] = OriginalGOps[A, A](fixity)(op0, ops: _*)
}