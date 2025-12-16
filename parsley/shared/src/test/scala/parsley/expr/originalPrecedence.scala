/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.Parsley
import parsley.combinator.choice

object originalPrecedence {
    def apply[A](atom0: Parsley[A], atoms: Parsley[A]*)(lvlTightest: OriginalOps[A, A], lvls: OriginalOps[A, A]*): Parsley[A] = {
        apply(lvls.foldLeft[OriginalPrec[A]](new OriginalLevel(OriginalAtoms(atom0, atoms: _*), lvlTightest))(new OriginalLevel(_, _)))
    }

    def apply[A](lvlWeakest: OriginalOps[A, A], lvls: OriginalOps[A, A]*)(atom0: Parsley[A], atoms: Parsley[A]*): Parsley[A] = {
        val (lvlTightest +: lvls_) = (lvlWeakest +: lvls).reverse: @unchecked
        apply(atom0, atoms: _*)(lvlTightest, lvls_ : _*)
    }

    def apply[A](table: OriginalPrec[A]): Parsley[A] = crushLevels(table)

    private def crushLevels[A](lvls: OriginalPrec[A]): Parsley[A] = lvls match {
        case OriginalAtoms(atom0, atoms @ _*) => choice((atom0 +: atoms): _*)
        case OriginalLevel(lvls, ops) => ops.chain(crushLevels(lvls))
    }
}