/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.collection.mutable
import scala.language.higherKinds

private [parsley] object XCompat {
    def applyWrap[A, B](f: A => B)(p: Parsley[A]): Parsley[B] = f match {
        case refl: (A <:< B) => refl.substituteCo[Parsley](p)
        case refl: (A =:= B) => refl.substituteCo[Parsley](p)
        case wrap => p.map(wrap)
    }

    private implicit class SubtitutionEq[A, B](ev: A =:= B) {
        def substituteCo[F[_]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
    }

    implicit class SubtitutionSub[A, B](ev: A <:< B) {
        def substituteCo[F[_]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
    }

    implicit class MapValuesInPlace[K, V](m: mutable.Map[K, V]) {
        def mapValuesInPlace(f: (K, V) => V): mutable.Map[K, V] = m.transform(f)
    }

    def codePoints(str: String): Iterator[Int] = new Iterator[Int] {
        var idx = 0
        def hasNext: Boolean = idx < str.length
        def next(): Int = {
            val c = str.codePointAt(idx)
            idx += Character.charCount(c)
            c
        }
    }
}
