/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.collection.mutable

private [parsley] object XCompat {
    def applyWrap[A, B](f: A => B)(p: Parsley[A]): Parsley[B] = f match {
        case refl: (A <:< B) => refl.substituteParsley(p)
        case wrap            => p.map(wrap)
    }

    implicit class SubtitutionSub[A, B](ev: A <:< B) {
        def substituteParsley(p: Parsley[A]): Parsley[B] = ev.substituteCo(p)
    }

    implicit class MapValuesInPlace[K, V](m: mutable.Map[K, V]) {
        def mapValuesInPlaceCompat(f: (K, V) => V): mutable.Map[K, V] = m.mapValuesInPlace(f)
    }
}
