/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.collection.mutable
import scala.language.higherKinds

private [parsley] object XCompat {
    def applyWrap[A, B](f: A => B)(p: Parsley[A]): Parsley[B] = f match {
        case refl: (A <:< B) => refl.substituteParsley(p)
        case refl: (A =:= B) => refl.substituteParsley(p)
        case wrap => p.map(wrap)
    }

    private implicit class SubtitutionEq[A, B](val ev: A =:= B) extends AnyVal {
        def substituteCo[F[_]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
        def substituteParsley(p: Parsley[A]): Parsley[B] = substituteCo(p)
    }

    implicit class SubtitutionSub[A, B](val ev: A <:< B) extends AnyVal {
        def substituteCo[F[_]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
        def substituteParsley(p: Parsley[A]): Parsley[B] = substituteCo(p)
    }

    implicit class MapValuesInPlace[K, V](val m: mutable.Map[K, V]) extends AnyVal {
        def mapValuesInPlaceCompat(f: (K, V) => V): mutable.Map[K, V] = m.transform(f)
    }
}
