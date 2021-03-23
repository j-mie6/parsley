package parsley

import scala.collection.mutable

private[parsley] object XCompat {
    def refl[A]: A =:= A = <:<.refl
    def applyWrap[A, B](f: A => B)(p: Parsley[A]): Parsley[B] = f match {
        case refl: (A <:< B) => refl.substituteCo[Parsley](p)
        case wrap => p.map(wrap)
    }
}
