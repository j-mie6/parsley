package parsley

import scala.collection.mutable
import scala.language.higherKinds

private[parsley] object XCompat {
  def refl[A]: A =:= A = implicitly[A =:= A]

  implicit class Subtitution[A, B](ev: A =:= B) {
    def substituteCo[F[_]](fa: F[A]) = fa.asInstanceOf[F[B]]
  }

  implicit class MapValuesInPlace[K, V](m: mutable.Map[K, V]) {
    def mapValuesInPlace(f: (K, V) => V): mutable.Map[K, V] = m.transform(f)
  }
}
