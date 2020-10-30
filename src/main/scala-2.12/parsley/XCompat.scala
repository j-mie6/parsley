package parsley

import scala.collection.mutable
import scala.language.higherKinds

private[parsley] object XCompat {
  def refl[A]: A =:= A = implicitly[A =:= A]

  implicit class Subtitution[A, B](ev: A =:= B) {
    def substituteCo[F[_]](fa: F[A]) = fa.asInstanceOf[F[B]]
  }

  def mapValuesInPlace[A, B](m: mutable.Map[A, B])(f: (A, B) => B): m.type =
    m.transform(f)
}
