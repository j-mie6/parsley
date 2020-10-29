package parsley

import scala.collection.mutable

private[parsley] object XCompat {
  def refl[A]: A =:= A = implicitly[A =:= A]

  def substituteCo[F[_], A, B](fa: F[A])(implicit ev: A =:= B): F[B] =
    fa.asInstanceOf[F[B]]

  def mapValuesInPlace[A, B](m: mutable.Map[A, B])(f: (A, B) => B): m.type =
    m.transform(f)
}
