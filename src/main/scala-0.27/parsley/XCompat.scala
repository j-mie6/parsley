package parsley

import scala.collection.mutable

private[parsley] object XCompat {
  def refl[A]: A =:= A = <:<.refl

  def substituteCo[F[_], A, B](fa: F[A])(implicit ev: A =:= B): F[B] =
    ev.substituteCo[F](fa)

  def mapValuesInPlace[A, B](m: mutable.Map[A, B])(f: (A, B) => B): m.type =
    m.mapValuesInPlace(f)
}
