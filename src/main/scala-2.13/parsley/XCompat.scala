package parsley

import scala.collection.mutable

private[parsley] object XCompat {
  def refl[A]: A =:= A = <:<.refl

  def mapValuesInPlace[A, B](m: mutable.Map[A, B])(f: (A, B) => B): m.type =
    m.mapValuesInPlace(f)
}
