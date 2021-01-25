package parsley

import scala.collection.mutable

private[parsley] object XCompat {
  def refl[A]: A =:= A = <:<.refl
  def isIdentityWrap[A, B](f: A => B): Boolean = f eq refl
}
