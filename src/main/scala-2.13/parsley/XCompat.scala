package parsley

import scala.collection.mutable

private[parsley] object XCompat {
  def refl[A]: A =:= A = <:<.refl
}
