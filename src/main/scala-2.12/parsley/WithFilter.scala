package parsley

import Parsley.LazyParsley

object Parsley212 {
    /** This class enables any combinators and functionality that is only supported to provide back-compat with Scala 2.12.
      *  It does not appear in any 2.13+ releases
      * @since 3.0
      */
    implicit final class LazyParsley212[P, +A](p: =>P)(implicit con: P => Parsley[A]) {
        // $COVERAGE-OFF$
        /**
          * This is an alias for `p.filter(pred)`. It is needed to support for-comprehension syntax with `if`s in Scala 2.12.
          */
        def withFilter(pred: A => Boolean): Parsley[A] = p.filter(pred)
        // $COVERAGE-ON$
    }
}