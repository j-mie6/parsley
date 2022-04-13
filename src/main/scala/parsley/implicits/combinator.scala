package parsley.implicits

import scala.language.implicitConversions

import parsley.Parsley
import parsley.Parsley.void

/**
  * Provides implicit conversions for parsers into unit parsers, and other implicits involving combinators.
  * @since 3.0.0
  */
object combinator {
    // $COVERAGE-OFF$
    /** Drops the result of a parser when required by another combinator */
    @inline implicit def voidImplicitly[P](p: P)(implicit con: P => Parsley[_]): Parsley[Unit] = void(con(p))
    // $COVERAGE-ON$
}
