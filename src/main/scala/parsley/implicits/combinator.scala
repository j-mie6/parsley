package parsley.implicits

import parsley.Parsley
import parsley.Parsley.void

import scala.language.implicitConversions

/**
  * Provides implicit conversions for parsers into unit parsers, and other implicits involving combinators.
  * @since 3.0.0
  */
object combinator {
    // $COVERAGE-OFF$
    /** Drops the result of a parser when required by another combinator */
    @inline implicit def voidImplicitly[P](p: P)(implicit con: P => Parsley[_]): Parsley[Unit] = void(p)
    // $COVERAGE-ON$
}