package parsley

import parsley.implicits

import scala.language.implicitConversions

/**
  * Provides implicit conversions and lifts for different values and parsers.
  */
@deprecated("This object will be removed in Parsley 3.0, use `parsley.implicits` instead", "v2.1.0")
object Implicits
{
    // $COVERAGE-OFF$
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.implicits.voidImplicitly` instead", "v2.1.0")
    @inline implicit def voidImplicitly[P](p: P)(implicit con: P => Parsley[_]): Parsley[Unit] = implicits.voidImplicitly(p)
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.implicits.stringLift` instead", "v2.1.0")
    @inline implicit def stringLift(str: String): Parsley[String] = implicits.stringLift(str)
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.implicits.charLift` instead", "v2.1.0")
    @inline implicit def charLift(c: Char): Parsley[Char] = implicits.charLift(c)
    // $COVERAGE-ON$
}
