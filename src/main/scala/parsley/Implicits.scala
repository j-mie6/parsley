package parsley

import parsley.Char.{char, string}
import parsley.Parsley.void

import scala.language.implicitConversions

/**
  * Provides implicit conversions and lifts for different values and parsers.
  */
object Implicits
{
    @inline implicit def voidImplicitly[P](p: P)(implicit con: P => Parsley[_]): Parsley[Unit] = void(p)
    @inline implicit def stringLift(str: String): Parsley[String] = string(str)
    @inline implicit def charLift(c: Char): Parsley[Char] = char(c)
}
