package parsley.implicits

import parsley.Parsley
import parsley.character.{char, string}

import scala.language.implicitConversions

/**
  * Provides implicit conversions for characters and strings.
  * @since 3.0.0
  */
object character
{
    // $COVERAGE-OFF$
    /** Converts a string literal into a parser that reads that string */
    @inline implicit def stringLift(str: String): Parsley[String] = string(str)
    /** Converts a char literal into a parser that reads that character */
    @inline implicit def charLift(c: Char): Parsley[Char] = char(c)
    // $COVERAGE-ON$
}