/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.predicate

import scala.collection.immutable.NumericRange

/** This object provides implicit functionality for constructing `CharPredicate` values.
  * @since 4.1.0
  */
object implicits {
    /** Implicit conversions to make `Basic` values.
      * @since 4.1.0
      */
    object Basic {
        // $COVERAGE-OFF$
        /** Lifts a regular character predicate.
          * @since 4.1.0
          */
        implicit def funToBasic(pred: Char => Boolean): CharPredicate = parsley.token.predicate.Basic(pred)
        /** Constructs a predicate for the specific given character.
          * @since 4.1.0
          */
        implicit def charToBasic(c: Char): CharPredicate = parsley.token.predicate.Basic(_ == c)
        /** Constructs a predicate for anything in a range of specific characters.
          * @since 4.1.0
          */
        implicit def rangeToBasic(cs: NumericRange[Char]): CharPredicate = parsley.token.predicate.Basic(cs.contains)
        // $COVERAGE-ON$
    }

    /** Implicit conversions to make `Unicode` values.
      * @since 4.1.0
      */
    object Unicode {
        // $COVERAGE-OFF$
        /** Lifts a regular full-width character predicate.
          * @since 4.1.0
          */
        implicit def funToUnicode(pred: Int => Boolean): CharPredicate = parsley.token.predicate.Unicode(pred)
        /** Lifts a regular character predicate.
          * @since 4.1.0
          */
        implicit def charFunToUnicode(pred: Char => Boolean): CharPredicate = parsley.token.predicate.Unicode(c => c.isValidChar && pred(c.toChar))
        /** Constructs a predicate for the specific given character.
          * @since 4.1.0
          */
        implicit def charToUnicode(c: Char): CharPredicate = parsley.token.predicate.Unicode(_ == c.toInt)
        /** Constructs a predicate for the specific given unicode codepoint.
          * @since 4.1.0
          */
        implicit def intToUnicode(c: Int): CharPredicate = parsley.token.predicate.Unicode(_ == c)
        /** Constructs a predicate for anything in a range of specific characters.
          * @since 4.1.0
          */
        implicit def charRangeToUnicode(cs: NumericRange[Char]): CharPredicate = parsley.token.predicate.Unicode(cs.contains)
        /** Constructs a predicate for anything in a range of specific unicode codepoints.
          * @since 4.1.0
          */
        implicit def intRangeToUnicode(cs: NumericRange[Int]): CharPredicate = parsley.token.predicate.Unicode(cs.contains)
        /** Constructs a predicate for anything in a range of specific unicode codepoints.
          * @since 4.1.0
          */
        implicit def rangeToUnicode(cs: Range): CharPredicate = parsley.token.predicate.Unicode(cs.contains)
        // $COVERAGE-ON$
    }
}
