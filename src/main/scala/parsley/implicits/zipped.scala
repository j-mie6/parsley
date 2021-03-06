package parsley.implicits

import parsley.Parsley
import parsley.lift.{lift1, lift2, lift3, lift4, lift5, lift6, lift7, lift8, lift9, lift10, lift11,
                     lift12, lift13, lift14, lift15, lift16, lift17, lift18, lift19, lift20, lift21, lift22}

import scala.language.implicitConversions

/**
  * Provides implicit conversions and lifts for different values and parsers.
  * @since 2.2.0
  */
object zipped
{
    // $COVERAGE-OFF$
    // Forgive me, for I have sinned: courtesy of Andrei Gramescu and George Stacey, who wished to forever have their names stamped on this (useful) abomination
    implicit class Map2[T1, T2](private val t: (Parsley[T1], Parsley[T2])) extends AnyVal {
        def map[R](f: (T1, T2) => R): Parsley[R] = lift2(f, t._1, t._2)
    }
    implicit class Map3[T1, T2, T3](private val t: (Parsley[T1], Parsley[T2], Parsley[T3])) extends AnyVal {
        def map[R](f: (T1, T2, T3) => R): Parsley[R] = lift3(f, t._1, t._2, t._3)
    }
    implicit class Map4[T1, T2, T3, T4](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4) => R): Parsley[R] = lift4(f, t._1, t._2, t._3, t._4)
    }
    implicit class Map5[T1, T2, T3, T4, T5](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5) => R): Parsley[R] = lift5(f, t._1, t._2, t._3, t._4, t._5)
    }
    implicit class Map6[T1, T2, T3, T4, T5, T6](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6) => R): Parsley[R] = lift6(f, t._1, t._2, t._3, t._4, t._5, t._6)
    }
    implicit class Map7[T1, T2, T3, T4, T5, T6, T7]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7) => R): Parsley[R] = lift7(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7)
    }
    implicit class Map8[T1, T2, T3, T4, T5, T6, T7, T8]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): Parsley[R] = lift8(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
    }
    implicit class Map9[T1, T2, T3, T4, T5, T6, T7, T8, T9]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): Parsley[R] = lift9(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    }
    implicit class Map10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R): Parsley[R] = lift10(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
    }
    implicit class Map11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R): Parsley[R] =
            lift11(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
    }
    implicit class Map12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R): Parsley[R] =
            lift12(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
    }
    implicit class Map13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R): Parsley[R] =
            lift13(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
    }
    implicit class Map14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R): Parsley[R] =
            lift14(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
    }
    implicit class Map15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R): Parsley[R] =
            lift15(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
    }
    implicit class Map16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R): Parsley[R] =
            lift16(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
    }
    implicit class Map17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16],
                         Parsley[T17])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R): Parsley[R] =
            lift17(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
    }
    implicit class Map18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16],
                         Parsley[T17], Parsley[T18])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R): Parsley[R] =
            lift18(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
    }
    implicit class Map19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16],
                         Parsley[T17], Parsley[T18], Parsley[T19])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R): Parsley[R] =
            lift19(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
    }
    implicit class Map20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16],
                         Parsley[T17], Parsley[T18], Parsley[T19], Parsley[T20])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R): Parsley[R] =
            lift20(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
    }
    implicit class Map21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16],
                         Parsley[T17], Parsley[T18], Parsley[T19], Parsley[T20], Parsley[T21])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R): Parsley[R] =
            lift21(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
    }
    implicit class Map22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8],
                         Parsley[T9], Parsley[T10], Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16],
                         Parsley[T17], Parsley[T18], Parsley[T19], Parsley[T20], Parsley[T21], Parsley[T22])) extends AnyVal {
        def map[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R): Parsley[R] =
            lift22(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11,
                   t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
    }
    // $COVERAGE-ON$
}