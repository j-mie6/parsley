package parsley.implicits

import parsley.Parsley
import parsley.lift.{lift1, lift2, lift3, lift4, lift5, lift6, lift7, lift8, lift9, lift10, lift11,
                     lift12, lift13, lift14, lift15, lift16, lift17, lift18, lift19, lift20, lift21, lift22}

import scala.language.implicitConversions

/**
  * Provides an alterative to the `f.lift(x, y)` syntax that is instead `(x, y).zipped(f)`. This is prefered when type inferences fails. Also enables a
  * parameterless `zipped` method, to pair an arbitrary number of parsers such that `(p, q).zipped = p.zip(q)`
  *
  * Warning: these methods are *not* lazy like the `lift` syntax or `liftN` functions!
  * @since 3.0.0
  */
object zipped
{
    // $COVERAGE-OFF$
    // Forgive me, for I have sinned: courtesy of Andrei Gramescu and George Stacey, who wished to forever have their names stamped on this (useful) abomination
    implicit final class Zipped2[T1, T2](private val t: (Parsley[T1], Parsley[T2])) extends AnyVal {
        def zipped[R](f: (T1, T2) => R): Parsley[R] = lift2(f, t._1, t._2)
        def zipped: Parsley[(T1, T2)] = this.zipped((_, _))
    }
    implicit final class Zipped3[T1, T2, T3](private val t: (Parsley[T1], Parsley[T2], Parsley[T3])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3) => R): Parsley[R] = lift3(f, t._1, t._2, t._3)
        def zipped: Parsley[(T1, T2, T3)] = this.zipped((_, _, _))
    }
    implicit final class Zipped4[T1, T2, T3, T4](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4) => R): Parsley[R] = lift4(f, t._1, t._2, t._3, t._4)
        def zipped: Parsley[(T1, T2, T3, T4)] = this.zipped((_, _, _, _))
    }
    implicit final class Zipped5[T1, T2, T3, T4, T5](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5) => R): Parsley[R] = lift5(f, t._1, t._2, t._3, t._4, t._5)
        def zipped: Parsley[(T1, T2, T3, T4, T5)] = this.zipped((_, _, _, _, _))
    }
    implicit final class Zipped6[T1, T2, T3, T4, T5, T6]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6) => R): Parsley[R] = lift6(f, t._1, t._2, t._3, t._4, t._5, t._6)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6)] = this.zipped((_, _, _, _, _, _))
    }
    implicit final class Zipped7[T1, T2, T3, T4, T5, T6, T7]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7) => R): Parsley[R] = lift7(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7)] = this.zipped((_, _, _, _, _, _, _))
    }
    implicit final class Zipped8[T1, T2, T3, T4, T5, T6, T7, T8]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): Parsley[R] = lift8(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8)] = this.zipped((_, _, _, _, _, _, _, _))
    }
    implicit final class Zipped9[T1, T2, T3, T4, T5, T6, T7, T8, T9]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): Parsley[R] = lift9(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = this.zipped((_, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9],
                         Parsley[T10])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R): Parsley[R] = lift10(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = this.zipped((_, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R): Parsley[R] =
            lift11(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R): Parsley[R] =
            lift12(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R): Parsley[R] =
            lift13(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R): Parsley[R] =
            lift14(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R): Parsley[R] =
            lift15(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R): Parsley[R] =
            lift16(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R): Parsley[R] =
            lift17(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R): Parsley[R] =
            lift18(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18],
                         Parsley[T19])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R): Parsley[R] =
            lift19(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19],
                         Parsley[T20])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R): Parsley[R] =
            lift20(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19],
                         Parsley[T20], Parsley[T21])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R): Parsley[R] =
            lift21(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit final class Zipped22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19],
                         Parsley[T20], Parsley[T21], Parsley[T22])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R): Parsley[R] =
            lift22(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11,
                      t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    // $COVERAGE-ON$
}