package parsley.implicits

import parsley.Parsley
import parsley.lift.{lift1, lift2, lift3, lift4, lift5, lift6, lift7, lift8, lift9, lift10, lift11,
                     lift12, lift13, lift14, lift15, lift16, lift17, lift18, lift19, lift20, lift21, lift22}

import scala.language.implicitConversions

/**
  * Provides an alterative to the `f.lift(x, y)` syntax that is instead `(x, y).zipped(f)`. This is prefered when type inferences fails. Also enables a
  * parameterless `zipped` method, to pair an arbitrary number of parsers such that `(p, q).zipped = p.zip(q)`
  * @since 3.0.0
  */
object zipped
{
    // $COVERAGE-OFF$
    // Forgive me, for I have sinned: courtesy of Andrei Gramescu and George Stacey, who wished to forever have their names stamped on this (useful) abomination
    implicit class Zipped2[T1, T2](private val t: (Parsley[T1], Parsley[T2])) extends AnyVal {
        def zipped[R](f: (T1, T2) => R): Parsley[R] = lift2(f, t._1, t._2)
        def zipped: Parsley[(T1, T2)] = this.zipped((_, _))
    }
    implicit class Zipped3[T1, T2, T3](private val t: (Parsley[T1], Parsley[T2], Parsley[T3])) extends AnyVal {
        def zipped[R](f: (T1, T2, T3) => R): Parsley[R] = lift3(f, t._1, t._2, t._3)
        def zipped: Parsley[(T1, T2, T3)] = this.zipped((_, _, _))
    }
    // Zipped2 and Zipped3 cannot have the same laziness as the others, because of a conflict with the (deprecated) .zipped from scala on tuples.
    implicit class LazyZipped2[T1, T2](t: =>(Parsley[T1], Parsley[T2])) {
        lazy val (p1, p2) = t
        def zippedLazy[R](f: (T1, T2) => R): Parsley[R] = lift2(f, p1, p2)
        def zippedLazy: Parsley[(T1, T2)] = this.zippedLazy((_, _))
    }
    implicit class LazyZipped3[T1, T2, T3](t: =>(Parsley[T1], Parsley[T2], Parsley[T3])) {
        lazy val (p1, p2, p3) = t
        def zippedLazy[R](f: (T1, T2, T3) => R): Parsley[R] = lift3(f, p1, p2, p3)
        def zippedLazy: Parsley[(T1, T2, T3)] = this.zippedLazy((_, _, _))
    }
    implicit class Zipped4[T1, T2, T3, T4](t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4])) {
        lazy val (p1, p2, p3, p4) = t
        def zipped[R](f: (T1, T2, T3, T4) => R): Parsley[R] = lift4(f, p1, p2, p3, p4)
        def zipped: Parsley[(T1, T2, T3, T4)] = this.zipped((_, _, _, _))
    }
    implicit class Zipped5[T1, T2, T3, T4, T5](t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5])) {
        lazy val (p1, p2, p3, p4, p5) = t
        def zipped[R](f: (T1, T2, T3, T4, T5) => R): Parsley[R] = lift5(f, p1, p2, p3, p4, p5)
        def zipped: Parsley[(T1, T2, T3, T4, T5)] = this.zipped((_, _, _, _, _))
    }
    implicit class Zipped6[T1, T2, T3, T4, T5, T6](t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6])) {
        lazy val (p1, p2, p3, p4, p5, p6) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6) => R): Parsley[R] = lift6(f, p1, p2, p3, p4, p5, p6)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6)] = this.zipped((_, _, _, _, _, _))
    }
    implicit class Zipped7[T1, T2, T3, T4, T5, T6, T7](t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7) => R): Parsley[R] = lift7(f, p1, p2, p3, p4, p5, p6, p7)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7)] = this.zipped((_, _, _, _, _, _, _))
    }
    implicit class Zipped8[T1, T2, T3, T4, T5, T6, T7, T8]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): Parsley[R] = lift8(f, p1, p2, p3, p4, p5, p6, p7, p8)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8)] = this.zipped((_, _, _, _, _, _, _, _))
    }
    implicit class Zipped9[T1, T2, T3, T4, T5, T6, T7, T8, T9]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): Parsley[R] = lift9(f, p1, p2, p3, p4, p5, p6, p7, p8, p9)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = this.zipped((_, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R): Parsley[R] = lift10(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = this.zipped((_, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
               Parsley[T11])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R): Parsley[R] = lift11(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R): Parsley[R] = lift12(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R): Parsley[R] =
            lift13(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R): Parsley[R] =
            lift14(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R): Parsley[R] =
            lift15(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R): Parsley[R] =
            lift16(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R): Parsley[R] =
            lift17(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R): Parsley[R] =
            lift18(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R): Parsley[R] =
            lift19(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19], Parsley[T20])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R): Parsley[R] =
            lift20(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19], Parsley[T20], Parsley[T21])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R): Parsley[R] =
            lift21(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    implicit class Zipped22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]
        (t: =>(Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10], Parsley[T11],
               Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19], Parsley[T20], Parsley[T21],
               Parsley[T22])) {
        lazy val (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22) = t
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R): Parsley[R] =
            lift22(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    // $COVERAGE-ON$
}