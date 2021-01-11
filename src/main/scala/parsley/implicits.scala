package parsley

import parsley.Char.{char, string}
import parsley.Parsley.{void, pure}
import parsley.lift.{lift1, lift2, lift3, lift4, lift5, lift6, lift7, lift8, lift9, lift10, lift11,
                     lift12, lift13, lift14, lift15, lift16, lift17, lift18, lift19, lift20, lift21, lift22}

import scala.language.implicitConversions

/**
  * Provides implicit conversions and lifts for different values and parsers.
  */
object implicits
{
    // $COVERAGE-OFF$
    @inline implicit def voidImplicitly[P](p: P)(implicit con: P => Parsley[_]): Parsley[Unit] = void(p)
    @inline implicit def stringLift(str: String): Parsley[String] = string(str)
    @inline implicit def charLift(c: Char): Parsley[Char] = char(c)

    implicit class Lift0[R]
        (private val x: R) extends AnyVal {
        def lift: Parsley[R] = pure(x)
    }
    implicit class Lift1[T1, R]
        (private val f: T1 => R) extends AnyVal {
        def lift(p1: Parsley[T1]): Parsley[R] = lift1(f, p1)
    }
    implicit class Lift2[T1, T2, R]
        (private val f: (T1, T2) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2]): Parsley[R] = lift2(f, p1, p2)
    }
    implicit class Lift3[T1, T2, T3, R]
        (private val f: (T1, T2, T3) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3]): Parsley[R] = lift3(f, p1, p2, p3)
    }
    implicit class Lift4[T1, T2, T3, T4, R]
        (private val f: (T1, T2, T3, T4) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4]): Parsley[R] = lift4(f, p1, p2, p3, p4)
    }
    implicit class Lift5[T1, T2, T3, T4, T5, R]
        (private val f: (T1, T2, T3, T4, T5) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5]): Parsley[R] = lift5(f, p1, p2, p3, p4, p5)
    }
    implicit class Lift6[T1, T2, T3, T4, T5, T6, R]
        (private val f: (T1, T2, T3, T4, T5, T6) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6]): Parsley[R] = lift6(f, p1, p2, p3, p4, p5, p6)
    }
    implicit class Lift7[T1, T2, T3, T4, T5, T6, T7, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7]): Parsley[R] = lift7(f, p1, p2, p3, p4, p5, p6, p7)
    }
    implicit class Lift8[T1, T2, T3, T4, T5, T6, T7, T8, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8]): Parsley[R] = lift8(f, p1, p2, p3, p4, p5, p6, p7, p8)
    }
    implicit class Lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9]): Parsley[R] = lift9(f, p1, p2, p3, p4, p5, p6, p7, p8, p9)
    }
    implicit class Lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10]): Parsley[R] = lift10(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    }
    implicit class Lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11]): Parsley[R] = lift11(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    }
    implicit class Lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12]): Parsley[R] =
            lift12(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
    }
    implicit class Lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13]): Parsley[R] = lift13(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
    }
    implicit class Lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14]): Parsley[R] = lift14(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
    }
    implicit class Lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15]): Parsley[R] =
            lift15(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
    }
    implicit class Lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16]): Parsley[R] =
            lift16(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
    }
    implicit class Lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16],
                 p17: Parsley[T17]): Parsley[R] = lift17(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
    }
    implicit class Lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16],
                 p17: Parsley[T17], p18: Parsley[T18]): Parsley[R] = lift18(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
    }
    implicit class Lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16],
                 p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19]): Parsley[R] =
            lift19(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
    }
    implicit class Lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16],
                 p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20]): Parsley[R] =
            lift20(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
    }
    implicit class Lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16],
                 p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20],
                 p21: Parsley[T21]): Parsley[R] = lift21(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
    }
    implicit class Lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R) extends AnyVal {
        def lift(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4],
                 p5: Parsley[T5], p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8],
                 p9: Parsley[T9], p10: Parsley[T10], p11: Parsley[T11], p12: Parsley[T12],
                 p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15], p16: Parsley[T16],
                 p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20], p21: Parsley[T21], p22: Parsley[T22]): Parsley[R] =
            lift22(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
    }
    // $COVERAGE-ON$
}