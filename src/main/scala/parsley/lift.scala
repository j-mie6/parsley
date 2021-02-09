package parsley
import parsley.internal.deepembedding

/**
  * This module contains `lift1` through `lift22`, which allow for the
  * application of a `FunctionN` to `N` parsers for example:
  *
  * @example {{{lift2[Int, Int, Int](_+_, px, py): Parsley[Int]}}}
  * @example {{{lift3((x: Int, y: Int, z: Int) => x + y + z, px, py, pz): Parsley[Int]}}}
  * @since 2.2.0
  */
object lift {
    def lift1[T1, R]
        (f: T1 => R,
         p1: =>Parsley[T1]): Parsley[R] =
        p1.map(f)
    def lift2[T1, T2, R]
        (f: (T1, T2) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2]): Parsley[R] =
        new Parsley(new deepembedding.Lift2(f, p1.internal, p2.internal))
    def lift3[T1, T2, T3, R]
        (f: (T1, T2, T3) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3]): Parsley[R] =
        new Parsley(new deepembedding.Lift3(f, p1.internal, p2.internal, p3.internal))
    // $COVERAGE-OFF$
    def lift4[T1, T2, T3, T4, R]
        (f: (T1, T2, T3, T4) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R] =
        lift4(f.curried, p1, p2, p3, p4)
    def lift5[T1, T2, T3, T4, T5, R]
        (f: (T1, T2, T3, T4, T5) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R] =
        lift5(f.curried, p1, p2, p3, p4, p5)
    def lift6[T1, T2, T3, T4, T5, T6, R]
        (f: (T1, T2, T3, T4, T5, T6) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6]): Parsley[R] =
        lift6(f.curried, p1, p2, p3, p4, p5, p6)
    def lift7[T1, T2, T3, T4, T5, T6, T7, R]
        (f: (T1, T2, T3, T4, T5, T6, T7) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7]): Parsley[R] =
        lift7(f.curried, p1, p2, p3, p4, p5, p6, p7)
    def lift8[T1, T2, T3, T4, T5, T6, T7, T8, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7],
         p8: =>Parsley[T8]): Parsley[R] =
        lift8(f.curried, p1, p2, p3, p4, p5, p6, p7, p8)
    def lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9]): Parsley[R] =
        lift9(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9)
    def lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R] =
        lift10(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    def lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11]): Parsley[R] =
        lift11(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    def lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R] =
        lift12(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
    def lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13]): Parsley[R] =
        lift13(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
    def lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R] =
        lift14(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
    def lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14],
         p15: =>Parsley[T15]): Parsley[R] =
        lift15(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
    def lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16]):
        Parsley[R] = lift16(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
    def lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R] =
        lift17(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
    def lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R] =
        lift18(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
    def lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19]): Parsley[R] =
        lift19(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
    def lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R] =
        lift20(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
    def lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21]): Parsley[R] =
        lift21(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
    // $COVERAGE-ON$
    def lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21],
         p22: =>Parsley[T22]): Parsley[R] =
        lift22(f.curried, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)

    // INTERNAL HELPERS
    @inline private def lift4[T1, T2, T3, T4, R]
        (f: T1 => T2 => T3 => T4 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R] =
        lift3((x1: T1, x2: T2, x3: T3) => f(x1)(x2)(x3), p1, p2, p3) <*> p4
    @inline private def lift5[T1, T2, T3, T4, T5, R]
        (f: T1 => T2 => T3 => T4 => T5 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R] =
        lift4(f, p1, p2, p3, p4) <*> p5
    @inline private def lift6[T1, T2, T3, T4, T5, T6, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6]): Parsley[R] =
        lift5(f, p1, p2, p3, p4, p5) <*> p6
    @inline private def lift7[T1, T2, T3, T4, T5, T6, T7, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7]): Parsley[R] =
        lift6(f, p1, p2, p3, p4, p5, p6) <*> p7
    @inline private def lift8[T1, T2, T3, T4, T5, T6, T7, T8, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7],
         p8: =>Parsley[T8]): Parsley[R] =
        lift7(f, p1, p2, p3, p4, p5, p6, p7) <*> p8
    @inline private def lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9]): Parsley[R] =
        lift8(f, p1, p2, p3, p4, p5, p6, p7, p8) <*> p9
    @inline private def lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R] =
        lift9(f, p1, p2, p3, p4, p5, p6, p7, p8, p9) <*> p10
    @inline private def lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11]): Parsley[R] =
        lift10(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) <*> p11
    @inline private def lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R] =
        lift11(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) <*> p12
    @inline private def lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13]): Parsley[R] =
        lift12(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) <*> p13
    @inline private def lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R] =
        lift13(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) <*> p14
    @inline private def lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14],
         p15: =>Parsley[T15]): Parsley[R] =
        lift14(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) <*> p15
    @inline private def lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16]):
        Parsley[R] = lift15(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) <*> p16
    @inline private def lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R] =
        lift16(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) <*> p17
    @inline private def lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R] =
        lift17(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) <*> p18
    @inline private def lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19]): Parsley[R] =
        lift18(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) <*> p19
    @inline private def lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R] =
        lift19(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) <*> p20
    @inline private def lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => T21 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21]): Parsley[R] =
        lift20(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) <*> p21
    @inline private def lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (f: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => T21 => T22 => R,
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21],
         p22: =>Parsley[T22]): Parsley[R] =
        lift21(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) <*> p22
}