/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package experimental.generic

import generic.ErrorBridge

/*
Problem space:
    * How are error bridges incorporated in (annotation?)
*/
object bridges {
    trait SingletonBridge[+A] extends ErrorBridge {
        infix def from(op: Parsley[?]): Parsley[A]
        final def <#(op: Parsley[?]): Parsley[A] = this.from(op).uo(s"$this <#")
    }

    trait Bridge1[T, R] extends SingletonBridge[T => R] {
        def apply(p1: Parsley[T]): Parsley[R]
    }

    trait Bridge2[T1, T2, R] extends SingletonBridge[(T1, T2) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2]): Parsley[R]
    }

    trait Bridge3[T1, T2, T3, R] extends SingletonBridge[(T1, T2, T3) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3]): Parsley[R]
    }

    trait Bridge4[T1, T2, T3, T4, R] extends SingletonBridge[(T1, T2, T3, T4) => R]  {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4]): Parsley[R]
    }

    trait Bridge5[T1, T2, T3, T4, T5, R] extends SingletonBridge[(T1, T2, T3, T4, T5) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5]): Parsley[R]
    }

    trait Bridge6[T1, T2, T3, T4, T5, T6, R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6]): Parsley[R]
    }

    trait Bridge7[T1, T2, T3, T4, T5, T6, T7, R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7]): Parsley[R]
    }

    trait Bridge8[T1, T2, T3, T4, T5, T6, T7, T8, R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8]): Parsley[R]
    }

    trait Bridge9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9]): Parsley[R]
    }

    trait Bridge10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10]): Parsley[R]
    }

    trait Bridge11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11]): Parsley[R]
    }

    trait Bridge12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12]): Parsley[R]
    }

    trait Bridge13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13]): Parsley[R]
    }

    trait Bridge14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14]): Parsley[R]
    }

    trait Bridge15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15]): Parsley[R]
    }

    trait Bridge16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16]): Parsley[R]
    }

    trait Bridge17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17]): Parsley[R]
    }

    trait Bridge18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18]): Parsley[R]
    }

    trait Bridge19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19]): Parsley[R]
    }

    trait Bridge20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20]): Parsley[R]
    }

    trait Bridge21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20],
                  p21: Parsley[T21]): Parsley[R]
    }

    trait Bridge22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R] {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20],
                  p21: Parsley[T21], p22: Parsley[T22]): Parsley[R]
    }
}
