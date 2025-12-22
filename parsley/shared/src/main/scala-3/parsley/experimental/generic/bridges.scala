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

// TODO: <# synthesis and from api. Incorporate Singleton bridge (non-template as parent)
object bridges {
    abstract class Bridge1[T, R] extends ErrorBridge {
        def apply(p1: Parsley[T]): Parsley[R]
    }

    abstract class Bridge2[T1, T2, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2]): Parsley[R]
    }

    abstract class Bridge3[T1, T2, T3, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3]): Parsley[R]
    }

    abstract class Bridge4[T1, T2, T3, T4, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4]): Parsley[R]
    }

    abstract class Bridge5[T1, T2, T3, T4, T5, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5]): Parsley[R]
    }

    abstract class Bridge6[T1, T2, T3, T4, T5, T6, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6]): Parsley[R]
    }

    abstract class Bridge7[T1, T2, T3, T4, T5, T6, T7, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7]): Parsley[R]
    }

    abstract class Bridge8[T1, T2, T3, T4, T5, T6, T7, T8, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8]): Parsley[R]
    }

    abstract class Bridge9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9]): Parsley[R]
    }

    abstract class Bridge10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10]): Parsley[R]
    }

    abstract class Bridge11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11]): Parsley[R]
    }

    abstract class Bridge12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12]): Parsley[R]
    }

    abstract class Bridge13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13]): Parsley[R]
    }

    abstract class Bridge14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14]): Parsley[R]
    }

    abstract class Bridge15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15]): Parsley[R]
    }

    abstract class Bridge16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16]): Parsley[R]
    }

    abstract class Bridge17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17]): Parsley[R]
    }

    abstract class Bridge18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18]): Parsley[R]
    }

    abstract class Bridge19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19]): Parsley[R]
    }

    abstract class Bridge20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20]): Parsley[R]
    }

    abstract class Bridge21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20],
                  p21: Parsley[T21]): Parsley[R]
    }

    abstract class Bridge22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] extends ErrorBridge {
        def apply(p1: Parsley[T1], p2: Parsley[T2], p3: Parsley[T3], p4: Parsley[T4], p5: Parsley[T5],
                  p6: Parsley[T6], p7: Parsley[T7], p8: Parsley[T8], p9: Parsley[T9], p10: Parsley[T10],
                  p11: Parsley[T11], p12: Parsley[T12], p13: Parsley[T13], p14: Parsley[T14], p15: Parsley[T15],
                  p16: Parsley[T16], p17: Parsley[T17], p18: Parsley[T18], p19: Parsley[T19], p20: Parsley[T20],
                  p21: Parsley[T21], p22: Parsley[T22]): Parsley[R]
    }
}
