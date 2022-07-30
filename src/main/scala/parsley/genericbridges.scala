package parsley

import lift._

object genericbridges {
    // $COVERAGE-OFF$
    // scalastyle:off parameter.number ensure.single.space.after.token
    trait ParserSingletonBridge[+A] {
        def con: A
        final def <#(op: Parsley[_]): Parsley[A] = op #> con
    }

    trait ParserBridge0[+R] extends ParserSingletonBridge[R] { this: R =>
        override final def con: R = this
    }

    trait ParserBridge1[-T1, +R] extends ParserSingletonBridge[T1 => R] {
        def apply(x1: T1): R
        def apply(x1: Parsley[T1]): Parsley[R] = lift1(this.con, x1)
        override final def con: T1 => R = this.apply(_)
    }

    trait ParserBridge2[-T1, -T2, +R] extends ParserSingletonBridge[(T1, T2) => R] {
        def apply(x1: T1, x2: T2): R
        def apply(x1: Parsley[T1], x2: =>Parsley[T2]): Parsley[R] = lift2(this.con, x1, x2)
        override final def con: (T1, T2) => R = this.apply(_, _)
    }

    trait ParserBridge3[-T1, -T2, -T3, +R] extends ParserSingletonBridge[(T1, T2, T3) => R] {
        def apply(x1: T1, x2: T2, x3: T3): R
        def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3]): Parsley[R] = lift3(this.con, x1, x2, x3)
        override final def con: (T1, T2, T3) => R = this.apply(_, _, _)
    }

    trait ParserBridge4[-T1, -T2, -T3, -T4, +R] extends ParserSingletonBridge[(T1, T2, T3, T4) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4): R
        def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4]): Parsley[R] = lift4(this.con, x1, x2, x3, x4)
        override final def con: (T1, T2, T3, T4) => R = this.apply(_, _, _, _)
    }

    trait ParserBridge5[-T1, -T2, -T3, -T4, -T5, +R] extends ParserSingletonBridge[(T1, T2, T3, T4, T5) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5): R
        def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5]): Parsley[R] = lift5(this.con, x1, x2, x3, x4, x5)
        override final def con: (T1, T2, T3, T4, T5) => R = this.apply(_, _, _, _, _)
    }

    trait ParserBridge6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6): R
        def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6]): Parsley[R] =
            lift6(this.con, x1, x2, x3, x4, x5, x6)
        override final def con: (T1, T2, T3, T4, T5, T6) => R = this.apply(_, _, _, _, _, _)
    }

    trait ParserBridge7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7): R
        def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
                  x7: =>Parsley[T7]): Parsley[R] =
            lift7(this.con, x1, x2, x3, x4, x5, x6, x7)
        override final def con: (T1, T2, T3, T4, T5, T6, T7) => R = this.apply(_, _, _, _, _, _, _)
    }

    trait ParserBridge8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8): R
        def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
                  x7: =>Parsley[T7], x8: =>Parsley[T8]): Parsley[R] =
            lift8(this.con, x1, x2, x3, x4, x5, x6, x7, x8)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8) => R = this.apply(_, _, _, _, _, _, _, _)
    }

    trait ParserBridge9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9): R
        def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
                  x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9]): Parsley[R] =
            lift9(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R = this.apply(_, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10): R
        def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
                  x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10]): Parsley[R] =
            lift10(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R = this.apply(_, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11): R
        def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5: =>Parsley[T5], x6: =>Parsley[T6],
                  x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11]): Parsley[R] =
            lift11(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12): R
        def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5: =>Parsley[T5],   x6:  =>Parsley[T6],
                  x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12]): Parsley[R] =
            lift12(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13): R
        def apply(x1:    Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5: =>Parsley[T5],   x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13]): Parsley[R] =
            lift13(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14]): Parsley[R] =
            lift14(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R] {
        def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3], x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15]): Parsley[R] =
            lift15(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5: =>Parsley[T5],   x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16]): Parsley[R] =
            lift16(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17]): Parsley[R] =
            lift17(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7, x8: T8, x9: T9, x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18]): Parsley[R] =
            lift18(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8, x9: T9, x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
                  x19: =>Parsley[T19]): Parsley[R] =
            lift19(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8,   x9: T9, x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
                  x19: =>Parsley[T19], x20: =>Parsley[T20]): Parsley[R] =
            lift20(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8,   x9: T9,   x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
                  x19: =>Parsley[T19], x20: =>Parsley[T20], x21: =>Parsley[T21]): Parsley[R] =
            lift21(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }

    trait ParserBridge22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R]
        extends ParserSingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R] {
        def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8,   x9: T9,   x10: T10, x11: T11,
                  x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21, x22: T22): R
        def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
                  x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
                  x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
                  x19: =>Parsley[T19], x20: =>Parsley[T20], x21: =>Parsley[T21], x22: =>Parsley[T22]): Parsley[R] =
            lift22(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)
        override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R =
            this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    }
    // scalastyle:on parameter.number ensure.single.space.after.token
    // $COVERAGE-ON$
}
