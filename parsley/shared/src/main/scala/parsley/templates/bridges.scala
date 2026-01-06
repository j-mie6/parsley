/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package templates

import Parsley.transPure
import lift.*

// $COVERAGE-OFF$
// scalastyle:off parameter.number ensure.single.space.after.token
// Allows for easier implementation for the singleton = pure case
private [templates] trait PureSingletonImpl[+A] { this:  bridges.ParserSingletonBridge[A] =>
    /** The abstract hook method: what value is the singleton representing?
      * @since 4.0.0
      */
    protected def con: A
    final protected def singleton: Parsley[A] = transPure(con)
}

/** Template bridge trait for singleton objects that simply return themselves
  * after running the parser provided to `from`.
  *
  * @since 4.0.0
  */
trait PureParserBridge0[+R] extends bridges.ParserSingletonBridge[R] with PureSingletonImpl[R] { this: R =>
    /** @inheritdoc */
    override final def con: R = this
}

/** Template bridge trait for types that have constructors of arity 1. */
trait PureParserBridge1[-T1, +R] extends bridges.ParserBridge1[T1, R] with PureSingletonImpl[T1 => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1: Parsley[T1]): Parsley[R] = error(lift1(this.con, x1).ut()).uo(name)
    /** @inheritdoc */
    override final def con: T1 => R = this.apply
}

/** Template bridge trait for types that have constructors of arity 2. */
trait PureParserBridge2[-T1, -T2, +R] extends bridges.ParserBridge2[T1, T2, R] with PureSingletonImpl[(T1, T2) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1: Parsley[T1], x2: =>Parsley[T2]): Parsley[R] = error(lift2(this.con, x1, x2).ut()).uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2) => R = this.apply(_, _)
}

/** Template bridge trait for types that have constructors of arity 3. */
trait PureParserBridge3[-T1, -T2, -T3, +R] extends bridges.ParserBridge3[T1, T2, T3, R] with PureSingletonImpl[(T1, T2, T3) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3]): Parsley[R] = error(lift3(this.con, x1, x2, x3).ut()).uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3) => R = this.apply(_, _, _)
}

/** Template bridge trait for types that have constructors of arity 4. */
trait PureParserBridge4[-T1, -T2, -T3, -T4, +R] extends bridges.ParserBridge4[T1, T2, T3, T4, R] with PureSingletonImpl[(T1, T2, T3, T4) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4]): Parsley[R] = {
        error(lift4(this.con, x1, x2, x3, x4).ut()).uo(name)
    }
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4) => R = this.apply(_, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 5. */
trait PureParserBridge5[-T1, -T2, -T3, -T4, -T5, +R] extends bridges.ParserBridge5[T1, T2, T3, T4, T5, R] with PureSingletonImpl[(T1, T2, T3, T4, T5) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5]): Parsley[R] = error {
        lift5(this.con, x1, x2, x3, x4, x5).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5) => R = this.apply(_, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 6. */
trait PureParserBridge6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends bridges.ParserBridge6[T1, T2, T3, T4, T5, T6, R] with PureSingletonImpl[(T1, T2, T3, T4, T5, T6) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1: Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6]): Parsley[R] = error {
        lift6(this.con, x1, x2, x3, x4, x5, x6).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6) => R = this.apply(_, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 7. */
trait PureParserBridge7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends bridges.ParserBridge7[T1, T2, T3, T4, T5, T6, T7, R] with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
              x7: =>Parsley[T7]): Parsley[R] = error {
        lift7(this.con, x1, x2, x3, x4, x5, x6, x7).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7) => R = this.apply(_, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 8. */
trait PureParserBridge8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends bridges.ParserBridge8[T1, T2, T3, T4, T5, T6, T7, T8, R] with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
              x7: =>Parsley[T7], x8: =>Parsley[T8]): Parsley[R] = error {
        lift8(this.con, x1, x2, x3, x4, x5, x6, x7, x8).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8) => R = this.apply(_, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 9. */
trait PureParserBridge9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends bridges.ParserBridge9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4: =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
              x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9]): Parsley[R] = error {
        lift9(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R = this.apply(_, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 10. */
trait PureParserBridge10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends bridges.ParserBridge10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4], x5: =>Parsley[T5], x6: =>Parsley[T6],
              x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10]): Parsley[R] = error {
        lift10(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R = this.apply(_, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 11. */
trait PureParserBridge11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R]
    extends bridges.ParserBridge11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5: =>Parsley[T5], x6: =>Parsley[T6],
              x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11]): Parsley[R] = error {
        lift11(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 12. */
trait PureParserBridge12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R]
    extends bridges.ParserBridge12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:   Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5: =>Parsley[T5],   x6:  =>Parsley[T6],
              x7: =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12]): Parsley[R] = error {
        lift12(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 13. */
trait PureParserBridge13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R]
    extends bridges.ParserBridge13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1], x2: =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5: =>Parsley[T5],   x6:  =>Parsley[T6],
              x7:  =>Parsley[T7], x8: =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13]): Parsley[R] = error {
        lift13(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 14. */
trait PureParserBridge14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R]
    extends bridges.ParserBridge14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2], x3: =>Parsley[T3], x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8], x9: =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14]): Parsley[R] = error {
        lift14(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R = this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 15. */
trait PureParserBridge15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R]
    extends bridges.ParserBridge15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3], x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9], x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15]): Parsley[R] = error {
        lift15(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 16. */
trait PureParserBridge16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R]
    extends bridges.ParserBridge16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5: =>Parsley[T5],   x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16]): Parsley[R] = error {
        lift16(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 17. */
trait PureParserBridge17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R]
    extends bridges.ParserBridge17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17]): Parsley[R] = error {
        lift17(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 18. */
trait PureParserBridge18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R]
    extends bridges.ParserBridge18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7, x8: T8, x9: T9, x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18]): Parsley[R] =
        error(lift18(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18).ut()).uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 19. */
trait PureParserBridge19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R]
    extends bridges.ParserBridge19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8, x9: T9, x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
              x19: =>Parsley[T19]): Parsley[R] = error {
        lift19(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 20. */
trait PureParserBridge20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R]
    extends bridges.ParserBridge20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8,   x9: T9, x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
              x19: =>Parsley[T19], x20: =>Parsley[T20]): Parsley[R] = error {
        lift20(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 21. */
trait PureParserBridge21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R]
    extends bridges.ParserBridge21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8,   x9: T9,   x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
              x19: =>Parsley[T19], x20: =>Parsley[T20], x21: =>Parsley[T21]): Parsley[R] = error {
        lift21(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}

/** Template bridge trait for types that have constructors of arity 22. */
trait PureParserBridge22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R]
    extends bridges.ParserBridge22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
       with PureSingletonImpl[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R] {
    /** The abstract hook method: this is the method that should be used
      * to combine the results of the parsers provided to the template method
      * into the result type `R`. */
    def apply(x1:  T1,  x2:  T2,  x3:  T3,  x4:  T4,  x5:  T5,  x6: T6,   x7: T7,   x8: T8,   x9: T9,   x10: T10, x11: T11,
              x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21, x22: T22): R
    /** The template method: this is the method that can be used to
      * sequence and combine the results of all the parsers. */
    def apply(x1:    Parsley[T1],  x2:  =>Parsley[T2],  x3:  =>Parsley[T3],  x4:  =>Parsley[T4],  x5:  =>Parsley[T5],  x6:  =>Parsley[T6],
              x7:  =>Parsley[T7],  x8:  =>Parsley[T8],  x9:  =>Parsley[T9],  x10: =>Parsley[T10], x11: =>Parsley[T11], x12: =>Parsley[T12],
              x13: =>Parsley[T13], x14: =>Parsley[T14], x15: =>Parsley[T15], x16: =>Parsley[T16], x17: =>Parsley[T17], x18: =>Parsley[T18],
              x19: =>Parsley[T19], x20: =>Parsley[T20], x21: =>Parsley[T21], x22: =>Parsley[T22]): Parsley[R] = error {
        lift22(this.con, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22).ut()
    }.uo(name)
    /** @inheritdoc */
    override final def con: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R =
        this.apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
}
// scalastyle:on parameter.number ensure.single.space.after.token
// $COVERAGE-ON$
