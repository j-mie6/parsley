/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package bridges

import parsley.errors.combinator.*

// TODO: documentation
package object bridges

/** This generically allows for the tagging of parsers via its `error` combinator:
  * as it happens, both labels and reasons are very often attached to bridges.
  *
  * @since 5.0.0
  */
trait ErrorBridge {
    /** The labels that should be associated with a failure to parse this bridge.
      *
      * The default, `Nil`, will not affect the labelling of the original error.
      */
    def labels: List[String] = Nil
    /** The reason that should be associated with a failure to parse this bridge.
      *
      * The default, `None`, will not add any reasons.
      */
    def reason: Option[String] = None

    /** Applies the error components described by `labels` and `reason` to the given
      * parser.
      *
      * @note this should be used within a bridge's apply and the `from` combinator.
      */
    protected final def error[T](p: Parsley[T]): Parsley[T] = applyReason(applyLabels(p))
    private def applyLabels[T](p: Parsley[T]): Parsley[T] = labels match {
        case Nil => p
        case l0 :: ls => p.label(l0, ls*).ut()
    }
    private def applyReason[T](p: Parsley[T]): Parsley[T] = reason.foldLeft(p)(_.explain(_).ut())
}

/** Bridge trait enabling the `<#`/`from` combinator on this type:
  * this is useful when the constructor is not applied immediately,
  * like when using `precedence`. It does not track any metadata.
  *
  * @since 5.0.0
  */
trait SingletonBridge[+A] extends ErrorBridge {
    protected def singleton: Parsley[A]
    protected def name: String = this.toString
    /** The combinator on this implementing type that performs the parser and
      * returns `con`.
      *
      * @param op the parser that should be parsed before returning `con`.
      */
    infix final def from(op: Parsley[?]): Parsley[A] = error((singleton.ut() <~ op).ut()).uo(s"$name.from")
    /** The syntax on this implementing type that performs the parser and
      * returns `con`.
      *
      * @param op the parser that should be parsed before returning `con`.
      * @note equivalent to `from`.
    */
    final def <#(op: Parsley[?]): Parsley[A] = this.from(op).uo(s"$name <#")
}

trait ParserBridge1[-T, +R] extends SingletonBridge[T => R] {
    def apply(p1: Parsley[T]): Parsley[R]
}

trait ParserBridge2[-T1, -T2, +R] extends SingletonBridge[(T1, T2) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2]): Parsley[R]
}

trait ParserBridge3[-T1, -T2, -T3, +R] extends SingletonBridge[(T1, T2, T3) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3]): Parsley[R]
}

trait ParserBridge4[-T1, -T2, -T3, -T4, +R] extends SingletonBridge[(T1, T2, T3, T4) => R]  {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R]
}

trait ParserBridge5[-T1, -T2, -T3, -T4, -T5, +R] extends SingletonBridge[(T1, T2, T3, T4, T5) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R]
}

trait ParserBridge6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6]): Parsley[R]
}

trait ParserBridge7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7]): Parsley[R]
}

trait ParserBridge8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8]): Parsley[R]
}

trait ParserBridge9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9]): Parsley[R]
}

trait ParserBridge10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R]
}

trait ParserBridge11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11]): Parsley[R]
}

trait ParserBridge12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R]
}

trait ParserBridge13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13]): Parsley[R]
}

trait ParserBridge14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R]
}

trait ParserBridge15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15]): Parsley[R]
}

trait ParserBridge16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16]): Parsley[R]
}

trait ParserBridge17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R]
}

trait ParserBridge18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R]
}

trait ParserBridge19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19]): Parsley[R]
}

trait ParserBridge20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R]
}

trait ParserBridge21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20],
              p21: =>Parsley[T21]): Parsley[R]
}

trait ParserBridge22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R]
    extends SingletonBridge[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R] {
    def apply(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5],
              p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10],
              p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
              p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20],
              p21: =>Parsley[T21], p22: =>Parsley[T22]): Parsley[R]
}
