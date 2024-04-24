/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

//TODO: opaque/transparent
import parsley.Parsley.{transPure => pure}
import parsley.ap._ // scalastyle:ignore underscore.import

import parsley.internal.deepembedding.frontend

/** This module contains `lift1` through `lift22`, which allow for the application of a function of arity `N` to `N` parsers.
  *
  * The combinators contained in this module all sequence a number of parsers together, but are capable of combining the
  * results generated by these parsers into a single value with a given function of the correct arity. This is a clean
  * way of putting together multiple parsers and getting a meaningful result out.
  *
  * @example {{{
  * scala> import parsley.character.char
  * scala> import parsley.lift.{lift2, lift3}
  * scala> case class Add(x: Int, y: Int)
  * scala> val p = lift2(Add, char('a').as(4), char('b').as(5))
  * scala> p.parse("ab")
  * val res0 = Success(Add(4, 5))
  * scala> val q = lift3((x: Int, y: Int, z: Int) => x * y + z, char('a').as(3), char('b').as(2), char('c').as(5))
  * scala> q.parse("abc")
  * val res1 = Success(11)
  * scala> q.parse("ab")
  * val res2 = Failure(..)
  * scala> val q2 = lift3[Int, Int, Int, Int](_ * _ + _, char('a').as(3), char('b').as(2), char('c').as(5))
  * }}}
  * @since 2.2.0
  *
  * @define bodyLift
  *     This combinator applies the given parsers in sequence and then applies the given function `f` of to all of the results.
  *
  *     Firstly, each parser is parsed in turn, each producing a result. So long as all of the parsers succeeded,
  *     the combinator can succeed by returning the application of the function `f` to all the arguments. If any
  *     of the parsers fails, the entire combinator fails.
  *
  * @define paramLift a function to apply to the results of the parsers with arity
  * @define returnLift a parser that parses all of the given parsers in order, and then combines their results with `f`.
  */
object lift extends lift
private [parsley] trait lift {
    // scalastyle:off parameter.number ensure.single.space.after.token
    /** This combinator allows the result of a given parser to be changed using a given function.
      *
      * Effectively alias for `map`, to be consistent with the other `lift` variants.
      *
      * @param f the function to map across the given parser
      * @return a parser that applies the function `f` to the result of the given parser.
      */
    final def lift1[T1, R]
        (f: T1 => R,
         p1: Parsley[T1]): Parsley[R] =
        p1.map(f)
    /** $bodyLift
      *
      * @param f $paramLift two.
      * @return $returnLift
      */
    final def lift2[T1, T2, R]
        (f: (T1, T2) => R,
         p1: Parsley[T1], p2: =>Parsley[T2]): Parsley[R] =
        new Parsley(new frontend.Lift2(f, p1.internal, p2.internal))
    /** $bodyLift
      *
      * @param f $paramLift three.
      * @return $returnLift
      */
    final def lift3[T1, T2, T3, R]
        (f: (T1, T2, T3) => R,
         p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3]): Parsley[R] =
        new Parsley(new frontend.Lift3(f, p1.internal, p2.internal, p3.internal))
    // $COVERAGE-OFF$
    /** $bodyLift
      *
      * @param f $paramLift four.
      * @return $returnLift
      */
    final def lift4[T1, T2, T3, T4, R]
        (f: (T1, T2, T3, T4) => R,
         p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R] =
        ap4(pure(f), p1, p2, p3, p4)
    /** $bodyLift
      *
      * @param f $paramLift five.
      * @return $returnLift
      */
    final def lift5[T1, T2, T3, T4, T5, R]
        (f: (T1, T2, T3, T4, T5) => R,
         p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R] =
        ap5(pure(f), p1, p2, p3, p4, p5)
    /** $bodyLift
      *
      * @param f $paramLift six.
      * @return $returnLift
      */
    final def lift6[T1, T2, T3, T4, T5, T6, R]
        (f: (T1, T2, T3, T4, T5, T6) => R,
         p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6]): Parsley[R] =
        ap6(pure(f), p1, p2, p3, p4, p5, p6)
    /** $bodyLift
      *
      * @param f $paramLift seven.
      * @return $returnLift
      */
    final def lift7[T1, T2, T3, T4, T5, T6, T7, R]
        (f: (T1, T2, T3, T4, T5, T6, T7) => R,
         p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7]): Parsley[R] =
        ap7(pure(f), p1, p2, p3, p4, p5, p6, p7)
    /** $bodyLift
      *
      * @param f $paramLift eight.
      * @return $returnLift
      */
    final def lift8[T1, T2, T3, T4, T5, T6, T7, T8, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7],
         p8: =>Parsley[T8]): Parsley[R] =
        ap8(pure(f), p1, p2, p3, p4, p5, p6, p7, p8)
    /** $bodyLift
      *
      * @param f $paramLift nine.
      * @return $returnLift
      */
    final def lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9]): Parsley[R] =
        ap9(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9)
    /** $bodyLift
      *
      * @param f $paramLift ten.
      * @return $returnLift
      */
    final def lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R] =
        ap10(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    /** $bodyLift
      *
      * @param f $paramLift eleven.
      * @return $returnLift
      */
    final def lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11]): Parsley[R] =
        ap11(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    /** $bodyLift
      *
      * @param f $paramLift twelve.
      * @return $returnLift
      */
    final def lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R] =
        ap12(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
    /** $bodyLift
      *
      * @param f $paramLift thirteen.
      * @return $returnLift
      */
    final def lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13]): Parsley[R] =
        ap13(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
    /** $bodyLift
      *
      * @param f $paramLift fourteen.
      * @return $returnLift
      */
    final def lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R] =
        ap14(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
    /** $bodyLift
      *
      * @param f $paramLift fifteen.
      * @return $returnLift
      */
    final def lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14],
         p15: =>Parsley[T15]): Parsley[R] =
        ap15(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
    /** $bodyLift
      *
      * @param f $paramLift sixteen.
      * @return $returnLift
      */
    final def lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16]):
        Parsley[R] = ap16(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
    /** $bodyLift
      *
      * @param f $paramLift seventeen.
      * @return $returnLift
      */
    final def lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R] =
        ap17(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
    /** $bodyLift
      *
      * @param f $paramLift eighteen.
      * @return $returnLift
      */
    final def lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R] =
        ap18(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
    /** $bodyLift
      *
      * @param f $paramLift nineteen.
      * @return $returnLift
      */
    final def lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19]): Parsley[R] =
        ap19(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
    /** $bodyLift
      *
      * @param f $paramLift twenty.
      * @return $returnLift
      */
    final def lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R] =
        ap20(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
    /** $bodyLift
      *
      * @param f $paramLift twenty-one.
      * @return $returnLift
      */
    final def lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21]): Parsley[R] =
        ap21(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
    // $COVERAGE-ON$
    /** $bodyLift
      *
      * @param f $paramLift twenty-two.
      * @return $returnLift
      */
    final def lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R,
         p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21],
         p22: =>Parsley[T22]): Parsley[R] =
        ap22(pure(f), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
    // scalastyle:on parameter.number ensure.single.space.after.token
}
