/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.implicits

import parsley.Parsley
import parsley.lift._

// TODO: Scaladoc bug where the `f` and `z` macros aren't expanding into the methods, so it's duplicated. Works on Scala 3 though
/** This module provides alternatives to the `f.lift(x, y)` syntax, `(x, y).zipped(f)`, which works better with type inference.
  *
  * Also enables a parameterless `zipped` method, to pair an arbitrary number of parsers such that `(p, q).zipped = p.zip(q)`.
  *
  * ''Thanks to Andrei Gramescu and George Stacey for ensuring that these combinators even exist in the first place''.
  *
  * @example {{{
  * scala> import parsley.character.char
  * scala> import parsley.implicits.zipped.{Zipped2, Zipped3}
  * scala> case class Add(x: Int, y: Int)
  * scala> val p = (char('a') #> 4, char('b') #> 5).zipped(Add)
  * scala> p.parse("ab")
  * val res0 = Success(Add(4, 5))
  * scala> val q = (char('a') #> 3, char('b') #> 2, char('c') #> 5).zipped((x, y, z) => x * y + z)
  * scala> q.parse("abc")
  * val res1 = Success(11)
  * scala> q.parse("ab")
  * val res2 = Failure(..)
  * }}}
  *
  * @note these methods are '''not''' lazy like the `lift` syntax or `liftN` functions! Use the [[parsley.Parsley.LazyParsley.unary_~ prefix `~`]] combinator
  *       to make arguments lazy where necessary.
  * @since 3.0.0
  *
  * @define constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
  * @define classdesc This class enables the `zipped` syntax on tuples of
  * @define paramdesc parsers whose results should be zipped together.
  *
  * @define fdesc the function to apply across the results of all the parsers.
  * @define freturn a parser that seqeunces each of these parsers and combines their results with the function `f`.
  * @define fbody
  *     This combinator executes each of these parsers and combines their results with a given function.
  *
  *     Each of these parsers is executed in turn, each yielding a result. So long as every parser
  *     succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
  *     The result of applying `f` to the results is returned by the combinator. If any of these parsers
  *     fail, the whole combinator fails.
  *
  * @define zreturn a parser that seqeunces each of these parsers and pairs them all together.
  * @define zbody
  *     This combinator executes each of these parsers and pairs all their results together.
  *
  *     Each of these parsers is executed in turn, each yielding a result. So long as every parser
  *     succeeded, the whole combinator succeeds.
  *     The pair formed from all of the results is returned by the parser. If any of these parsers
  *     fail, the whole combinator fails.
  */
object zipped {
    // $COVERAGE-OFF$
    // scalastyle:off parameter.number ensure.single.space.after.token
    /** This class enables the `zipped` syntax on pairs of two parsers.
      *
      * @constructor $constructor
      * @param t the two $paramdesc
      */
    implicit final class Zipped2[T1, T2](private val t: (Parsley[T1], Parsley[T2])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2) => R): Parsley[R] = lift2(f, t._1, t._2)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2)] = this.zipped((_, _))
    }
    /** $classdesc three parsers.
      *
      * @constructor $constructor
      * @param t the three $paramdesc
      */
    implicit final class Zipped3[T1, T2, T3](private val t: (Parsley[T1], Parsley[T2], Parsley[T3])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3) => R): Parsley[R] = lift3(f, t._1, t._2, t._3)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3)] = this.zipped((_, _, _))
    }
    /** $classdesc four parsers.
      *
      * @constructor $constructor
      * @param t the four $paramdesc
      */
    implicit final class Zipped4[T1, T2, T3, T4](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4) => R): Parsley[R] = lift4(f, t._1, t._2, t._3, t._4)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4)] = this.zipped((_, _, _, _))
    }
    /** $classdesc five parsers.
      *
      * @constructor $constructor
      * @param t the five $paramdesc
      */
    implicit final class Zipped5[T1, T2, T3, T4, T5](private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5) => R): Parsley[R] = lift5(f, t._1, t._2, t._3, t._4, t._5)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5)] = this.zipped((_, _, _, _, _))
    }
    /** $classdesc six parsers.
      *
      * @constructor $constructor
      * @param t the six $paramdesc
      */
    implicit final class Zipped6[T1, T2, T3, T4, T5, T6]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6) => R): Parsley[R] = lift6(f, t._1, t._2, t._3, t._4, t._5, t._6)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6)] = this.zipped((_, _, _, _, _, _))
    }
    /** $classdesc seven parsers.
      *
      * @constructor $constructor
      * @param t the seven $paramdesc
      */
    implicit final class Zipped7[T1, T2, T3, T4, T5, T6, T7]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7) => R): Parsley[R] = lift7(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7)] = this.zipped((_, _, _, _, _, _, _))
    }
    /** $classdesc eight parsers.
      *
      * @constructor $constructor
      * @param t the eight $paramdesc
      */
    implicit final class Zipped8[T1, T2, T3, T4, T5, T6, T7, T8]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R): Parsley[R] = lift8(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8)] = this.zipped((_, _, _, _, _, _, _, _))
    }
    /** $classdesc nine parsers.
      *
      * @constructor $constructor
      * @param t the nine $paramdesc
      */
    implicit final class Zipped9[T1, T2, T3, T4, T5, T6, T7, T8, T9]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R): Parsley[R] = lift9(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = this.zipped((_, _, _, _, _, _, _, _, _))
    }
    /** $classdesc ten parsers.
      *
      * @constructor $constructor
      * @param t the ten $paramdesc
      */
    implicit final class Zipped10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9],
                         Parsley[T10])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R): Parsley[R] = lift10(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = this.zipped((_, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc eleven parsers.
      *
      * @constructor $constructor
      * @param t the eleven $paramdesc
      */
    implicit final class Zipped11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R): Parsley[R] =
            lift11(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc twelve parsers.
      *
      * @constructor $constructor
      * @param t the twelve $paramdesc
      */
    implicit final class Zipped12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R): Parsley[R] =
            lift12(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc thirteen parsers.
      *
      * @constructor $constructor
      * @param t the thirteen $paramdesc
      */
    implicit final class Zipped13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R): Parsley[R] =
            lift13(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc fourteen parsers.
      *
      * @constructor $constructor
      * @param t the fourteen $paramdesc
      */
    implicit final class Zipped14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R): Parsley[R] =
            lift14(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc fifteen parsers.
      *
      * @constructor $constructor
      * @param t the fifteen $paramdesc
      */
    implicit final class Zipped15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R): Parsley[R] =
            lift15(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc sixteen parsers.
      *
      * @constructor $constructor
      * @param t the sixteen $paramdesc
      */
    implicit final class Zipped16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R): Parsley[R] =
            lift16(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc seventeen parsers.
      *
      * @constructor $constructor
      * @param t the seventeen $paramdesc
      */
    implicit final class Zipped17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R): Parsley[R] =
            lift17(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc eighteen parsers.
      *
      * @constructor $constructor
      * @param t the eighteen $paramdesc
      */
    implicit final class Zipped18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R): Parsley[R] =
            lift18(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc nineteen parsers.
      *
      * @constructor $constructor
      * @param t the nineteen $paramdesc
      */
    implicit final class Zipped19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18],
                         Parsley[T19])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R): Parsley[R] =
            lift19(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc twenty parsers.
      *
      * @constructor $constructor
      * @param t the twenty $paramdesc
      */
    implicit final class Zipped20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19],
                         Parsley[T20])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R): Parsley[R] =
            lift20(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc twenty-one parsers.
      *
      * @constructor $constructor
      * @param t the twenty-one $paramdesc
      */
    implicit final class Zipped21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19],
                         Parsley[T20], Parsley[T21])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R): Parsley[R] =
            lift21(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    /** $classdesc twenty-two parsers.
      *
      * @constructor $constructor
      * @param t the twenty-two $paramdesc
      */
    implicit final class Zipped22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]
        (private val t: (Parsley[T1], Parsley[T2], Parsley[T3], Parsley[T4], Parsley[T5], Parsley[T6], Parsley[T7], Parsley[T8], Parsley[T9], Parsley[T10],
                         Parsley[T11], Parsley[T12], Parsley[T13], Parsley[T14], Parsley[T15], Parsley[T16], Parsley[T17], Parsley[T18], Parsley[T19],
                         Parsley[T20], Parsley[T21], Parsley[T22])) extends AnyVal {
        /** This combinator executes each of these parsers and combines their results with a given function.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds and each of the results is fed into the function `f`.
          * The result of applying `f` to the results is returned by the combinator. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @param f the function to apply across the results of all the parsers.
          * @return a parser that seqeunces each of these parsers and combines their results with the function `f`.
          */
        def zipped[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R): Parsley[R] =
            lift22(f, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11,
                      t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
        /** This combinator executes each of these parsers and pairs all their results together.
          *
          * Each of these parsers is executed in turn, each yielding a result. So long as every parser
          * succeeded, the whole combinator succeeds.
          * The pair formed from all of the results is returned by the parser. If any of these parsers
          * fail, the whole combinator fails.
          *
          * @return a parser that seqeunces each of these parsers and pairs them all together.
          */
        def zipped: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
            this.zipped((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))
    }
    // scalastyle:on parameter.number ensure.single.space.after.token
    // $COVERAGE-ON$
}
