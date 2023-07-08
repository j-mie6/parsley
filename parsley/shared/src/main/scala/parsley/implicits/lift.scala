/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.implicits

import parsley.Parsley, Parsley.pure
import parsley.lift._

// TODO: Scaladoc bug where the macros aren't expanding into the methods, so it's duplicated. Works on Scala 3 though
/** This module provides the "`lift` syntax", which enables a `lift` combinator on functions
  * of arities up to 22, applying the function across the results of several parsers.
  *
  * @example {{{
  * scala> import parsley.character.char
  * scala> import parsley.implicits.lift.{Lift2, Lift3}
  * scala> case class Add(x: Int, y: Int)
  * scala> val p = Add.lift(char('a') #> 4, char('b') #> 5)
  * scala> p.parse("ab")
  * val res0 = Success(Add(4, 5))
  * scala> val f = (x: Int, y: Int, z: Int) => x * y + z
  * scala> val q = f.lift(char('a') #> 3, char('b') #> 2, char('c') #> 5)
  * scala> q.parse("abc")
  * val res1 = Success(11)
  * scala> q.parse("ab")
  * val res2 = Failure(..)
  * }}}
  *
  * @since 3.0.0
  * @note a limitation of this syntax is that it requires the function's type to be fully known.
  *       For a version of this syntax that behaves better with type inference, see [[zipped `zipped`]].
  *
  * @define constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
  * @define classdesc This class enables the `lift` syntax on functions of arity
  * @define paramdesc the function to apply to the parsers.
  *
  * @define body
  *     This combinator executes each of its argument parsers in turn and applies this function to their results.
  *
  *     Each of the given parsers is executed in sequence, each yielding a result. So long as
  *     every parser succeeded, the whole combinator succeeds and each of the results is fed into
  *     this function. The result of this application is returned by the combinator. If any
  *     of the given parsers fails then the whole combinator fails.
  * @define return a parser that applies this function to the results of all the given parsers.
  */
object lift {
    // $COVERAGE-OFF$
    // scalastyle:off parameter.number ensure.single.space.after.token
    /** This class enables the `lift` syntax on any value, which functions as `pure` as a postfix method.
      *
      * @constructor $constructor
      * @param x the value to lift to the parser level.
      */
    implicit final class Lift0[R](private val x: R) extends AnyVal {
        /** This combinator injects a value into the parser level without consuming input.
          *
          * This acts as a postfix verion of the `pure` combinator, this exists
          * for consistency with the other arity lifters.
          *
          * @return
          * @see [[parsley.Parsley.pure `pure`]]
          */
        def lift: Parsley[R] = pure(x)
    }
    /** $classdesc one: a flipped `map`.
      *
      * @constructor $constructor
      * @param f the function to apply to the result of the parser.
      */
    implicit final class Lift1[T1, R](private val f: T1 => R) extends AnyVal {
        /** This combinator maps this function over the result of the given parser `p1`.
          *
          * Acting as a flipped version of the regular `map` combinator, this exists
          * for consistency with the other arity lifters.
          *
          * @return a parser that applies this function the result of the given parser.
          * @see [[parsley.lift.lift1 `lift1`]]
          */
        def lift(p1: Parsley[T1]): Parsley[R] = lift1(f, p1)
    }
    /** $classdesc two.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift2[T1, T2, R](private val f: (T1, T2) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift2 `lift2`]]
          */
        def lift(p1: Parsley[T1], p2: =>Parsley[T2]): Parsley[R] = lift2(f, p1, p2)
    }
    /** $classdesc three.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift3[T1, T2, T3, R](private val f: (T1, T2, T3) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift3 `lift3`]]
          */
        def lift(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3]): Parsley[R] = lift3(f, p1, p2, p3)
    }
    /** $classdesc four.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift4[T1, T2, T3, T4, R](private val f: (T1, T2, T3, T4) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift4 `lift4`]]
          */
        def lift(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R] = lift4(f, p1, p2, p3, p4)
    }
    /** $classdesc five.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift5[T1, T2, T3, T4, T5, R](private val f: (T1, T2, T3, T4, T5) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift5 `lift5`]]
          */
        def lift(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R] = lift5(f, p1, p2, p3, p4, p5)
    }
    /** $classdesc six.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift6[T1, T2, T3, T4, T5, T6, R](private val f: (T1, T2, T3, T4, T5, T6) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift6 `lift6`]]
          */
        def lift(p1: Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6]): Parsley[R] =
            lift6(f, p1, p2, p3, p4, p5, p6)
    }
    /** $classdesc seven.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift7[T1, T2, T3, T4, T5, T6, T7, R](private val f: (T1, T2, T3, T4, T5, T6, T7) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift7 `lift7`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7]): Parsley[R] = lift7(f, p1, p2, p3, p4, p5, p6, p7)
    }
    /** $classdesc eight.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift8[T1, T2, T3, T4, T5, T6, T7, T8, R](private val f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift8 `lift8`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8]): Parsley[R] = lift8(f, p1, p2, p3, p4, p5, p6, p7, p8)
    }
    /** $classdesc nine.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift9 `lift9`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9]): Parsley[R] = lift9(f, p1, p2, p3, p4, p5, p6, p7, p8, p9)
    }
    /** $classdesc ten.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift10 `lift10`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R] = lift10(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    }
    /** $classdesc eleven.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift11 `lift11`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11]): Parsley[R] =
            lift11(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    }
    /** $classdesc twelve.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift12 `lift12`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4],
                 p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
                 p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R] =
            lift12(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
    }
    /** $classdesc thirteen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift13 `lift13`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13]): Parsley[R] = lift13(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
    }
    /** $classdesc fourteen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift14 `lift14`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R] = lift14(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
    }
    /** $classdesc fifteen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift15 `lift15`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15]): Parsley[R] =
            lift15(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
    }
    /** $classdesc sixteen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift16 `lift16`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16]): Parsley[R] =
            lift16(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
    }
    /** $classdesc seventeen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift17 `lift17`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R] =
            lift17(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
    }
    /** $classdesc eighteen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift18 `lift18`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R] =
            lift18(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
    }
    /** $classdesc nineteen.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift19 `lift19`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18],
                 p19: =>Parsley[T19]): Parsley[R] = lift19(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
    }
    /** $classdesc twenty.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift20 `lift20`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18],
                 p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R] =
            lift20(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
    }
    /** $classdesc twenty-one.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift21 `lift21`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18],
                 p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21]): Parsley[R] =
            lift21(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
    }
    /** $classdesc twenty-two.
      *
      * @constructor $constructor
      * @param f $paramdesc
      */
    implicit final class Lift22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (private val f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R) extends AnyVal {
        /** This combinator executes each of its argument parsers in turn and applies this function to their results.
          *
          * Each of the given parsers is executed in sequence, each yielding a result. So long as
          * every parser succeeded, the whole combinator succeeds and each of the results is fed into
          * this function. The result of this application is returned by the combinator. If any
          * of the given parsers fails then the whole combinator fails.
          *
          * @return a parser that applies this function to the results of all the given parsers.
          * @see [[parsley.lift.lift22 `lift22`]]
          */
        def lift(p1:   Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6],
                 p7: =>Parsley[T7], p8: =>Parsley[T8], p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12],
                 p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15], p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18],
                 p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21], p22: =>Parsley[T22]): Parsley[R] =
            lift22(f, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
    }
    // scalastyle:on parameter.number ensure.single.space.after.token
    // $COVERAGE-ON$
}
