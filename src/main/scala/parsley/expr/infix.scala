package parsley.expr

import parsley.Parsley
import parsley.internal.deepembedding

import scala.annotation.implicitNotFound

/** This module contains the very useful chaining family of combinators,
  * which are mostly used to parse operators and expressions of varying fixities.
  * It is a more low-level API compared with [[precedence]].
  *
  * Compared with the combinators in [[chain]], these allow for more freedom in
  * the type of the values and the operators.
  *
  * @since 4.0.0
  */
object infix {
    /** `right(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.
      * @since 4.0.0
      */
    def right[A, B, C >: B](p: Parsley[A], op: =>Parsley[(A, C) => B], x: C)
            (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${C}") wrap: A => C): Parsley[C] = right1(p, op).getOrElse(x)

    /** `left(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.
      * @since 4.0.0
      */
    def left[A, B, C >: B](p: Parsley[A], op: =>Parsley[(C, A) => B], x: C)
            (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${C}") wrap: A => C): Parsley[C] = left1(p, op).getOrElse(x)

    /** `right1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.
      * @since 4.0.0
      */
    def right1[A, B, C >: B](p: Parsley[A], op: =>Parsley[(A, C) => B])
            (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => C): Parsley[C] = {
        new Parsley(new deepembedding.Chainr(p.internal, op.internal, wrap))
    }

    /** `left1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.
      * @since 4.0.0
      */
    def left1[A, B, C >: B](p: Parsley[A], op: =>Parsley[(C, A) => B])
            (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => C): Parsley[C] = {
        // a sneaky sneaky trick :) If we know that A =:= B because refl was provided, then we can skip the wrapping
        new Parsley(new deepembedding.Chainl(parsley.XCompat.applyWrap(wrap)(p).internal, p.internal, op.internal))
    }
}