package parsley.expr

import parsley.Parsley, Parsley.LazyParsley
import parsley.internal.deepembedding

import scala.annotation.implicitNotFound

/** This module contains the very useful chaining family of combinators,
  * which are mostly used to parse operators and expressions of varying fixities.
  * It is a more low-level API compared with [[precedence]].
  * @since 2.2.0
  */
object chain {
    /**`right(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.
      * @since 2.2.0
      */
    def right[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B], x: B)
                   (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = right1(p, op).getOrElse(x)

    /**`left(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.
      * @since 2.2.0
      */
    def left[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B], x: B)
                  (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = left1(p, op).getOrElse(x)

    /**`right1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.
      * @since 2.2.0
      */
    def right1[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B])
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        new Parsley(new deepembedding.Chainr(p.internal, op.internal, wrap))
    }

    /**left1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.
      * @since 2.2.0
      */
    def left1[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B])
                   (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        lazy val _p = p
        // a sneaky sneaky trick :) If we know that A =:= B because refl was provided, then we can skip the wrapping
        lazy val init = if (parsley.XCompat.isIdentityWrap(wrap)) _p.asInstanceOf[Parsley[B]] else _p.map(wrap)
        new Parsley(new deepembedding.Chainl(init.internal, _p.internal, op.internal))
    }

    /**`prefix(op, p)` parses many prefixed applications of `op` onto a single final result of `p`
      * @since 2.2.0
      */
    def prefix[A](op: =>Parsley[A => A], p: =>Parsley[A]): Parsley[A] = new Parsley(new deepembedding.ChainPre(p.internal, op.internal))

    /**`postfix(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.
      * @since 2.2.0
      */
    def postfix[A](p: =>Parsley[A], op: =>Parsley[A => A]): Parsley[A] = new Parsley(new deepembedding.ChainPost(p.internal, op.internal))

    /**`prefix1(op, p)` parses one or more prefixed applications of `op` onto a single final result of `p`
      * @since 3.0.0
      */
    def prefix1[A, B <: A](op: =>Parsley[A => B], p: =>Parsley[A]): Parsley[B] = {
        lazy val op_ = op
        op_ <*> prefix(op_, p)
    }

    /**`postfix1(p, op)` parses one occurrence of `p`, followed by one or more postfix applications of `op`
      * that associate to the left.
      * @since 3.0.0
      */
    def postfix1[A, B <: A](p: =>Parsley[A], op: =>Parsley[A => B]): Parsley[B] = {
        lazy val op_ = op
        postfix(p <**> op_, op_)
    }
}