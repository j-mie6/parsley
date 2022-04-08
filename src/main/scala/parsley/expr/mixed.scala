package parsley.expr

import parsley.Parsley, Parsley._
import parsley.internal.deepembedding
import parsley.lift.lift4
import parsley.implicits.zipped.Zipped2

import scala.annotation.implicitNotFound

/** This module contains specialist combinators for mixing unary and binary operators
  * on the same level. This is only sensible when mixing infix-left and postfix
  * or infix-right and prefix.
  *
  * @since 4.0.0
  * @group Chains
  */
object mixed {
    private def flip[A, B, C](f: (A, B) => C, y: B)(x: A) = f(x, y)

    /**
      * @since 4.0.0
      */
    def right1[A, B](p: Parsley[A], uop: Parsley[B => B], bop: =>Parsley[(A, B) => B])
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        lazy val rest: Parsley[B] = (
                uop <*> rest
            <|> (p <**> ((bop, rest).zipped(flip(_, _) _) <|> pure(wrap)))
        )
        rest
    }

    /**
      * @since 4.0.0
      */
    def left1[A, B](p: Parsley[A], uop: =>Parsley[B => B], bop: =>Parsley[(B, A) => B])
                   (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        lazy val _uop = uop
        lazy val uops = _uop.foldLeft(identity[B] _)(_ compose _)
        lazy val rest: Parsley[B => B] = (
                lift4((b: (B, A) => B, y: A, u: B => B, r: B => B) => (x: B) => r(u(b(x, y))), bop, p, uops, rest)
            <|> pure(identity[B] _)
        )
        chain.postfix(p.map(wrap), _uop) <**> rest
    }
}