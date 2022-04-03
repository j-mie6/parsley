package parsley.expr

import parsley.Parsley
import parsley.internal.deepembedding.frontend

import scala.annotation.implicitNotFound

/** This module contains the very useful chaining family of combinators,
  * which are mostly used to parse operators and expressions of varying fixities.
  * It is a more low-level API compared with [[precedence]].
  * @since 2.2.0
  * @group chains
  *
  * @groupprio binary 0
  * @groupname binary Binary Operator Chains
  * @groupdesc binary
  *     These chains allow for the chaining together of values and binary operators in either left- or right-associative application.
  *
  * @groupprio unary 0
  * @groupname unary Unary Operator Chains
  * @groupdesc unary
  *     These chains allow for the chaining together and application of multiple prefix or postfix unary operators to a single value.
  */
object chain {
    /** `right(p, op, x)` parses '''zero''' or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.
      * @since 4.0.0
      * @group binary
      */
    def right[A](p: Parsley[A], op: =>Parsley[(A, A) => A], x: A): Parsley[A] = infix.right(p, op, x)

    /** `left(p, op, x)` parses '''zero''' or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.
      * @since 4.0.0
      * @group binary
      */
    def left[A](p: Parsley[A], op: =>Parsley[(A, A) => A], x: A): Parsley[A] = infix.left(p, op, x)

    /** `right1(p, op)` parses '''one''' or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.
      * @since 4.0.0
      * @group binary
      */
    def right1[A](p: Parsley[A], op: =>Parsley[(A, A) => A]): Parsley[A] = infix.right1(p, op)

    /** `left1(p, op)` parses '''one''' or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.
      * @since 4.0.0
      * @group binary
      */
    def left1[A](p: Parsley[A], op: =>Parsley[(A, A) => A]): Parsley[A] = infix.left1(p, op)

    /** `prefix(op, p)` parses many prefixed applications of `op` onto a single final result of `p`
      * @since 2.2.0
      * @group unary
      */
    def prefix[A](op: Parsley[A => A], p: Parsley[A]): Parsley[A] = new Parsley(new frontend.ChainPre(p.internal, op.internal))

    /** `postfix(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.
      * @since 2.2.0
      * @group unary
      */
    def postfix[A](p: Parsley[A], op: =>Parsley[A => A]): Parsley[A] = new Parsley(new frontend.ChainPost(p.internal, op.internal))

    /** `prefix1(op, p)` parses one or more prefixed applications of `op` onto a single final result of `p`
      * @since 3.0.0
      * @group unary
      */
    def prefix1[A, B <: A](op: Parsley[A => B], p: =>Parsley[A]): Parsley[B] = op <*> prefix(op, p)

    /** `postfix1(p, op)` parses one occurrence of `p`, followed by one or more postfix applications of `op`
      * that associate to the left.
      * @since 3.0.0
      * @group unary
      */
    def postfix1[A, B <: A](p: Parsley[A], op: =>Parsley[A => B]): Parsley[B] = {
        lazy val op_ = op
        postfix(p <**> op_, op_)
    }
}