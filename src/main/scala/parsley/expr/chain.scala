package parsley.expr

import parsley.Parsley, Parsley.LazyParsley
import parsley.internal.deepembedding

import scala.annotation.implicitNotFound

object chain {
    /**`right(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def right[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B], x: B)
                   (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = right1(p, op).getOrElse(x)

    /**`left(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def left[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B], x: B)
                  (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = left1(p, op).getOrElse(x)

    /**`right1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.*/
    def right1[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B])
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        new Parsley(new deepembedding.Chainr(p.internal, op.internal, wrap))
    }

    /**left1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.*/
    def left1[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B])
                   (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        new Parsley(new deepembedding.Chainl(new deepembedding.Subroutine(p.internal), op.internal, wrap))
    }

    /**`prefix(op, p)` parses many prefixed applications of `op` onto a single final result of `p`*/
    def prefix[A](op: =>Parsley[A => A], p: =>Parsley[A]): Parsley[A] = new Parsley(new deepembedding.ChainPre(p.internal, op.internal))

    /**`postfix(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.*/
    def postfix[A](p: =>Parsley[A], op: =>Parsley[A => A]): Parsley[A] = new Parsley(new deepembedding.ChainPost(p.internal, op.internal))
}