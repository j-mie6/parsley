/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.Parsley

/**
  * Denotes the fixity and associativity of an operator. Importantly, it also specifies the type of the
  * of the operations themselves.
  * @since 4.0.0
  * @group Fixities
  */
sealed trait Fixity {
    type Op[A, B]
    private [expr] def chain[A, B](p: Parsley[A], op: Parsley[Op[A, B]])(implicit wrap: A => B): Parsley[B]
    private [parsley] def ordinal: Int
}

/**
  * Describes left-associative binary operators
  * @since 4.0.0
  * @group Fixities
  */
case object InfixL extends Fixity {
    override type Op[-A, B] = (B, A) => B
    private [expr] def chain[A, B](p: Parsley[A], op: Parsley[Op[A, B]])(implicit wrap: A => B): Parsley[B] = infix.left1(p)(op)
    private [parsley] def ordinal: Int = Fixity.InfixLTag
}

/**
  * Describes right-associative binary operators
  * @since 4.0.0
  * @group Fixities
  */
case object InfixR extends Fixity {
    override type Op[-A, B] = (A, B) => B
    private [expr] def chain[A, B](p: Parsley[A], op: Parsley[Op[A, B]])(implicit wrap: A => B): Parsley[B] = infix.right1(p)(op)
    private [parsley] def ordinal: Int = Fixity.InfixRTag
}

/**
  * Describes unary prefix operators
  * @since 4.0.0
  * @group Fixities
  */
case object Prefix extends Fixity {
    override type Op[A, B] = B => B
    private [expr] def chain[A, B](p: Parsley[A], op: Parsley[Op[A, B]])(implicit wrap: A => B): Parsley[B] = infix.prefix(p)(op)
    private [parsley] def ordinal: Int = Fixity.PrefixTag
}

/**
  * Describes unary postfix operators
  * @since 4.0.0
  * @group Fixities
  */
case object Postfix extends Fixity {
    override type Op[A, B] = B => B
    private [expr] def chain[A, B](p: Parsley[A], op: Parsley[Op[A, B]])(implicit wrap: A => B): Parsley[B] = infix.postfix(p)(op)
    private [parsley] def ordinal: Int = Fixity.PostfixTag
}

/**
  * Describes non-associative operators
  * @since 4.0.0
  * @group Fixities
  */
case object InfixN extends Fixity {
    override type Op[-A, +B] = (A, A) => B
    private [expr] def chain[A, B](p: Parsley[A], op: Parsley[Op[A, B]])(implicit wrap: A => B): Parsley[B] = infix.nonassoc(p)(op)
    private [parsley] def ordinal: Int = Fixity.InfixNTag
}

// $COVERAGE-OFF$
private [parsley] object Fixity {
  final val PrefixTag = 0
  final val PostfixTag = 1
  final val InfixLTag = 2
  final val InfixRTag = 3
  final val InfixNTag = 4
}
// $COVERAGE-ON$
