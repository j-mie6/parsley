/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

/**
  * Denotes the fixity and associativity of an operator. Importantly, it also specifies the type of the
  * of the operations themselves.
  * @since 4.0.0
  * @group Fixities
  */
sealed trait Fixity {
    type Op[A, B]
}

/**
  * Describes left-associative binary operators
  * @since 4.0.0
  * @group Fixities
  */
case object InfixL extends Fixity {
    override type Op[-A, B] = (B, A) => B
}

/**
  * Describes right-associative binary operators
  * @since 4.0.0
  * @group Fixities
  */
case object InfixR extends Fixity {
    override type Op[-A, B] = (A, B) => B
}

/**
  * Describes unary prefix operators
  * @since 4.0.0
  * @group Fixities
  */
case object Prefix extends Fixity {
    override type Op[A, B] = B => B
}

/**
  * Describes unary postfix operators
  * @since 4.0.0
  * @group Fixities
  */
case object Postfix extends Fixity {
    override type Op[A, B] = B => B
}

/**
  * Describes non-associative operators
  * @since 4.0.0
  * @group Fixities
  */
case object InfixN extends Fixity {
    override type Op[-A, +B] = (A, A) => B
}
