/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.Parsley

/** This class allows for the description of a single layer of operators in the precedence tree.
  *
  * Describes the operators at a specific level in the precedence tree, such that these ops
  * consume `B`s, possibly `A`s and produce `B`s: this depends on the [[Fixity]] of the operators.
  *
  * @tparam A the base type consumed by the operators.
  * @tparam B the type produced/consumed by the operators.
  * @note for less complex types `Ops[A, A]` is sufficient.
  * @since 2.2.0
  * @group Table
  */
sealed abstract class Ops[A, B] { //TODO: This doesn't /seem/ to have an impact anymore... are we ok to merge with scala 2?
    private [expr] val wrap: A => B
}
private [expr] case class Lefts[A, B](ops: Parsley[InfixL.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Rights[A, B](ops: Parsley[InfixR.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Prefixes[A, B](ops: Parsley[Prefix.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Postfixes[A, B](ops: Parsley[Postfix.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class NonAssocs[A, B](ops: Parsley[InfixN.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]

/** This helper object is used to build values of `Ops[A, A]`, for homogeneous precedence parsing.
  *
  * @since 2.2.0
  * @group Builders
  */
object Ops {
    /** This function builds an `Ops` object representing many operators found at the same precedence level, with a given fixity.
      *
      * The operators found on the level constructed by this function are homogeneous: the type of the level below must
      * match the types of values produced at this level.
      *
      * Using path-dependent typing, the given fixity describes the shape of the operators expected. For more information see
      * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
      *
      * @tparam A the type associated with the operators (which it consumes and produces)
      * @param fixity the fixity of the operators described.
      * @param ops The operators themselves, in varargs.
      * @see [[Fixity `Fixity`]]
      * @note currently a bug in scaladoc incorrect displays this functions type, it should be: `fixity.Op[A, A]`, NOT `Op[A, A]`.
      * @since 2.2.0
      */
    def apply[A](fixity: Fixity)(ops: Parsley[fixity.Op[A, A]]*): Ops[A, A] = GOps[A, A](fixity)(ops: _*)
}
