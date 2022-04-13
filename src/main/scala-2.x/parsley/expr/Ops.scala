package parsley.expr

import parsley.Parsley

/**
  * Describes the operators at a specific level in the precedence tree, such that these ops
  * consume `B`s, possibly `A`s and produce `B`s: this depends on the [[Fixity]] of the operators.
  * @tparam A The base type consumed by the operators
  * @tparam B The type produced/consumed by the operators
  * @note For less complex types `Ops[A, A]` is sufficient
  * @since 2.2.0
  * @group Table
  */
sealed abstract class Ops[-A, B] {
    private [expr] val wrap: A => B
}
private [expr] case class Lefts[A, B](ops: Parsley[InfixL.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Rights[A, B](ops: Parsley[InfixR.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Prefixes[A, B](ops: Parsley[Prefix.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class Postfixes[A, B](ops: Parsley[Postfix.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]
private [expr] case class NonAssocs[A, B](ops: Parsley[InfixN.Op[A, B]]*)(implicit override val wrap: A => B) extends Ops[A, B]

/**
 * Helper object to build values of `Ops[A, A]`, for monolithic precedence parsing
 * @since 2.2.0
 * @group Builders
 */
object Ops {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.Op[A, A]`, NOT `Op[A, A]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam A The type associated with the operators (which it consumes and produces)
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    * @since 2.2.0
    */
    def apply[A](fixity: Fixity)(ops: Parsley[fixity.Op[A, A]]*): Ops[A, A] = GOps[A, A](fixity)(ops: _*)
}
