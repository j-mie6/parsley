package parsley.expr

import parsley.Parsley

/**
 * Describes the operators at a specific level in the precedence tree, such that these ops
 * consume `B`s, possibly `A`s and produce `B`s: this depends on the [[Fixity]] of the operators.
 * @tparam A The base type consumed by the operators
 * @tparam B The type produced/consumed by the operators
 * @note For less complex types, such as those which use subtyping `Ops[A, A]` is sufficient
 */
trait Ops[-A, B] {
    private [expr] val wrap: A => B
}
private [expr] case class Lefts[-A, B](ops: Parsley[(B, A) => B]*)(override val wrap: A => B) extends Ops[A, B]
private [expr] case class Rights[-A, B](ops: Parsley[(A, B) => B]*)(override val wrap: A => B) extends Ops[A, B]
private [expr] case class Prefixes[-A, B](ops: Parsley[B => B]*)(override val wrap: A => B) extends Ops[A, B]
private [expr] case class Postfixes[-A, B](ops: Parsley[B => B]*)(override val wrap: A => B) extends Ops[A, B]

/**
 * Helper object to build values of `Ops[A, B]`, for generalised precedence parsing
 */
object GOps {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.GOp[A, B]`, NOT `GOp[A, B]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam A The base type consumed by the operators
    * @tparam B The type produced/consumed by the operators
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    * @param wrap The function which should be used to wrap up a value of type `A` when required
    *             (this will be at right of a left-assoc chain, left of a right-assoc chain, or
    *             the root of a prefix/postfix chain)
    */
    def apply[A, B](fixity: Fixity)(ops: Parsley[fixity.GOp[A, B]]*)(implicit wrap: A => B): Ops[A, B] = fixity match {
        case InfixL  => Lefts[A, B](ops.asInstanceOf[Seq[Parsley[InfixL.GOp[A, B]]]]: _*)(wrap)
        case InfixR  => Rights[A, B](ops.asInstanceOf[Seq[Parsley[InfixR.GOp[A, B]]]]: _*)(wrap)
        case Prefix  => Prefixes[A, B](ops.asInstanceOf[Seq[Parsley[Prefix.GOp[A, B]]]]: _*)(wrap)
        case Postfix => Postfixes[A, B](ops.asInstanceOf[Seq[Parsley[Postfix.GOp[A, B]]]]: _*)(wrap)
    }
}

/**
 * Helper object to build values of `Ops[A, A]`, for monolithic precedence parsing
 */
object Ops {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.Op[A]`, NOT `Op[A]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam A The type associated with the operators (which it consumes and produces)
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    */
    def apply[A](fixity: Fixity)(ops: Parsley[fixity.Op[A]]*): Ops[A, A] = GOps[A, A](fixity)(ops: _*)
}