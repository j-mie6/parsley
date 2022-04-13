package parsley.expr

import parsley.Parsley

/**
 * Helper object to build values of `Ops[A, B]`, for generalised precedence parsing
 * @since 2.2.0
 */
object GOps {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.Op[A, B]`, NOT `Op[A, B]`. Builds an `Ops` object which represents many operators
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
    * @since 2.2.0
    */
    def apply[A, B](fixity: Fixity)(ops: Parsley[fixity.Op[A, B]]*)(implicit wrap: A => B): Ops[A, B] = fixity match {
        case InfixL  => Lefts[A, B](ops.asInstanceOf[Seq[Parsley[InfixL.Op[A, B]]]]: _*)
        case InfixR  => Rights[A, B](ops.asInstanceOf[Seq[Parsley[InfixR.Op[A, B]]]]: _*)
        case Prefix  => Prefixes[A, B](ops.asInstanceOf[Seq[Parsley[Prefix.Op[A, B]]]]: _*)
        case Postfix => Postfixes[A, B](ops.asInstanceOf[Seq[Parsley[Postfix.Op[A, B]]]]: _*)
        case InfixN  => NonAssocs[A, B](ops.asInstanceOf[Seq[Parsley[InfixN.Op[A, B]]]]: _*)
    }
}

/**
 * Helper object to build values of `Ops[A, B]`, for precedence parsing with subtyped data-structures.
 * @since 3.0.0
 */
object SOps {
    /**
    * '''NOTE''': Currently a bug in scaladoc incorrect displays this functions type, it should be:
    * `fixity.Op[A, B]`, NOT `Op[A, B]`. Builds an `Ops` object which represents many operators
    * which act at the same precedence level, with a given fixity. Using path-dependent typing,
    * the given fixity describes the shape of the operators expected. For more information see
    * [[https://github.com/j-mie6/Parsley/wiki/Building-Expression-Parsers the Parsley wiki]].
    * @tparam B The type produced/consumed by the operators, must be a supertype of `A`
    * @tparam A The base type consumed by the operators
    * @param fixity The fixity of the operators described. See [[Fixity]]
    * @param ops The operators themselves, in varargs
    * @since 3.0.0
    * @note The order of types in this method is reversed compared with [[GOps.apply]], this is due to
    *       a Scala typing issue.
    */
    def apply[B, A <: B](fixity: Fixity)(ops: Parsley[fixity.Op[A, B]]*): Ops[A, B] = GOps(fixity)(ops: _*)
}
