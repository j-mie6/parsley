package parsley.expr

import parsley.Parsley, Parsley.notFollowedBy
import parsley.combinator.choice
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.zipped.Zipped2

/** This object is used to construct precedence parsers from either a [[Prec]] or many `Ops[A, A]`.
  * @since 2.2.0
  * @group Precedence
  */
object precedence {
    private def convertOperators[A, B](atom: Parsley[A], opList: Ops[A, B]): Parsley[B] = {
        implicit val wrap: A => B = opList.wrap
        opList match {
            case Lefts(ops @ _*) => infix.left1(atom, choice(ops: _*))
            case Rights(ops @ _*) => infix.right1(atom, choice(ops: _*))
            case Prefixes(ops @ _*) => chain.prefix(choice(ops: _*), parsley.XCompat.applyWrap(wrap)(atom))
            // FIXME: Postfix operators which are similar to binary ops may fail, how can we work around this?
            case Postfixes(ops @ _*) => chain.postfix(parsley.XCompat.applyWrap(wrap)(atom), choice(ops: _*))
            case NonAssocs(ops @ _*) =>
                val op = choice(ops: _*)
                val guardNonAssoc = notFollowedBy(op).explain("non-associative operators cannot be chained together")
                atom <**> ((op, atom).zipped((f, y) => f(_, y)) </> wrap) <* guardNonAssoc
        }
    }

    private def crushLevels[A](lvls: Prec[A]): Parsley[A] = lvls match {
        case Atoms(atoms @ _*) => choice(atoms: _*)
        case Level(lvls, ops) => convertOperators(crushLevels(lvls), ops)
    }

    /** This is used to build an expression parser for a monolithic type: levels are specified from strongest
      * to weakest.
      * @tparam A The type of the monolithic result
      * @param atoms The atomic units of the expression, for instance numbers/variables
      * @param table A table of operators. Table is ordered highest precedence to lowest precedence.
      *              Each list in the table corresponds to operators of the same precedence level.
      * @return A parser for the described expression language
      * @since 3.0.0
      */
    def apply[A](atoms: Parsley[A]*)(table: Ops[A, A]*): Parsley[A] = apply(table.foldLeft[Prec[A]](Atoms(atoms: _*))(Level.apply))

    /** This is used to build an expression parser for a monolithic type: levels are specified from weakest
      * to strongest.
      * @tparam A The type of the monolithic result
      * @param atom The atomic unit of the expression, for instance numbers/variables
      * @param table A table of operators. Table is ordered highest precedence to lowest precedence.
      *              Each list in the table corresponds to operators of the same precedence level.
      * @return A parser for the described expression language
      * @since 3.0.0
      * @note due to limitations with type erasure, the `atom` for this function is ''not'' variadic.
      */
    def apply[A](table: Ops[A, A]*)(atom: Parsley[A]): Parsley[A] = apply(atom)(table.reverse: _*)

    /** This is used to build an expression parser for a multi-layered expression tree type. Levels can be
      * either tightest to loosest binding (using `:+`) or loosest to tightest (using `+:`)
      * @tparam A The type of the resulting parse tree (outermost operations)
      * @param table A table of operators. Table is ordered depending on the operator used to build it.
      *              See [[Prec]] and it's subtypes for a description of how the types work.
      * @return A parser for the described expression language
      * @since 4.0.0
      */
    def apply[A](table: Prec[A]): Parsley[A] = crushLevels(table)
}
