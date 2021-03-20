package parsley.expr

import parsley.Parsley, Parsley.notFollowedBy
import parsley.combinator.choice
import parsley.implicits.zipped.Zipped2
import parsley.XCompat._
import parsley.errors.combinator.ErrorMethods

/** This object is used to construct precedence parsers from either a [[Levels]] or many `Ops[A, A]`.
  * @since 2.2.0
  */
object precedence {
    private def convertOperators[A, B](atom: Parsley[A], opList: Ops[A, B])(implicit wrap: A => B): Parsley[B] = opList match
    {
        case Lefts(ops @ _*) => chain.left1(atom, choice(ops: _*))
        case Rights(ops @ _*) => chain.right1(atom, choice(ops: _*))
        case Prefixes(ops @ _*) => chain.prefix(choice(ops: _*), parsley.XCompat.applyWrap(wrap)(atom))
        // FIXME: Postfix operators which are similar to binary ops may fail, how can we work around this?
        case Postfixes(ops @ _*) => chain.postfix(parsley.XCompat.applyWrap(wrap)(atom), choice(ops: _*))
        case NonAssocs(ops @ _*) => {
            val op = choice(ops: _*)
            val guardNonAssoc = notFollowedBy(op).explain("non-associative operators cannot be chained together")
            atom <**> ((op, atom).zipped((f, y) => f(_, y)) </> wrap) <* guardNonAssoc
        }

    }

    private def crushLevels[A, B](lvls: Levels[A, B]): Parsley[B] = lvls match {
        case Atoms_(ev, atoms@_*) => ev.substituteCo[Parsley](choice(atoms: _*))
        case Level(lvls, ops) => convertOperators(crushLevels(lvls), ops)(ops.wrap)
    }

    /** This is used to build an expression parser for a monolithic type. Levels are specified from strongest
     * to weakest.
     * @tparam A The type of the monolithic tree
     * @param atoms The atomic units of the expression, for instance numbers/variables
     * @param table A table of operators. Table is ordered highest precedence to lowest precedence.
     *              Each list in the table corresponds to operators of the same precedence level.
     * @return A parser for the described expression language
     * @since 3.0.0
     */
    def apply[A](atoms: Parsley[A]*)(table: Ops[A, A]*): Parsley[A] = apply(table.foldLeft(Atoms(atoms: _*))(Level.apply[A, A, A]))

    //def apply[A](atoms: Parsley[A]*)(table: Ops[A, A]*): Parsley[A] = apply(table.foldLeft(Atoms(atoms: _*))(Lvl.apply[A, A, A]))

    /** This is used to build an expression parser for a multi-layered expression tree type. Levels can be
      * either tightest to loosest binding (using `:+`) or loosest to tightest (using `+:`)
      * @tparam A The type of the atomic unit of the expression
      * @tparam B The type of the resulting parse tree (outermost operations)
      * @param table A table of operators. Table is ordered depending on the operator used to build it.
      *              See [[Levels]] and it's subtypes for a description of how the types work.
      * @return A parser for the described expression language
      * @since 3.0.0
      */
    def apply[A, B](table: Levels[A, B]): Parsley[B] = crushLevels(table)
}