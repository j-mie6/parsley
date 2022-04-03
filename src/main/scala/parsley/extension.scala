package parsley

import Parsley.{notFollowedBy}
import combinator.{many, option, some, ifP}

/**
  * These implicit classes can be used to extend the core combinator set of Parsley.
  * This may mean that importing them enables combinators that can be used on ''non-`Parsley`'' types,
  * or might enable some syntactic sugar that is not part of the core combinator "style".
  */
// $COVERAGE-OFF$
object extension {
    /**
     * This class exposes the `<#>` combinator on functions.
     *
     * @param f The function that is used for the map
     * @version 1.0.0
     */
    implicit final class HaskellStyleMap[-A, +B](val f: A => B) extends AnyVal {
        /**This combinator is an alias for `map`.*/
        def <#>(p: Parsley[A]): Parsley[B] = p.map(f)
    }
    /**
     * This class exposes a ternary operator on pairs of parsers.
     *
     * @param pq The parsers which serve the branches of the if
     * @param con A conversion (if required) to turn elements of `pq` into parsers
     * @version 1.0.0
     */
    implicit final class LazyChooseParsley[P, +A](pq: =>(P, P))(implicit con: P => Parsley[A]) {
        private lazy val (p, q) = pq
        /**This combinator is an alias for `ifP`.*/
        def ?:(b: Parsley[Boolean]): Parsley[A] = ifP(b, con(p), con(q))
    }

    /** This class exposes a collection of "operator-style" combinators that are
     * plain syntactic sugar for other functionality in the library; they are potentially
     * less readable than the combinators they replace, so should be used sparingly.
     *
     * @since 4.0.0
     */
    implicit final class OperatorSugar[P, +A](p: P)(implicit con: P => Parsley[A]) {
        // TODO: doc
        def * : Parsley[List[A]] = many(con(p))
        // TODO: doc
        def + : Parsley[List[A]] = some(con(p))
        // TODO: doc
        def unary_! : Parsley[Unit] = notFollowedBy(con(p))
        // TODO: doc
        def -[B](q: Parsley[B]): Parsley[A] = !q *> con(p)
        // TODO: doc
        def ? : Parsley[Option[A]] = option(con(p))
    }
}
// $COVERAGE-ON$