package parsley

import Parsley.{notFollowedBy}
import combinator.{many, option, some, ifP}

/**
  * These implicit classes can be used to extend the core combinator set of Parsley.
  *
  * This may mean that importing them enables combinators that can be used on ''non-`Parsley`'' types,
  * or might enable some syntactic sugar that is not part of the core combinator "style".
  */
// $COVERAGE-OFF$
object extension {
    /** This class exposes the `<#>` combinator on functions.
      *
      * This extension class operates on functions. It enables the use of
      * the `<#>` combinator, which is an alias for `map` designed to more
      * closely mimic Haskell's style.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param f the function that is used for the map.
      * @version 1.0.0
      */
    implicit final class HaskellStyleMap[-A, +B](val f: A => B) extends AnyVal {
        /** This combinator maps this function over the given parser `p` to alter its result.
          *
          * @see [[Parsley.map `map`]]
          */
        def <#>(p: Parsley[A]): Parsley[B] = p.map(f)
    }

    /** This class exposes an if combinator on pairs of parsers.
      *
      * This extension class operators on pairs of values that are convertible to parsers. It enables the
      * use of the `?:` combinator, which is an alias for `ifP`.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param pq The values (convertible to parsers) that serve the branches of the if.
      * @param conP a conversion that allows values convertible to parsers to be used.
      * @param conQ a conversion that allows values convertible to parsers to be used.
      * @tparam P the type of left base value that this class is used on (the conversion to `Parsley`) is summoned automatically.
      * @tparam Q the type of right base value that this class is used on (the conversion to `Parsley`) is summoned automatically.
      * @version 1.0.0
      */
    implicit final class LazyChooseParsley[P, Q, +A](pq: =>(P, Q))(implicit conP: P => Parsley[A], conQ: Q => Parsley[A]) {
        private lazy val (p, q) = pq
        /** This combinator will conditionally parse one of the parsers in this pair depending on the result of a third parser `b`.
          *
          * Equivalent to `ifP`, but in operator form: `ifP(b, p, q)` is the same as `b ?: (p, q)`.
          *
          * @note Scala applies methods ending in `:` right-associatively, and the pair is on the right-hand side of the operator.
          * @see [[combinator.ifP `ifP`]]
          */
        def ?:(b: Parsley[Boolean]): Parsley[A] = ifP(b, conP(p), conQ(q))
    }

    /** This class enables "operator-style" alternative combinators on parsers.
      *
      * This extension class exposes a collection of "operator-style" combinators on values that are convertible to parsers
      * that are plain syntactic sugar for other functionality in the library; they are potentially
      * less readable than the combinators they replace, so should be used sparingly.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param p the value that this class is enabling methods on.
      * @param con a conversion that allows values convertible to parsers to be used.
      * @tparam P the type of base value that this class is used on (the conversion to `Parsley`) is summoned automatically.
      * @since 4.0.0
      */
    implicit final class OperatorSugar[P, +A](p: P)(implicit con: P => Parsley[A]) {
        /** This combinator will parse this parser '''zero''' or more times returning a list of the parsed results.
          *
          * Equivalent to `many`, but as a postfix operator: `many(p)` is the same as `p.*`.
          *
          * @note an alias for `many`.
          * @see [[combinator.many `many`]] for more details.
          */
        def * : Parsley[List[A]] = many(con(p))
        /** This combinator will parse this parser '''one''' or more times returning a list of the parsed results.
          *
          * Equivalent to `some`, but as a postfix operator: `some(p)` is the same as `p.+`.
          *
          * @note an alias for `some`.
          * @see [[combinator.some `some`]] for more details.
          */
        def + : Parsley[List[A]] = some(con(p))
        /** This combinator will succeed when this parser fails, and vice-versa, never consuming input.
          *
          * Equivalent to `notFollowedBy`, but as a prefix operator: `notFollowedBy(p)` is the same as `!p`.
          *
          * @note an alias for `notFollowedBy`.
          * @see [[Parsley.notFollowedBy `notFollowedBy`]] for more details.
          */
        def unary_! : Parsley[Unit] = notFollowedBy(con(p))
        /** This combinator first parses its argument `q`, and if it fails, it will parse this parser, returning its result.
          *
          * First `q` is parsed, and if it fails (regardless of whether it consumed input), then this parser is parsed.
          * The result of this parser is returned. If either `q` succeeds or this parser fails, the combinator fails.
          *
          * @example {{{
          * // a less efficient version of a keyword: it's more efficient to not have to read the entire keyword twice
          * def keyword(kw: String): Parsley[Unit] = {
          *     string(kw).void - identifier
          * }
          * }}}
          *
          * @param q the parser to quotient this parser by.
          * @return a parser that only parses this parser if `q` cannot parse.
          */
        def -(q: Parsley[_]): Parsley[A] = !q *> con(p)
        /** This combinator will try parsing this parser wrapping its result in `Some`, and return `None` if it fails.
          *
          * Equivalent to `option`, but as a postfix operator: `option(p)` is the same as `p.?`.
          *
          * @note an alias for `option`.
          * @see [[combinator.option `option`]] for more details.
          */
        def ? : Parsley[Option[A]] = option(con(p))
    }
}
// $COVERAGE-ON$