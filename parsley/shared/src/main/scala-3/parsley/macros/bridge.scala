/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package macros

import scala.quoted.*

import bridges.ErrorBridge

// TODO: we want to have .guardAgainst/.filterOut terminal methods too for each
// TODO: we want some kind of way to denote where exactly the metadata should be parsed after

/** This object implements a macro that can synthesise parser bridges.
  *
  * @since 5.0.0
  */
object bridge {
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @since 5.0.0
      */
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`
      * and upcasts to `Parsley[S]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @tparam S the parent type of `T` to upcast to
      *
      * @since 5.0.0
      */
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](Nil, None)

    /** Returns a new bridge synthesiser, which builds bridges labelled by the
      * given errors.
      *
      * @param label0 the first label
      * @param labels any other labels
      * @since 5.0.0
      */
    inline def label(label0: String, labels: String*) = Labelled(label0 +: labels)

    /** Returns a new bridge synthesiser, which builds bridges explained by the given reason.
      *
      * @param reason explanation behind what is expected
      * @since 5.0.0
      */
    inline def explain(reason: String) = Explained(reason)
}

class Labelled(labels: Seq[String]) {
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @note has labels applied on error
      * @since 5.0.0
      */
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`
      * and upcasts to `Parsley[S]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @tparam S the parent type of `T` to upcast to
      * @note has labels applied on error
      * @since 5.0.0
      */
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](labels.toList, None)

    /** Returns a new bridge synthesiser, which builds bridges explained by the given reason
      * in addition to the existing labels.
      *
      * @param reason explanation behind what is expected
      * @since 5.0.0
      */
    inline def explain(reason: String) = LabelledAndExplained(labels, reason)
}

class Explained(reason: String) {
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @note has reason applied on error
      * @since 5.0.0
      */
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`
      * and upcasts to `Parsley[S]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @tparam S the parent type of `T` to upcast to
      * @note has reason applied on error
      * @since 5.0.0
      */
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](Nil, Some(reason))

    /** Returns a new bridge synthesiser, which builds bridges labelled by the
      * given errors as well as the already provided reason.
      *
      * @param label0 the first label
      * @param labels any other labels
      * @since 5.0.0
      */
    inline def label(label0: String, labels: String*) = LabelledAndExplained(label0 +: labels, reason)
}

class LabelledAndExplained(labels: Seq[String], reason: String) {
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @note has labels and reason applied on error
      * @since 5.0.0
      */
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    /** Synthesises a parser bridge that allows for the combining of the
      * correct number of sub-parsers to produce a value of type `Parsley[T]`
      * and upcasts to `Parsley[S]`.
      *
      * @tparam T the product type to bridge the arguments of
      * @tparam S the parent type of `T` to upcast to
      * @note has labels and reason applied on error
      * @since 5.0.0
      */
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](labels.toList, Some(reason))
}

private object MacroImpl {
    inline transparent def impl[T, S >: T](labels: List[String], reason: Option[String]) = ${bridgeImpl[T, S]('labels, 'reason)}
    def bridgeImpl[T: Type, S >: T: Type](labels: Expr[List[String]], reason: Expr[Option[String]])(using Quotes) = {
        BridgeImpl().synthesise[T, S](labels, reason)
    }
}
