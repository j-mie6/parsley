/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package generic.experimental
import scala.quoted.*

import bridges.ErrorBridge

// TODO: we want to have .guardAgainst/.filterOut terminal methods too for each
// TODO: we want some kind of way to denote where exactly the metadata should be parsed after

/**
  *
  */
object bridge {
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](Nil, None)

    inline def label(label: String, labels: String*) = Labelled(label +: labels)
    inline def explain(reason: String) = Explained(reason)
}

class Labelled(labels: Seq[String]) {
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](labels.toList, None)

    inline def explain(reason: String) = LabelledAndExplained(labels, reason)
}

class Explained(reason: String) {
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](Nil, Some(reason))

    inline def label(label: String, labels: String*) = LabelledAndExplained(label +: labels, reason)
}

class LabelledAndExplained(labels: Seq[String], reason: String) {
    inline transparent def apply[T]: ErrorBridge = apply[T, T]
    inline transparent def apply[T, S >: T]: ErrorBridge = MacroImpl.impl[T, S](labels.toList, Some(reason))
}

private object MacroImpl {
    inline transparent def impl[T, S >: T](labels: List[String], reason: Option[String]) = ${bridgeImpl[T, S]('labels, 'reason)}
    def bridgeImpl[T: Type, S >: T: Type](labels: Expr[List[String]], reason: Expr[Option[String]])(using Quotes): Expr[ErrorBridge] = {
        BridgeImpl().synthesise[T, S](labels, reason)
    }
}
