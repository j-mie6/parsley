/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package parsley
package experimental.generic
import scala.quoted.*

import bridges.ErrorBridge

// TODO: consider how to incorporate the errors in cleanly, this sucks
inline transparent def bridge[T]: ErrorBridge = bridge[T, T]
inline transparent def bridge[T, S >: T]: ErrorBridge = ${bridgeImpl[T, S]('Nil, 'None)}

inline transparent def bridgeErr[T](labels: List[String], reason: Option[String]): ErrorBridge = bridgeErr[T, T](labels, reason)
inline transparent def bridgeErr[T, S >: T](labels: List[String], reason: Option[String]): ErrorBridge = ${bridgeImpl[T, S]('labels, 'reason)}

private def bridgeImpl[T: Type, S >: T: Type](labels: Expr[List[String]], reason: Expr[Option[String]])(using Quotes): Expr[ErrorBridge] = {
    BridgeImpl().synthesise[T, S](labels, reason)
}
