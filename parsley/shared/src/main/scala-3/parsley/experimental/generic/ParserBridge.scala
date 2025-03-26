/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package experimental.generic

import scala.quoted.*
import generic.*

/*
Problem space:
    * How are error bridges incorporated in (annotation?)
    * How can we resolve defaults (https://github.com/com-lihaoyi/cask/blob/master/cask/src-3/cask/router/Macros.scala)
*/

class DummyBridge1[T, R] extends ErrorBridge
class DummyBridge2[T1, T2, R] extends ErrorBridge

inline transparent def bridge[T]: ErrorBridge = bridge[T, T]
inline transparent def bridge[T, S >: T]: ErrorBridge = ${bridgeImpl[T, S]}
def bridgeImpl[T: Type, S: Type](using Quotes): Expr[ErrorBridge] = {
    import quotes.reflect.*

    val clsDef = TypeRepr.of[T].classSymbol
    println(clsDef)
    clsDef match {
        case Some(cls) =>
            val DefDef("<init>", paramClauses, _, _) = cls.primaryConstructor.tree: @unchecked
            // paramClauses is a list of lists, including *generics*
            // our bridge arity is related to the first non-type bracket
            val bridgeParams :: otherParams = paramClauses.collect {
                case TermParamClause(ps) => ps
            }: @unchecked
            val numBridgeParams = bridgeParams.length
            // FIXME: default check doesn't work, primary constructor doesn't have the default args...
            val badArgs = otherParams.flatten.zipWithIndex.collect {
                case (ValDef(name, ty, _), n) if !isPos(ty.tpe) && defaulted(cls, numBridgeParams + n + 1).isEmpty => name
            }
            if (badArgs.nonEmpty) {
                report.errorAndAbort(s"Arguments ${badArgs.mkString(",")} are not `Pos`, do not have defaults, and are not in first set of brackets; cannot construct bridge")
            }
            val bridgeTyArgs = bridgeParams.collect {
                case ValDef(_, ty, _) if !isPos(ty.tpe) => ty.tpe
            }
            println(bridgeTyArgs.map(_.typeSymbol))
            val arity = bridgeTyArgs.length
            bridgeTyArgs.map(_.asType) match {
                case List('[t1]) => '{new DummyBridge1[t1, S]}
                case List('[t1], '[t2]) => '{new DummyBridge2[t1, t2, S]}
                case _ => '{???}
            }
        case None => report.errorAndAbort("cannot make bridge for non-class/object type")
    }
}

def isPos(using Quotes)(ty: quotes.reflect.TypeRepr): Boolean = ty.asType match {
    case '[parsley.experimental.generic.Pos] => true
    case _                                   => false
}

def defaulted(using Quotes)(cls: quotes.reflect.Symbol, n: Int): Option[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    val DefName = s"$$lessinit$$greater$$default$$$n"
    // find the first method that have the defName
    cls.companionModule.declaredMethods.map(_.tree).collectFirst {
        case dfn@DefDef(DefName, _, _, _) => dfn.symbol
    }
}
