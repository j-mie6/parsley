/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package experimental.generic

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.*
import generic.*

/*
Problem space:
    * How are error bridges incorporated in (annotation?)
    * How can we resolve defaults (https://github.com/com-lihaoyi/cask/blob/master/cask/src-3/cask/router/Macros.scala)
*/

abstract class Bridge1[T, R] extends ErrorBridge {
    def apply(p1: Parsley[T]): Parsley[R]
}
abstract class Bridge2[T1, T2, R] extends ErrorBridge {
    def apply(p1: Parsley[T1], p2: Parsley[T2]): Parsley[R]
}

inline transparent def bridge[T]: ErrorBridge = bridge[T, T]
inline transparent def bridge[T, S >: T]: ErrorBridge = ${bridgeImpl[T, S]}
def bridgeImpl[T: Type, S: Type](using Quotes): Expr[ErrorBridge] = {
    import quotes.reflect.*

    val tyRepr = TypeRepr.of[T]
    tyRepr match {
        case Bridgeable(cls, bridgeParams, otherParams) =>
            val numBridgeParams = bridgeParams.length
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
            //val arity = bridgeTyArgs.length
            bridgeTyArgs.map(_.asType) match {
                case List('[t1]) =>
                    '{new Bridge1[t1, S] {
                        //  ap1(pos.map(con), x)
                        def apply(x1: Parsley[t1]): Parsley[S] = x1.map(${constructor1[t1, S]})
                    }}
                case List('[t1], '[t2]) => '{new Bridge2[t1, t2, S] {}}
                // TODO: 20 more of these
                case _ => '{???}
            }
        case _ => report.errorAndAbort("can only make bridges for constructible classes or objects")
    }
}

class Bar[A](val x: Boolean)(val y: String = "hello world")

object Bridgeable {
    def unapply(using Quotes)(ty: quotes.reflect.TypeRepr): Option[(quotes.reflect.Symbol, List[quotes.reflect.ValDef], List[List[quotes.reflect.ValDef]])] = {
        import quotes.reflect.*
        val clsDef = ty.classSymbol
        clsDef match {
            case Some(cls) =>
                val primCon = cls.primaryConstructor
                if (primCon.isNoSymbol) None
                else primCon.tree match {
                    // this even works for objects, as they are singletons with an empty constructor,
                    // returning (cls, Nil, Nil)
                    case DefDef("<init>", paramClauses, _, _) =>
                        val bridgeParams :: otherParams = paramClauses.collect {
                            case TermParamClause(ps) => ps
                        }: @unchecked
                        Some((cls, bridgeParams, otherParams))
                    case _ => None
                }
            case None => None
        }
    }
}

// NOT handling positions
// Everything but first set of brackets have defaults
def constructor1[T: Type, R: Type](using Quotes): Expr[T => R] = {
    import quotes.reflect.*
    // if R is an AppliedType, we need to deconstruct it, because the constructor will need the things
    println(TypeRepr.of[R].dealias)
    import quotes.reflect.*
    println('{(x: Boolean) => new Bar[T](x)()}.asTerm)
    '{???}
}

def isPos(using Quotes)(ty: quotes.reflect.TypeRepr): Boolean = ty.asType match {
    case '[parsley.experimental.generic.Pos] => true
    case _                                   => false
}

def defaultName(n: Int) = s"$$lessinit$$greater$$default$$$n"

def defaulted(using Quotes)(cls: quotes.reflect.Symbol, n: Int): Option[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    val DefName = defaultName(n)
    // find the first method that have the defName
    cls.companionModule.declaredMethods.map(_.tree).collectFirst {
        case dfn@DefDef(DefName, _, _, _) => dfn.symbol
    }
}

// returns the structure of the secondary arguments, but needs turning into tree.
@tailrec
def transformDefaults(using Quotes)(cls: quotes.reflect.Symbol, nonPrimaryArgs: List[List[_]], n: Int = 1, buf: mutable.ListBuffer[List[Option[quotes.reflect.Symbol]]] = mutable.ListBuffer.empty): List[List[Option[quotes.reflect.Symbol]]] = nonPrimaryArgs match {
    case Nil => buf.toList
    case args :: restArgs =>
        transformDefaults(cls, restArgs, n + args.length, buf += args.zipWithIndex.map((_, i) => defaulted(cls, i + n)))
}
