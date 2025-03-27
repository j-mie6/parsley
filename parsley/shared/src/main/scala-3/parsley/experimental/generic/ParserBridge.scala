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
*/

abstract class Bridge1[T, R] extends ErrorBridge {
    def apply(p1: Parsley[T]): Parsley[R]
}
abstract class Bridge2[T1, T2, R] extends ErrorBridge {
    def apply(p1: Parsley[T1], p2: Parsley[T2]): Parsley[R]
}

inline transparent def bridge[T]: ErrorBridge = bridge[T, T]
inline transparent def bridge[T, S >: T]: ErrorBridge = ${bridgeImpl[T, S]}
private def bridgeImpl[T: Type, S: Type](using Quotes): Expr[ErrorBridge] = BridgeImpl().synthesise[T, S]
// having a class here simplifies the importing of quotes.reflect.* for the enum
private class BridgeImpl(using Quotes) {
    import quotes.reflect.*
    private enum BridgeArg {
        case Pos
        case Default(n: Int, sym: Symbol)
        case Err(name: String)
    }

    def synthesise[T: Type, S: Type] = {
        val tyRepr = TypeRepr.of[T]
        tyRepr match {
            case Bridgeable(cls, bridgeParams, otherParams) =>
                val numBridgeParams = bridgeParams.length
                val categorisedArgs = categoriseArgs(cls, otherParams, numBridgeParams + 1)
                val bridgeTyArgs = bridgeParams.collect {
                    case ValDef(_, ty, _) if !isPos(ty.tpe) => ty.tpe
                }
                println(bridgeTyArgs.map(_.typeSymbol))
                println(categorisedArgs)
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

    private def isPos(ty: TypeRepr): Boolean = ty.asType match {
        case '[parsley.experimental.generic.Pos] => true
        case _                                   => false
    }

    private def defaultName(n: Int) = s"$$lessinit$$greater$$default$$$n"
    private def defaulted(cls: Symbol, n: Int): Option[Symbol] = {
        cls.companionModule.declaredMethod(defaultName(n)).headOption
    }

    @tailrec
    private def categoriseArgs(cls: Symbol, nonPrimaryArgs: List[List[ValDef]], n: Int, buf: mutable.ListBuffer[List[BridgeArg]] = mutable.ListBuffer.empty): List[List[BridgeArg]] = nonPrimaryArgs match {
        case Nil => buf.toList
        case args :: restArgs =>
            categoriseArgs(cls, restArgs, n + args.length, buf += args.zipWithIndex.map {
                case (ValDef(name, ty, _), i) => defaulted(cls, i + n) match {
                    case None if isPos(ty.tpe) => BridgeArg.Pos
                    case Some(sym)             => BridgeArg.Default(i + n, sym)
                    case None                  => BridgeArg.Err(name)
                }
            })
    }

    // NOT handling positions
    // Everything but first set of brackets have defaults
    private def constructor1[T: Type, R: Type]: Expr[T => R] = {
        // if R is an AppliedType, we need to deconstruct it, because the constructor will need the things
        println('{(x: Boolean) => new Bar[T](x)()}.asTerm)
        '{???}
    }

    private object Bridgeable {
        def unapply(ty: TypeRepr): Option[(Symbol, List[ValDef], List[List[ValDef]])] = {
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
}

class Bar[A](val x: Boolean)(val y: String = "hello world")
