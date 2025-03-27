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
            case Bridgeable(cls, tyParams, bridgeParams, otherParams) =>
                val numBridgeParams = bridgeParams.length
                val categorisedArgs = categoriseArgs(cls, otherParams, numBridgeParams + 1)
                // Used for the types of the lambda passed to combinator
                val bridgeTyArgs = bridgeParams.collect {
                    case sym if !isPos(sym.termRef) => sym.termRef
                }
                // TODO: look for unique position
                // TODO: ensure validation if Err is encounted (report separately, but then abort if failed (Option))
                println(categorisedArgs)
                //val arity = bridgeTyArgs.length
                bridgeTyArgs.map(_.typeSymbol.typeRef.asType) match {
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
    private def defaulted(cls: Symbol, n: Int): Option[Symbol] = cls.companionModule.declaredMethod(defaultName(n)).headOption

    @tailrec
    private def categoriseArgs(cls: Symbol, nonPrimaryArgs: List[List[Symbol]], n: Int, buf: mutable.ListBuffer[List[BridgeArg]] = mutable.ListBuffer.empty): List[List[BridgeArg]] = nonPrimaryArgs match {
        case Nil => buf.toList
        case args :: restArgs =>
            categoriseArgs(cls, restArgs, n + args.length, buf += args.zipWithIndex.map {
                case (sym, i) => defaulted(cls, i + n) match {
                    case None if isPos(sym.termRef) => BridgeArg.Pos
                    case Some(sym)                  => BridgeArg.Default(i + n, sym)
                    case None                       => BridgeArg.Err(sym.name)
                }
            })
    }

    // NOT handling positions
    // Everything but first set of brackets have defaults
    private def constructor1[T: Type, R: Type]: Expr[T => R] = {
        // if R is an AppliedType, we need to deconstruct it, because the constructor will need the things
        //println('{(x: Boolean) => new Bar[T](x)()}.asTerm)
        /*
        // can't say I know what this is about tbf
        Inlined(Ident(BridgeImpl),List(),
            Block(
                List(
                    DefDef(
                        // I'm guessing this needs to be uniquely generated
                        $anonfun,
                        List(List(ValDef(x,Ident(Boolean),EmptyTree))),
                        // this is just the type R
                        TypeTree[AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class experimental)),object generic),Bar),
                                             List(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)))],
                        Block(List(),
                            Apply(
                                Apply(
                                    TypeApply(
                                        Select(
                                            New(
                                                // seemingly also the type R (slightly different form)
                                                AppliedTypeTree(
                                                    Ident(Bar),
                                                    List(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)])
                                                )
                                            ),
                                            <init>
                                        ),
                                        // arguments to R, would need to break the tree
                                        List(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)])
                                    ),
                                    List(Ident(x))
                                ),
                                List(
                                    Apply(
                                        // apparently, this has generics applied to it too? interesting
                                        TypeApply(
                                            Select(Ident(Bar),$lessinit$greater$default$2),
                                            List(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)])
                                        ),
                                        // so, turns out a default argument is given all the curried arguments before it as arguments
                                        // when default arguments come before us, it's not clear if that's given a block val?
                                        // the bytecode suggests so, but it would be good to see an AST example
                                        List(Ident(x))
                                    )
                                )
                            )
                        )
                    )
                ),
                Closure(List(),Ident($anonfun),EmptyTree)
            )
        )
        */
        '{???}
    }

    private object Bridgeable {
        def unapply(ty: TypeRepr): Option[(Symbol, List[Symbol], List[Symbol], List[List[Symbol]])] = {
            val clsDef = ty.classSymbol
            clsDef match {
                case Some(cls) =>
                    val primCon = cls.primaryConstructor
                    if (primCon.isNoSymbol) None
                    else {
                        // some of the arguments lists may be type introductions
                        // we should filter those out and handle separately
                        val (tyParamss, valParamss) = primCon.paramSymss.partition(_.forall(_.isType))
                        valParamss match {
                            // TODO: this .flatten might not work with curried types; but they don't exist yet?
                            case bridgeParams :: otherParams => Some((cls, tyParamss.flatten, bridgeParams, otherParams))
                            case Nil                         => Some((cls, tyParamss.flatten, Nil, Nil))
                        }
                    }
                case None => None
            }
        }
    }
}

class Bar[A](val x: Boolean)(val y: String = "hello world")
