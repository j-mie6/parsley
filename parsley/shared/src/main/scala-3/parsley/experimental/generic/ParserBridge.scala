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
private def bridgeImpl[T: Type, S >: T: Type](using Quotes): Expr[ErrorBridge] = BridgeImpl().synthesise[T, S]
// having a class here simplifies the importing of quotes.reflect.* for the enum
// (FIXME: it is considered back practice, so I will probably just make a parametric enum later)
private class BridgeImpl(using Quotes) {
    import quotes.reflect.*
    private enum BridgeArg {
        case Pos
        case Default(n: Int, sym: Symbol)
        case Err(name: String)
    }

    def synthesise[T: Type, S >: T: Type] = {
        val tyRepr = TypeRepr.of[T]
        val tyArgs = tyRepr.typeArgs
        tyRepr match {
            case Bridgeable(cls, tyParams, bridgeParams, otherParams) =>
                val numBridgeParams = bridgeParams.length
                val categorisedArgs = categoriseArgs(cls, otherParams, numBridgeParams + 1)
                // Used for the types of the lambda passed to combinator
                val bridgeTyArgs = bridgeParams.collect {
                    case sym if !isPos(sym.termRef) => sym.termRef.typeSymbol.typeRef.substituteTypes(tyParams, tyArgs)
                }
                // TODO: look for unique position
                // TODO: ensure validation if Err is encounted (report separately, but then abort if failed (Option))
                bridgeTyArgs.map(_.asType) match {
                    case List('[t1]) =>
                        '{new Bridge1[t1, S] {
                            //  ap1(pos.map(con), x)
                            def apply(x1: Parsley[t1]): Parsley[S] = x1.map(${constructor1[t1, T](cls, tyArgs, categorisedArgs)})
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

    // NOT handling positions yet
    // Everything but first set of brackets have defaults
    private def constructor1[T: Type, R: Type](cls: Symbol, clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]]): Expr[T => R] = {
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
                        /* snip, the body */
                    )
                ),
                Closure(List(),Ident($anonfun),EmptyTree)
            )
        )
        */
        '{(x: T) => ${
            val qx = 'x.asTerm
            val tys: List[TypeTree] = clsTyArgs.map(tyRep => TypeTree.of(using tyRep.asType))
            val objTy = New(Applied(TypeTree.ref(cls), tys))
            val con = objTy.select(cls.primaryConstructor).appliedToTypes(clsTyArgs)
            val conBridged = con.appliedTo(qx)
            // at this point, we have applied the constructor to the bridge args (except for positions)
            // we now need to apply the other default arguments
            // Each default argument takes all the previous sets of arguments (flattened).
            // this means old default arguments will need to be stored in vals within an enclosing block.
            // we'll need to collect the references to all these into a ListBuffer, which will be repeatedly
            // toList'd as applications are formed.
            def defBindings(owner: Symbol, paramss: List[List[BridgeArg]], seeds: mutable.ListBuffer[Term], defaults: mutable.ListBuffer[List[Term]])(k: List[List[Term]] => Term): Term = paramss match {
                case Nil => k(defaults.toList)
                case params :: paramss =>
                    val mySeeds = seeds.toList
                    val terms = for param <- params yield param match
                        case BridgeArg.Pos => report.errorAndAbort("positions are currently not supported in bridges")
                        case BridgeArg.Default(n, sym) =>
                            Ident(cls.companionModule.termRef).select(sym).appliedToTypes(clsTyArgs).appliedToArgs(mySeeds)
                        case BridgeArg.Err(name) =>
                            report.error(s"Argument $name for class ${cls.name} is neither a default or position outside of the primary arguments, a bridge cannot be formed")
                            // FIXME: need to deal with error gace semi-gracefully at the base case
                            ???

                    ValDef.let(owner, terms) { xs =>
                        defBindings(owner, paramss, seeds ++= xs, defaults += xs)(k)
                    }
            }

            val saturated = defBindings(qx.symbol.owner, otherArgs, mutable.ListBuffer.from(List(qx)), mutable.ListBuffer.empty) { defaults =>
                conBridged.appliedToArgss(defaults)
            }
            println(saturated)
            saturated.asExprOf[R]
        }}
    }

    private object Bridgeable {
        def unapply(ty: TypeRepr): Option[(Symbol, List[Symbol], List[Symbol], List[List[Symbol]])] = {
            val clsDef = ty.classSymbol
            clsDef match {
                case Some(cls) =>
                    val primCon = cls.primaryConstructor
                    Option.when(!primCon.isNoSymbol) {
                        // some of the arguments lists may be type introductions
                        // we should filter those out and handle separately
                        val (tyParamss, valParamss) = primCon.paramSymss.partition(_.forall(_.isType))
                        valParamss match {
                            // TODO: this .flatten might not work with curried types; but they don't exist yet?
                            // (might have to be careful with extension methods too, but extension bridges seem... weird)
                            case bridgeParams :: otherParams => (cls, tyParamss.flatten, bridgeParams, otherParams)
                            case Nil                         => (cls, tyParamss.flatten, Nil, Nil)
                        }
                    }
                case None => None
            }
        }
    }
}
