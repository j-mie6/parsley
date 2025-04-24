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
        case Pos(impl: PosImpl[?])
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
                val bridgeTyArgs = bridgeParams.map /*collect*/ {
                    // FIXME: switch back to collect and handle this properly
                    case sym if isPos(sym).nonEmpty => report.errorAndAbort("Currently, `Pos` cannot appear in the first set of arguments")
                    case sym /*if !isPos(sym.termRef)*/ => (sym.name, sym.termRef.typeSymbol.typeRef.substituteTypes(tyParams, tyArgs))
                }
                val existsUniquePosition = categorisedArgs.flatten.foldLeft(Option.empty[PosImpl[?]]) {
                    case (None, BridgeArg.Pos(impl)) => Some(impl)
                    case (Some(_), BridgeArg.Pos(_)) => report.errorAndAbort("When `Pos` appears in a bridged type, it must be unique")
                    case (pos, _)                    => pos
                }
                // TODO: look for unique position
                // TODO: ensure validation if Err is encountered (report separately, but then abort if failed (Option))
                bridgeTyArgs.map(_._2.asType) match {
                    case List('[t1]) =>
                        '{new Bridge1[t1, S] {
                            def apply(x1: Parsley[t1]): Parsley[S] = ${
                                val con = constructor[T](cls, bridgeTyArgs, tyArgs, categorisedArgs, existsUniquePosition.map(_.tyRepr))
                                existsUniquePosition match {
                                    case Some(impl@PosImpl(_, given Type[posTy])) =>
                                        '{lift.lift2(${con.asExprOf[(posTy, t1) => T]}, ${impl.parser}, x1)}
                                    case None => '{x1.map(${con.asExprOf[t1 => T]})}
                                }
                            }
                        }}
                    case List('[t1], '[t2]) => '{new Bridge2[t1, t2, S] {}}
                    // TODO: 20 more of these
                    case _ => '{???}
                }
            case _ => report.errorAndAbort("can only make bridges for constructible classes or objects")
        }
    }

    private case class PosImpl[T: Type](inst: Expr[PositionLike[T]], ty: Type[T]) {
        def parser: Expr[Parsley[T]] = '{$inst.pos}
        def tyRepr = TypeRepr.of[T]
    }
    private def isPos(sym: Symbol): Option[PosImpl[?]] = {
        val hasAnnotation = sym.hasAnnotation(TypeRepr.of[parsley.experimental.generic.isPosition].typeSymbol)
        sym.termRef.widen.asType match {
            case ty@'[t] if hasAnnotation => Expr.summon[parsley.experimental.generic.PositionLike[t]] match {
                case Some(inst) => Some(PosImpl[t](inst, ty))
                case None => report.errorAndAbort(s"attribute ${sym.name} can only be annotated with @isPosition if it is `PositionLike`")
            }
            case _ => None
        }
    }

    private def defaultName(n: Int) = s"$$lessinit$$greater$$default$$$n"
    private def defaulted(cls: Symbol, n: Int): Option[Symbol] = cls.companionModule.declaredMethod(defaultName(n)).headOption

    @tailrec
    private def categoriseArgs(cls: Symbol, nonPrimaryArgs: List[List[Symbol]], n: Int, buf: mutable.ListBuffer[List[BridgeArg]] = mutable.ListBuffer.empty): List[List[BridgeArg]] = nonPrimaryArgs match {
        case Nil => buf.toList
        case args :: restArgs =>
            categoriseArgs(cls, restArgs, n + args.length, buf += args.zipWithIndex.map {
                case (sym, i) => defaulted(cls, i + n) match {
                    case Some(sym) => BridgeArg.Default(i + n, sym)
                    case None => isPos(sym) match {
                        case Some(impl) => BridgeArg.Pos(impl)
                        case None => BridgeArg.Err(sym.name)
                    }
                }
            })
    }

    private def prependIf[A, B >: A](b: Boolean, x: =>B, xs: List[A]): List[B] = if b then x :: xs else xs

    // NOT handling positions yet
    private def constructor[R: Type](cls: Symbol, lamArgs: List[(String, TypeRepr)], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], posRepr: Option[TypeRepr]): Term = {
        val requiresPosition = posRepr.isDefined
        val (paramNames, lamTys) = prependIf(requiresPosition, "pos" -> posRepr.get, lamArgs).unzip
        // grrrrrrrr why has Scala given me Tree and not Term?!
        Lambda(Symbol.spliceOwner, MethodType(paramNames)(_ => lamTys, _ => TypeRepr.of[R]), { (lamSym, params) =>
            val paramTerms = params.map(_.asExpr.asTerm)
            val (posParam, paramTermsWithoutPos) = paramTerms match
                case posParam :: paramTerms if requiresPosition => (Some(posParam), paramTerms)
                case paramTerms => (None, paramTerms)
            appliedCon(cls, lamSym, paramTermsWithoutPos, clsTyArgs, otherArgs, posParam)
        })
    }

    private def appliedCon(cls: Symbol, owner: Symbol, params: List[Term], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], posParam: Option[Term]): Term = {
        val tys: List[TypeTree] = clsTyArgs.map(tyRep => TypeTree.of(using tyRep.asType))
        val objTy = New(Applied(TypeTree.ref(cls), tys))
        val con = objTy.select(cls.primaryConstructor).appliedToTypes(clsTyArgs)
        val conBridged = con.appliedToArgs(params) // TODO: this doesn't work when positions can be in the first set
        // at this point, we have applied the constructor to the bridge args (except for positions)
        // we now need to apply the other default arguments
        // Each default argument takes all the previous sets of arguments (flattened).
        // this means old default arguments will need to be stored in vals within an enclosing block.
        // we'll need to collect the references to all these into a ListBuffer, which will be repeatedly
        // toList'd as applications are formed.
        def defBindings(owner: Symbol, paramss: List[List[BridgeArg]], seeds: mutable.ListBuffer[List[Term]], extras: mutable.ListBuffer[List[Term]])(k: List[List[Term]] => Term): Term = paramss match {
            case Nil => k(extras.toList)
            case params :: paramss =>
                val mySeeds = seeds.toList
                val terms = params.map {
                    case BridgeArg.Pos(_) => posParam.get
                    case BridgeArg.Default(n, sym) =>
                        Ident(cls.companionModule.termRef).select(sym).appliedToTypes(clsTyArgs).appliedToArgss(mySeeds)
                    case BridgeArg.Err(name) =>
                        report.error(s"Argument $name for class ${cls.name} is neither a default or position outside of the primary arguments, a bridge cannot be formed")
                        // FIXME: need to deal with error gace semi-gracefully at the base case
                        ???
                }
                ValDef.let(owner, terms) { xs =>
                    defBindings(owner, paramss, seeds += xs, extras += xs)(k)
                }
        }
        val saturated = defBindings(owner, otherArgs, mutable.ListBuffer(params), mutable.ListBuffer.empty) { defaults =>
            conBridged.appliedToArgss(defaults)
        }
        saturated
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
