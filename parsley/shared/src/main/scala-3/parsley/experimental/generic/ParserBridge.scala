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
// (FIXME: it is considered bad practice, so I will probably just make a parametric enum later)
private class BridgeImpl(using Quotes) {
    import quotes.reflect.*
    private enum BridgeArg {
        case Pos(impl: PosImpl[?])
        case Bridged(sym: Symbol)
        case Default(n: Int, sym: Symbol)
        case Err(name: String, pos: Option[Position])
    }

    def synthesise[T: Type, S >: T: Type] = {
        val tyRepr = TypeRepr.of[T]
        val tyArgs = tyRepr.typeArgs
        tyRepr match {
            case Bridgeable(cls, tyParams, bridgeParams, otherParams) =>
                val categorisedArgs = categoriseArgs(cls, bridgeParams :: otherParams, 1, primary = true)
                // Used for the types of the lambda passed to combinator
                println(categorisedArgs)
                val bridgePrimaryArgs = bridgeParams.collect {
                    case sym if isPos(sym).isEmpty => (sym.name, sym.termRef.typeSymbol.typeRef.substituteTypes(tyParams, tyArgs))
                }
                val existsUniquePosition = categorisedArgs.flatten.foldLeft(Option.empty[PosImpl[?]]) {
                    case (None, BridgeArg.Pos(impl)) => Some(impl)
                    case (Some(_), BridgeArg.Pos(_)) => report.errorAndAbort("When `Pos` appears in a bridged type, it must be unique")
                    case (pos, _)                    => pos
                }
                val con = constructor[T](cls, bridgePrimaryArgs, tyArgs, categorisedArgs, existsUniquePosition.map(_.tyRepr))
                // TODO: look for unique position
                // TODO: ensure validation if Err is encountered (report separately, but then abort if failed (Option))
                bridgePrimaryArgs.map(_._2.asType) match {
                    case List('[t1]) =>
                        '{new Bridge1[t1, S] {
                            def apply(p1: Parsley[t1]): Parsley[S] = ${
                                existsUniquePosition match {
                                    case Some(impl@PosImpl(_, given Type[posTy])) =>
                                        '{lift.lift2(${con.asExprOf[(posTy, t1) => T]}, ${impl.parser}, p1)}
                                    case None => '{p1.map(${con.asExprOf[t1 => T]})}
                                }
                            }
                        }}
                    case List('[t1], '[t2]) =>
                        '{new Bridge2[t1, t2, S] {
                            def apply(p1: Parsley[t1], p2: Parsley[t2]): Parsley[S] = ${
                                existsUniquePosition match {
                                    case Some(impl@PosImpl(_, given Type[posTy])) =>
                                        '{lift.lift3(${con.asExprOf[(posTy, t1, t2) => T]}, ${impl.parser}, p1, p2)}
                                    case None => '{lift.lift2(${con.asExprOf[(t1, t2) => T]}, p1, p2)}
                                }
                            }
                        }}
                    // TODO: 19 more of these
                    case _ => '{???}
                }
            case _ => report.errorAndAbort("can only make bridges for constructible classes or objects")
        }
    }

    private case class PosImpl[T: Type](inst: Expr[PositionLike[T]], ty: Type[T]) {
        def parser: Expr[Parsley[T]] = '{$inst.pos}
        def tyRepr = TypeRepr.of[T]
    }
    private val annotation = TypeRepr.of[parsley.experimental.generic.isPosition].typeSymbol
    private def isPos(sym: Symbol): Option[PosImpl[?]] = Option.when(sym.hasAnnotation(annotation)) {
        sym.termRef.widen.asType match {
            case ty@'[t] => Expr.summon[parsley.experimental.generic.PositionLike[t]] match {
                case Some(inst) => PosImpl[t](inst, ty)
                case None =>
                    val typeName = TypeRepr.of[t].show(using Printer.TypeReprShortCode)
                    report.errorAndAbort(s"attribute ${sym.name} can only use @isPosition with a `parsley.generic.PositionLike[$typeName]` instance in scope", sym.pos.get)
            }
        }
    }

    private def defaultName(n: Int) = s"$$lessinit$$greater$$default$$$n"
    private def defaulted(cls: Symbol, n: Int): Option[Symbol] = cls.companionModule.declaredMethod(defaultName(n)).headOption

    @tailrec
    private def categoriseArgs(cls: Symbol, nonPrimaryArgs: List[List[Symbol]], n: Int, primary: Boolean, buf: mutable.ListBuffer[List[BridgeArg]] = mutable.ListBuffer.empty): List[List[BridgeArg]] = nonPrimaryArgs match {
        case Nil => buf.toList
        case args :: restArgs =>
            categoriseArgs(cls, restArgs, n + args.length, primary = false, buf += args.zipWithIndex.map {
                case (sym, i) => defaulted(cls, i + n) match {
                    // TODO: if it's primary, you could actually synthesise a default to the lifted constructor
                    case Some(sym) if !primary => BridgeArg.Default(i + n, sym)
                    case _ => isPos(sym) match {
                        case Some(impl)       => BridgeArg.Pos(impl)
                        case None if !primary => BridgeArg.Err(sym.name, sym.pos)
                        case None             => BridgeArg.Bridged(sym)
                    }
                }
            })
    }

    /** Constructs a lambda for a constructor applied to defaults and threading required metadata
      *
      * @param cls the class for the constructor
      * @param lamArgs the arguments for the lambda (without positions or defaulted)
      * @param clsTyArgs the type parameters provided to the constructor
      * @param otherArgs any remaining non-primary arguments
      * @param posRepr the position
      * @return a lambda of the form `(lamArgs..) => cls[clsTyArgs](..)(otherArgs)`
      */
    private def constructor[R: Type](cls: Symbol, lamArgs: List[(String, TypeRepr)], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], posRepr: Option[TypeRepr]): Term = {
        val requiresPosition = posRepr.isDefined
        val (paramNames, lamTys) = (posRepr.map("pos" -> _) ++: lamArgs).unzip
        // grrrrrrrr why has Scala given me Tree and not Term?!
        Lambda(Symbol.spliceOwner, MethodType(paramNames)(_ => lamTys, _ => TypeRepr.of[R]), { (lamSym, params) =>
            val paramTerms = params.map(_.asExpr.asTerm)
            val (posParam, paramTermsWithoutPos) = paramTerms match
                case posParam :: paramTerms if requiresPosition => (Some(posParam), paramTerms)
                case paramTerms => (None, paramTerms)
            appliedCon(cls, lamSym, paramTermsWithoutPos.toVector, clsTyArgs, otherArgs, posParam)
        })
    }

    private def appliedCon(cls: Symbol, owner: Symbol, lamParams: IndexedSeq[Term], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], posParam: Option[Term]): Term = {
        val tys: List[TypeTree] = clsTyArgs.map(tyRep => TypeTree.of(using tyRep.asType))
        val objTy = New(Applied(TypeTree.ref(cls), tys))
        val con = objTy.select(cls.primaryConstructor).appliedToTypes(clsTyArgs)
        //val conBridged = con.appliedToArgs(params) // TODO: this doesn't work when positions can be in the first set
        val kaboom: Term = '{???}.asTerm
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
                var i = 0 // FIXME: get rid of this
                val terms = params.map {
                    case BridgeArg.Pos(_) => posParam.get
                    case BridgeArg.Default(n, sym) =>
                        Ident(cls.companionModule.termRef).select(sym).appliedToTypes(clsTyArgs).appliedToArgss(mySeeds)
                    case BridgeArg.Err(name, pos) =>
                        report.error(s"Argument $name for class ${cls.name} is neither a default or position outside of the primary arguments, a bridge cannot be formed", pos.get)
                        kaboom
                    case BridgeArg.Bridged(_) =>
                        val p = lamParams(i)
                        i += 1
                        p
                }
                ValDef.let(owner, terms) { xs =>
                    defBindings(owner, paramss, seeds += xs, extras += xs)(k)
                }
        }
        val saturated = defBindings(owner, otherArgs, mutable.ListBuffer.empty, mutable.ListBuffer.empty) { defaults =>
            con.appliedToArgss(defaults)
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
