/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package experimental.generic

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.quoted.*
import bridges.ErrorBridge

inline transparent def bridge[T]: ErrorBridge = bridge[T, T]
inline transparent def bridge[T, S >: T]: ErrorBridge = ${bridgeImpl[T, S]('Nil, 'None)}

// TODO: consider how to incorporate the errors in cleanly, this sucks
inline transparent def bridgeErr[T](labels: List[String], reason: Option[String]): ErrorBridge = bridgeErr[T, T](labels, reason)
inline transparent def bridgeErr[T, S >: T](labels: List[String], reason: Option[String]): ErrorBridge = ${bridgeImpl[T, S]('labels, 'reason)}

private def bridgeImpl[T: Type, S >: T: Type](labels: Expr[List[String]], reason: Expr[Option[String]])(using Quotes): Expr[ErrorBridge] = {
    BridgeImpl().synthesise[T, S](labels, reason)
}

// this is annoying, but needs to be available publically, otherwise macros can't see it
transparent trait InternalMethodLeak { this: bridges.SingletonBridge[?] =>
    def macroImplLiftedWrap[A](p: Parsley[A]) = error(p.ut()).uo(name)
}

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

    def synthesise[T: Type, S >: T: Type](labels: Expr[List[String]], reason: Expr[Option[String]]) = {
        val tyRepr = TypeRepr.of[T]
        val tyArgs = tyRepr.typeArgs
        tyRepr match {
            case Bridgeable(cls, tyParams, bridgeParams, otherParams) =>
                val categorisedArgs = categoriseArgs(cls, bridgeParams :: otherParams, 1, primary = true, mutable.ListBuffer.empty)
                // Used for the types of the lambda passed to combinator
                //println(categorisedArgs)
                val bridgePrimaryArgs = bridgeParams.collect {
                    case sym if isPos(sym).isEmpty => (sym.name, tyRepr.memberType(sym).substituteTypes(tyParams, tyArgs))
                }
                val existsUniquePosition = categorisedArgs.flatten.foldLeft(Option.empty[PosImpl[?]]) {
                    case (None, BridgeArg.Pos(impl)) => Some(impl)
                    case (Some(_), BridgeArg.Pos(_)) => report.errorAndAbort("When `Pos` appears in a bridged type, it must be unique")
                    case (pos, _)                    => pos
                }
                val con = constructor[T](cls, bridgePrimaryArgs, tyArgs, categorisedArgs, existsUniquePosition.map(_.tyRepr))
                val lift = synthesiseLift[S](existsUniquePosition, bridgePrimaryArgs.map(_._2), con, _)
                val from = [Fn] => { (fnTy: Type[Fn]) =>
                    given Type[Fn] = fnTy
                    val curriedCon = curriedConstructor[Fn, T](cls, bridgePrimaryArgs, tyArgs, categorisedArgs, existsUniquePosition.map(_.tyRepr))
                    synthesiseSingle[Fn](existsUniquePosition, curriedCon)
                }
                // TODO: ensure validation if Err is encountered (report separately, but then abort if failed (Option))
                synthesiseBridge[S](tyRepr.typeSymbol.name, bridgePrimaryArgs.map(_._2.asType), lift, from, labels, reason)
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
    private def categoriseArgs(cls: Symbol, nonPrimaryArgs: List[List[Symbol]], n: Int, primary: Boolean, buf: mutable.ListBuffer[List[BridgeArg]]): List[List[BridgeArg]] = nonPrimaryArgs match {
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

    // FIXME: I don't like the duplication here...
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

    /** Constructs a lambda for a constructor applied to defaults and threading required metadata
      *
      * @param cls the class for the constructor
      * @param lamArgs the arguments for the lambda (without positions or defaulted)
      * @param clsTyArgs the type parameters provided to the constructor
      * @param otherArgs any remaining non-primary arguments
      * @param posRepr the position
      * @return a lambda of the form `pos => (lamArgs..) => cls[clsTyArgs](..)(otherArgs)`
      */
    private def curriedConstructor[Fn: Type, R: Type](cls: Symbol, lamArgs: List[(String, TypeRepr)], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], posRepr: Option[TypeRepr]): Term = {
        val (paramNames, lamTys) = lamArgs.unzip
        // grrrrrrrr why has Scala given me Tree and not Term?!
        def inner(owner: Symbol, posParam: Option[Term]) = Lambda(owner, MethodType(paramNames)(_ => lamTys, _ => TypeRepr.of[R]), { (lamSym, params) =>
            val paramTerms = params.map(_.asExpr.asTerm)
            appliedCon(cls, lamSym, paramTerms.toVector, clsTyArgs, otherArgs, posParam)
        })
        posRepr.fold(inner(Symbol.spliceOwner, None)) { posRepr =>
            Lambda(Symbol.spliceOwner, MethodType(List("pos"))(_ => List(posRepr), _ => TypeRepr.of[Fn]), { (outerLamSym, posParam) =>
                // grrrrrrrr why has Scala given me Tree and not Term?!
                inner(outerLamSym, posParam.map(_.asExpr.asTerm).headOption)
            })
        }
    }

    private def appliedCon(cls: Symbol, owner: Symbol, lamParams: IndexedSeq[Term], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], posParam: Option[Term]): Term = {
        val tys: List[TypeTree] = clsTyArgs.map(tyRep => TypeTree.of(using tyRep.asType))
        val objTy = if tys.nonEmpty then New(Applied(TypeTree.ref(cls), tys)) else New(TypeTree.ref(cls))
        val con = objTy.select(cls.primaryConstructor).appliedToTypes(clsTyArgs)
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

    private def synthesiseLift[R: Type](existsUniquePosition: Option[PosImpl[?]], argTys: List[TypeRepr], con: Term, args: List[Term]): Expr[Parsley[R]] = {
        val tys = argTys :+ TypeRepr.of[R]
        val arity = argTys.size + existsUniquePosition.size
        TypeRepr.of[parsley.lift.type].typeSymbol.methodMember(s"lift$arity").headOption.map('{parsley.lift}.asTerm.select) match {
            case Some(lift) => existsUniquePosition match {
                case Some(impl@PosImpl(_, given Type[posTy])) =>
                    val posTyRepr = TypeRepr.of[posTy]
                    lift.appliedToTypes(posTyRepr :: tys)
                        .appliedToArgs(con :: impl.parser.asTerm :: args)
                        .asExprOf[Parsley[R]]
                case None =>
                    lift.appliedToTypes(tys)
                        .appliedToArgs(con :: args)
                        .asExprOf[Parsley[R]]
            }
            case None => report.errorAndAbort(s"No `lift` available for arity $arity")
        }
    }

    private def synthesiseSingle[R: Type](existsUniquePosition: Option[PosImpl[?]], con: Term): Expr[Parsley[R]] = existsUniquePosition match {
        case Some(impl@PosImpl(_, given Type[posTy])) => '{${impl.parser}.map[R](${con.asExprOf[posTy => R]})}
        case None => '{Parsley.pure[R](${con.asExprOf[R]})}
    }

    private def synthesiseBridge[R: Type](n: String, argTys: List[Type[?]], lift: List[Term] => Expr[Parsley[R]], single: [T] => Type[T] => Expr[Parsley[T]], errLabels: Expr[List[String]], errReason: Expr[Option[String]]): Expr[ErrorBridge] = (argTys.size: @switch) match {
        // TODO: make generation of labels/reason conditional as to not bloat the objects
        case 1 => (argTys: @unchecked) match {
            case List('[t1]) => '{
                new bridges.Bridge1[t1, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1]): Parsley[R] = macroImplLiftedWrap(${lift(List('p1.asTerm))})
                    def singleton: Parsley[t1 => R] = ${single(Type.of[t1 => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 2 => (argTys: @unchecked) match {
            case List('[t1], '[t2]) => '{
                new bridges.Bridge2[t1, t2, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm))}
                    def singleton: Parsley[(t1, t2) => R] =
                        ${single(Type.of[(t1, t2) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 3 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3]) => '{
                new bridges.Bridge3[t1, t2, t3, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm))}
                    def singleton: Parsley[(t1, t2, t3) => R] =
                        ${single(Type.of[(t1, t2, t3) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 4 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4]) => '{
                new bridges.Bridge4[t1, t2, t3, t4, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4) => R] =
                        ${single(Type.of[(t1, t2, t3, t4) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 5 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5]) => '{
                new bridges.Bridge5[t1, t2, t3, t4, t5, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 6 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6]) => '{
                new bridges.Bridge6[t1, t2, t3, t4, t5, t6, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 7 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7]) => '{
                new bridges.Bridge7[t1, t2, t3, t4, t5, t6, t7, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 8 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8]) => '{
                new bridges.Bridge8[t1, t2, t3, t4, t5, t6, t7, t8, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 9 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9]) => '{
                new bridges.Bridge9[t1, t2, t3, t4, t5, t6, t7, t8, t9, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 10 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10]) => '{
                new bridges.Bridge10[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 11 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11]) => '{
                new bridges.Bridge11[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 12 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12]) => '{
                new bridges.Bridge12[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 13 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13]) => '{
                new bridges.Bridge13[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 14 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14]) => '{
                new bridges.Bridge14[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 15 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15]) => '{
                new bridges.Bridge15[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 16 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16]) => '{
                new bridges.Bridge16[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 17 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16], '[t17]) => '{
                new bridges.Bridge17[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16], p17: =>Parsley[t17]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm, 'p17.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 18 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16], '[t17], '[t18]) => '{
                new bridges.Bridge18[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16], p17: =>Parsley[t17], p18: =>Parsley[t18]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm, 'p17.asTerm, 'p18.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 19 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16], '[t17], '[t18], '[t19]) => '{
                new bridges.Bridge19[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16], p17: =>Parsley[t17], p18: =>Parsley[t18], p19: =>Parsley[t19]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm, 'p17.asTerm, 'p18.asTerm, 'p19.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 20 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16], '[t17], '[t18], '[t19], '[t20]) => '{
                new bridges.Bridge20[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16], p17: =>Parsley[t17], p18: =>Parsley[t18], p19: =>Parsley[t19], p20: =>Parsley[t20]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm, 'p17.asTerm, 'p18.asTerm, 'p19.asTerm, 'p20.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 21 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16], '[t17], '[t18], '[t19], '[t20], '[t21]) => '{
                new bridges.Bridge21[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16], p17: =>Parsley[t17], p18: =>Parsley[t18], p19: =>Parsley[t19], p20: =>Parsley[t20],
                              p21: =>Parsley[t21]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm, 'p17.asTerm, 'p18.asTerm, 'p19.asTerm, 'p20.asTerm,
                                    'p21.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case 22 => (argTys: @unchecked) match {
            case List('[t1], '[t2], '[t3], '[t4], '[t5], '[t6], '[t7], '[t8], '[t9], '[t10], '[t11], '[t12], '[t13], '[t14], '[t15], '[t16], '[t17], '[t18], '[t19], '[t20], '[t21], '[t22]) => '{
                new bridges.Bridge22[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, R] with InternalMethodLeak {
                    def apply(p1: Parsley[t1], p2: =>Parsley[t2], p3: =>Parsley[t3], p4: =>Parsley[t4], p5: =>Parsley[t5],
                              p6: =>Parsley[t6], p7: =>Parsley[t7], p8: =>Parsley[t8], p9: =>Parsley[t9], p10: =>Parsley[t10],
                              p11: =>Parsley[t11], p12: =>Parsley[t12], p13: =>Parsley[t13], p14: =>Parsley[t14], p15: =>Parsley[t15],
                              p16: =>Parsley[t16], p17: =>Parsley[t17], p18: =>Parsley[t18], p19: =>Parsley[t19], p20: =>Parsley[t20],
                              p21: =>Parsley[t21], p22: =>Parsley[t22]): Parsley[R] = macroImplLiftedWrap:
                        ${lift(List('p1.asTerm, 'p2.asTerm, 'p3.asTerm, 'p4.asTerm, 'p5.asTerm,
                                    'p6.asTerm, 'p7.asTerm, 'p8.asTerm, 'p9.asTerm, 'p10.asTerm,
                                    'p11.asTerm, 'p12.asTerm, 'p13.asTerm, 'p14.asTerm, 'p15.asTerm,
                                    'p16.asTerm, 'p17.asTerm, 'p18.asTerm, 'p19.asTerm, 'p20.asTerm,
                                    'p21.asTerm, 'p22.asTerm))}
                    def singleton: Parsley[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) => R] =
                        ${single(Type.of[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) => R])}
                    override def labels: List[String] = $errLabels
                    override def reason: Option[String] = $errReason
                    override protected def name = ${Expr(n)}
                }
            }
        }
        case _ => report.errorAndAbort("Bridges cannot have more than 22 arguments")
    }

    // TODO: use this generalised synthesis method once Symbol.newClass and ClassDef are no longer marked experimental
    // TODO: only does lift synthesis, not the singleton/errors/etc
    /*
    @experimental
    private def synthesiseBridge[R: Type](argTys: List[TypeRepr], body: List[Term] => Expr[Parsley[R]]): Expr[ErrorBridge] = {
        val arity = argTys.size
        // TODO: is there a better way to retrieve the bridge with desired arity?
        val bridgeTy = TypeTree.ref(TypeRepr.of[bridges.type].typeSymbol.typeMember(s"Bridge$arity"))

        // BridgeN[T1, ..., TN, R]
        val parents = List(Applied(bridgeTy, argTys.map(Inferred(_)) :+ TypeTree.of[R]))

        // def apply(p1: Parsley[T1], ..., pN: parsley[TN]): Parsley[R]
        def decls(cls: Symbol): List[Symbol] =
            List(Symbol.newMethod(cls, "apply", MethodType(
                paramNames = (1 to arity).map(i => s"p$i").toList
            )(
                paramInfosExp = _ => argTys.map(TypeRepr.of[Parsley].appliedTo(_)),
                resultTypeExp = _ => TypeRepr.of[Parsley].appliedTo(TypeRepr.of[R])
            )))

        val bridgeCls = Symbol.newClass(Symbol.spliceOwner, "$anon", parents.map(_.tpe), decls, selfType = None)

        // def apply(...) = ${ body(List('p1.asTerm, ..., 'pN.asTerm)) }
        val applySym = bridgeCls.declaredMethod("apply").head
        val applyDef = DefDef(applySym, argss => {
            val args = argss.head.map {
                case t: Term => t
                case tree    => report.errorAndAbort(s"Expected term while synthesising arguments for the apply method, got ${tree.show} instead")
            }
            Some(body(args).asTerm.changeOwner(applySym))
        })

        // class $anon extends BridgeN[T1, ..., TN, R] { def apply(...) = ... }
        val bridgeClsDef = ClassDef(bridgeCls, parents, body = List(applyDef))
        // new $anon(): BridgeN[T1, ..., TN, R]
        val newBridgeCls = Typed(Apply(Select(New(TypeIdent(bridgeCls)), bridgeCls.primaryConstructor), Nil), parents.head)

        Block(List(bridgeClsDef), newBridgeCls).asExprOf[ErrorBridge]
    }
    */

    private object Bridgeable {
        def unapply(ty: TypeRepr): Option[(Symbol, List[Symbol], List[Symbol], List[List[Symbol]])] = ty.classSymbol.flatMap { cls =>
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
        }
    }
}
