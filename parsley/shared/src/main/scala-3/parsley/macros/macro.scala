/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package macros

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.quoted.*
import bridges.ErrorBridge

// this is annoying, but needs to be available publically, otherwise macros can't see it
transparent trait InternalMethodLeak { this: bridges.SingletonBridge[?] =>
    def macroImplLiftedWrap[A](p: Parsley[A]) = error(p.ut()).uo(name)
}

// having a class here simplifies the importing of quotes.reflect.* for the enum
// (FIXME: it is considered bad practice, so I will probably just make a parametric enum later)
private class BridgeImpl(using Quotes) {
    import quotes.reflect.*
    private enum BridgeArg {
        case Meta(impl: MetaImpl[?])
        case Bridged(sym: Symbol)
        case Default(n: Int, sym: Symbol)
        case Err(name: String, pos: Option[Position])
    }

    def synthesise[T: Type, S >: T: Type](labels: Expr[List[String]], reason: Expr[Option[String]]) = {
        // dealias here removes any type aliases which could get in the way of proper synthesis
        val tyRepr = TypeRepr.of[T].dealias
        val tyArgs = tyRepr.typeArgs
        tyRepr match {
            // there must be the same number of type arguments as type params, or this is a higher-kinded T (oops!)
            case Bridgeable(cls, tyParams, bridgeParams, otherParams) if tyArgs.lengthCompare(tyParams) == 0 =>
                def contextualise(ty: TypeRepr) = ty.substituteTypes(tyParams, tyArgs)
                val categorisedArgs = categoriseArgs(cls, bridgeParams :: otherParams, 1, primary = true, mutable.ListBuffer.empty, contextualise)
                // Used for the types of the lambda passed to combinator
                //println(categorisedArgs)
                val bridgePrimaryArgs = bridgeParams.collect {
                    case sym if !hasMeta(sym) => (sym.name, contextualise(tyRepr.memberType(sym)))
                }
                val (metaTerms, metaReprs) = categorisedArgs.flatten.collect {
                    case BridgeArg.Meta(impl) => (impl.parser.asTerm, impl.tyRepr)
                }.unzip
                lazy val con = constructor[T](cls, bridgePrimaryArgs, tyArgs, categorisedArgs, metaReprs)
                val lift = (terms: List[Term]) => synthesiseLift[S](metaReprs ::: bridgePrimaryArgs.map(_._2), con, metaTerms ::: terms)
                val from = [Fn] => { (fnTy: Type[Fn]) =>
                    given Type[Fn] = fnTy
                    val (curriedCon, useFresh) = curriedConstructor[Fn, T](cls, bridgePrimaryArgs, tyArgs, categorisedArgs, metaReprs)
                    synthesiseLift[Fn](metaReprs, curriedCon, metaTerms, useFresh)
                }
                // TODO: ensure validation if Err is encountered (report separately, but then abort if failed (Option))
                synthesiseBridge[S](tyRepr.typeSymbol.name, bridgePrimaryArgs.map(_._2.asType), lift, from, labels, reason)
            case _ => report.errorAndAbort("can only make bridges for constructible classes or objects")
        }
    }

    private case class MetaImpl[T: Type](inst: Expr[ParsableMetadata[T]], ty: Type[T]) {
        def parser: Expr[Parsley[T]] = '{$inst.meta}
        def tyRepr = TypeRepr.of[T]
    }
    private val annotation = TypeRepr.of[isMeta].typeSymbol
    private def hasMeta(sym: Symbol) = sym.hasAnnotation(annotation)
    private def isMeta(sym: Symbol, contextualise: TypeRepr => TypeRepr): Option[MetaImpl[?]] = Option.when(hasMeta(sym)) {
        contextualise(sym.termRef.widen).asType match {
            case ty@'[t] => Expr.summon[ParsableMetadata[t]] match {
                case Some(inst) => MetaImpl[t](inst, ty)
                case None =>
                    val typeName = TypeRepr.of[t].show(using Printer.TypeReprShortCode)
                    report.errorAndAbort(s"attribute ${sym.name} can only use @isMeta with a `parsley.macros.ParsableMetadata[$typeName]` instance in scope", sym.pos.get)
            }
        }
    }

    private def defaultName(n: Int) = s"$$lessinit$$greater$$default$$$n"
    private def defaulted(cls: Symbol, n: Int): Option[Symbol] = cls.companionModule.declaredMethod(defaultName(n)).headOption

    @tailrec
    private def categoriseArgs(cls: Symbol, nonPrimaryArgs: List[List[Symbol]], n: Int, primary: Boolean, buf: mutable.ListBuffer[List[BridgeArg]], contextualise: TypeRepr => TypeRepr): List[List[BridgeArg]] = nonPrimaryArgs match {
        case Nil => buf.toList
        case args :: restArgs =>
            categoriseArgs(cls, restArgs, n + args.length, primary = false, buf += args.zipWithIndex.map {
                case (sym, i) => defaulted(cls, i + n) match {
                    // TODO: if it's primary, you could actually synthesise a default to the lifted constructor
                    case Some(sym) if !primary => BridgeArg.Default(i + n, sym)
                    case _ => isMeta(sym, contextualise) match {
                        case Some(impl)       => BridgeArg.Meta(impl)
                        case None if !primary => BridgeArg.Err(sym.name, sym.pos)
                        case None             => BridgeArg.Bridged(sym)
                    }
                }
            }, contextualise)
    }

    // FIXME: I don't like the duplication here...
    /** Constructs a lambda for a constructor applied to defaults and threading required metadata
      *
      * @param cls the class for the constructor
      * @param lamArgs the arguments for the lambda (without positions or defaulted)
      * @param clsTyArgs the type parameters provided to the constructor
      * @param otherArgs any remaining non-primary arguments
      * @param metaReprs the position
      * @return a lambda of the form `(lamArgs..) => cls[clsTyArgs](..)(otherArgs)`
      */
    private def constructor[R: Type](cls: Symbol, lamArgs: List[(String, TypeRepr)], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], metaReprs: List[TypeRepr]): Term = {
        val (paramNames, lamTys) = (metaReprs.zipWithIndex.map((ty, i) => s"meta$i" -> ty) ::: lamArgs).unzip
        // grrrrrrrr why has Scala given me Tree and not Term?!
        Lambda(Symbol.spliceOwner, MethodType(paramNames)(_ => lamTys, _ => TypeRepr.of[R]), { (lamSym, params) =>
            val paramTerms = params.map(_.asExpr.asTerm).toVector
            val (metaParams, paramTermsWithoutPos) = paramTerms.splitAt(metaReprs.length)
            appliedCon(cls, lamSym, paramTermsWithoutPos, clsTyArgs, otherArgs, metaParams)
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
    private def curriedConstructor[Fn: Type, R: Type](cls: Symbol, lamArgs: List[(String, TypeRepr)], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], metaReprs: List[TypeRepr]) = {
        val (paramNames, lamTys) = lamArgs.unzip
        val innerRepr = TypeRepr.of[R]
        def inner(owner: Symbol, metaParams: Vector[Term]) = {
            if (innerRepr.isSingleton) Ident(innerRepr.termSymbol.termRef)
            // this would be a class X(), with no parameters we need to fill
            else if (paramNames.isEmpty) {
                appliedCon(cls, owner, Vector.empty, clsTyArgs, otherArgs, metaParams)
            }
            else Lambda(owner, MethodType(paramNames)(_ => lamTys, _ => innerRepr), { (lamSym, params) =>
                // grrrrrrrr why has Scala given me Tree and not Term?!
                appliedCon(cls, lamSym, params.map(_.asExpr.asTerm).toVector, clsTyArgs, otherArgs, metaParams)
            })
        }
        // if this is not a singleton object, but is paramless, we don't want to cache the
        // created object, so feed this back to synthesiseLifted
        if (metaReprs.isEmpty) (inner(Symbol.spliceOwner, Vector.empty), !innerRepr.isSingleton && paramNames.isEmpty)
        else {
            val names = List.tabulate(metaReprs.length)(i => s"meta$i")
            val lambda = Lambda(Symbol.spliceOwner, MethodType(names)(_ => metaReprs, _ => TypeRepr.of[Fn]), { (outerLamSym, metaParam) =>
                // grrrrrrrr why has Scala given me Tree and not Term?!
                inner(outerLamSym, metaParam.map(_.asExpr.asTerm).toVector)
            })
            (lambda, false)
        }
    }

    private def appliedCon(cls: Symbol, owner: Symbol, lamParams: IndexedSeq[Term], clsTyArgs: List[TypeRepr], otherArgs: List[List[BridgeArg]], metaParams: IndexedSeq[Term]): Term = {
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
                var j = 0 // FIXME: get rid of this
                val terms = params.map {
                    case BridgeArg.Meta(_) =>
                        val p = metaParams(j)
                        j += 1
                        p
                    case BridgeArg.Default(n, sym) =>
                        Ident(cls.companionModule.termRef).select(sym).appliedToTypes(clsTyArgs).appliedToArgss(mySeeds)
                    case BridgeArg.Err(name, pos) =>
                        report.error(s"Argument $name for class ${cls.name} is neither a default or parsable metadata outside of the primary arguments, a bridge cannot be formed", pos.get)
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

    private def synthesiseLift[R: Type](argTys: List[TypeRepr], con: Term, args: List[Term], noCache: Boolean = false): Expr[Parsley[R]] = {
        val tys = argTys :+ TypeRepr.of[R]
        val arity = argTys.size
        if (arity == 0 && noCache) '{Parsley.fresh[R](${con.asExprOf[R]})}
        // no point in having lift0 if we have to special case fresh anyway...
        else if (arity == 0) '{Parsley.pure[R](${con.asExprOf[R]})}
        else TypeRepr.of[parsley.lift.type].typeSymbol.methodMember(s"lift$arity").headOption.map('{parsley.lift}.asTerm.select) match {
            case Some(lift) =>
                lift.appliedToTypes(tys)
                    .appliedToArgs(con :: args)
                    .asExprOf[Parsley[R]]
            case None => report.errorAndAbort(s"No `lift` available for arity $arity")
        }
    }

    private def synthesiseBridge[R: Type](n: String, argTys: List[Type[?]], lift: List[Term] => Expr[Parsley[R]], single: [T] => Type[T] => Expr[Parsley[T]], errLabels: Expr[List[String]], errReason: Expr[Option[String]]): Expr[ErrorBridge] = (argTys.size: @switch) match {
        case 0 => '{
            new bridges.SingletonBridge[R] with InternalMethodLeak {
                def singleton: Parsley[R] = ${single(Type.of[R])}
            }
        }
        // TODO: make generation of labels/reason conditional as to not bloat the objects
        case 1 => (argTys: @unchecked) match {
            case List('[t1]) => '{
                new bridges.ParserBridge1[t1, R] with InternalMethodLeak {
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
                new bridges.ParserBridge2[t1, t2, R] with InternalMethodLeak {
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
                new bridges.ParserBridge3[t1, t2, t3, R] with InternalMethodLeak {
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
                new bridges.ParserBridge4[t1, t2, t3, t4, R] with InternalMethodLeak {
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
                new bridges.ParserBridge5[t1, t2, t3, t4, t5, R] with InternalMethodLeak {
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
                new bridges.ParserBridge6[t1, t2, t3, t4, t5, t6, R] with InternalMethodLeak {
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
                new bridges.ParserBridge7[t1, t2, t3, t4, t5, t6, t7, R] with InternalMethodLeak {
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
                new bridges.ParserBridge8[t1, t2, t3, t4, t5, t6, t7, t8, R] with InternalMethodLeak {
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
                new bridges.ParserBridge9[t1, t2, t3, t4, t5, t6, t7, t8, t9, R] with InternalMethodLeak {
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
                new bridges.ParserBridge10[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, R] with InternalMethodLeak {
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
                new bridges.ParserBridge11[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, R] with InternalMethodLeak {
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
                new bridges.ParserBridge12[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, R] with InternalMethodLeak {
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
                new bridges.ParserBridge13[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, R] with InternalMethodLeak {
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
                new bridges.ParserBridge14[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, R] with InternalMethodLeak {
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
                new bridges.ParserBridge15[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, R] with InternalMethodLeak {
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
                new bridges.ParserBridge16[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, R] with InternalMethodLeak {
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
                new bridges.ParserBridge17[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, R] with InternalMethodLeak {
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
                new bridges.ParserBridge18[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, R] with InternalMethodLeak {
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
                new bridges.ParserBridge19[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, R] with InternalMethodLeak {
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
                new bridges.ParserBridge20[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, R] with InternalMethodLeak {
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
                new bridges.ParserBridge21[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, R] with InternalMethodLeak {
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
                new bridges.ParserBridge22[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, R] with InternalMethodLeak {
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
                val (tyParamss, valParamss) = primCon.paramSymss.partition(_.exists(_.isType))
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
