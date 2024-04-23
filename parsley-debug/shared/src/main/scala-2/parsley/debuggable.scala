/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

/** This annotation can be applied to an object or class to record their
  * names for the debugging/diagnostic combinators.
  *
  * @note BE WARNED: this annotation crashes the compiler for objects/classes nested within another type -- this is ok for Scala 3
  * @note It requires that the
  * object/class is type-checkable, which due to Scala 2 macro limitations
  * involes stripping away the enclosing type itself. This might lead to
  * weird edge-cases: if parsley reports that type-checking failed, you
  * should report this to the maintainers.
  *
  * @since 5.0.0
  */
@compileTimeOnly("macros need to be enabled to use this functionality: -Ymacro-annotations in 2.13, or use \"Macro Paradise\" for 2.12")
class debuggable extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro debuggable.impl
}

private object debuggable {
    def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
        import c.universe._
        // to accurately model the Scala 3 equivalent, we are treated like a black-box
        // macro: only the first annottee is relevant, and this must be a class or an object
        // anything else is returned as is.
        val inputs = annottees.toList
        val outputs = inputs match {
            case ClassDef(mods, clsName, tyParams, Template(parents, self, body)) :: rest =>
                collect(c)(clsName.toString, body, body => ClassDef(mods, clsName, tyParams, Template(parents, self, body))) :: rest
            case ModuleDef(mods, objName, Template(parents, self, body)) :: rest =>
                collect(c)(objName.toString, body, body => ModuleDef(mods, objName, Template(parents, self, body))) :: rest
            case _ =>
                c.error(c.enclosingPosition, "only classes/objects containing parsers can be annotated for debugging")
                inputs
        }
        q"{..$outputs}"
    }

    private def collect(c: blackbox.Context)(treeName: String, defs: List[c.Tree], recon: List[c.Tree] => c.Tree): c.Tree = {
        import c.universe._
        val parsleyTy = c.typeOf[Parsley[_]].typeSymbol
        // can't typecheck constructors in a stand-alone block
        val noConDefs = defs.flatMap {
            case DefDef(_, name, _, _, _, _) if name == TermName("<init>") => None
            // in addition, on 2.12, we need to remove `paramaccessor` modifiers on constructor arguments
            // but due to API incompatibility, we have to use NoFlags...
            case ValDef(Modifiers(_, privateWithin, annotations), name, tpt, x) =>
                Some(ValDef(Modifiers(NoFlags, privateWithin, annotations), name, tpt, x))
            case dfn => Some(dfn)
        }
        val typeMap = c.typecheck(q"..${noConDefs}", c.TERMmode, silent = false) match {
            // in this case, we want to use the original tree (it's still untyped, as required)
            // but we can process typedDefs into a map from identifiers to inferred types
            case Block(typedDefs, _) =>
                typedDefs.collect {
                    case ValDef(_, name, tpt, _) => name -> ((Nil, tpt.tpe))
                    case DefDef(_, name, _, args, ret, _) => name -> ((args.flatten.map(_.tpt.tpe), ret.tpe))
                }.toMap
            // in this case, we are stuck
            case _ =>
                c.error(c.enclosingPosition, s"`$treeName` cannot be typechecked, and so no names can be registered: please report this to the maintainers")
                Map.empty[TermName, (List[Type], Type)]
        }
        // filter the definitions based on their type from the type map:
        val parsers = defs.flatMap {
            case dfn: ValOrDefDef => typeMap.get(dfn.name) match {
                case Some((Nil, ty)) if ty.typeSymbol == parsleyTy => Some(dfn.name)
                case _ => None
            }
            case _ => None
        }
        // the idea is we inject a call to Collector.registerNames with a constructed
        // map from these identifiers to their compile-time names
        val listOfParsers = q"List(..${parsers.map(tr => q"${Ident(tr)} -> ${tr.toString}")})"
        val registration = q"parsley.debugger.util.Collector.registerNames($listOfParsers.toMap)"
        // add the registration as the last statement in the object
        // TODO: in future, we want to modify all `def`s with a top level `opaque` combinator
        // that will require a bit more modification of the overall `body`, unfortunately
        recon(defs :+ registration)
    }
}
