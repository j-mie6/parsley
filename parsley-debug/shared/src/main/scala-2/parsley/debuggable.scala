/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

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
                collect(c)(body, body => ClassDef(mods, clsName, tyParams, Template(parents, self, body))) :: rest
            case ModuleDef(mods, objName, Template(parents, self, body)) :: rest =>
                collect(c)(body, body => ModuleDef(mods, objName, Template(parents, self, body))) :: rest
            case _ =>
                c.error(c.enclosingPosition, "only classes/objects containing parsers can be annotated for debugging")
                inputs
        }
        q"{..$outputs}"
    }

    private def collect(c: blackbox.Context)(defs: List[c.Tree], recon: List[c.Tree] => c.Tree): c.Tree = {
        import c.universe._
        val parsleyTy = c.typeOf[Parsley[_]].typeSymbol
        // can't typecheck constructors in a stand-alone block
        // FIXME: in addition, on 2.12, we need to remove `paramaccessor` modifiers on constructor arguments
        val noConDefs = defs.filter {
            case DefDef(_, name, _, _, _, _) => name != TermName("<init>")
            case _ => true
        }
        val typeMap = c.typecheck(q"..${noConDefs}", c.TERMmode, silent = true) match {
            // in this case, we want to use the original tree (it's still untyped, as required)
            // but we can process typedDefs into a map from identifiers to inferred types
            case Block(typedDefs, _) =>
                typedDefs.collect {
                    case ValDef(_, name, tpt, _) => name -> ((Nil, tpt.tpe))
                    case DefDef(_, name, _, args, ret, _) => name -> ((args.flatten.map(_.tpt.tpe), ret.tpe))
                }.toMap
            // in this case, we can extract those with annotated type signatures
            case _ => Map.empty[TermName, (List[Type], Type)]
        }
        // filter the definitions based on their type from the type map:
        def filterParsley(t: Tree) = t match {
            case dfn: ValOrDefDef => typeMap.get(dfn.name) match {
                case Some((Nil, ty)) if ty.typeSymbol == parsleyTy => Some(dfn.name)
                case _ => None
            }
            case _ => None
        }
        val parsers = defs.collect {
            case t if filterParsley(t).isDefined => filterParsley(t).get // I hate you 2.12
        }
        // the idea is we inject a call to Collector.registerNames with a constructed
        // map from these identifiers to their compile-time names
        val listOfParsers = q"List(..${parsers.map(tr => q"${Ident(tr)} -> ${tr.toString}")})"
        val registration = q"parsley.debugger.util.Collector.registerNames($listOfParsers.toMap)"

        /*println(registration)
        println(typeMap)
        assert(parsers.nonEmpty)*/
        // add the registration as the last statement in the object
        // TODO: in future, we want to modify all `def`s with a top level `opaque` combinator
        // that will require a bit more modification of the overall `body`, unfortunately
        recon(defs :+ registration)
    }
}
