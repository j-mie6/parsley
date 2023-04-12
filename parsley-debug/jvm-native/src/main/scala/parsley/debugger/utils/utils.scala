/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import parsley.Parsley
import parsley.debugger.internal.Rename
import parsley.internal.deepembedding.frontend.LazyParsley

/** Miscellaneous utilities for enhancing the debugger.
  * Using these utilities is optional.
  */
package object utils {
  /** Attempt to collect all the fields in a class or object that contain a
    * parser of type [[Parsley]].
    *
    * This information is used later in the debug tree-building process to rename certain parsers
    * so that they do not end up being named things like "packageanon".
    *
    * A caveat is that this only works with fields (lazy or otherwise). It will not work with
    * parsers defined in `def`-methods.
    *
    * You only need to run this once per parser-holding object.
    *
    * Currently, this method only fully functions on the JVM.
    *
    * @param obj Class or object to analyse.
    * @tparam A  Type of class or object to analyse.
    */
  def collectNames(obj: Any): Unit = {
    // Get the runtime class.
    val clazz = obj.getClass

    // Get all the getters for parser instances in a class.
    // We're using Parsley.unit as our dummy instance (type erasure at runtime saves us here).
    val methods = clazz.getDeclaredMethods.filter(m =>
      m.getReturnType.isAssignableFrom(classOf[LazyParsley[_]])
      && m.getParameterCount == 0
      && !m.getName.startsWith("copy"))

    val asMapM = methods.flatMap(mth => {
      // Try to make this method accessible if it is private.
      if (!mth.trySetAccessible()) List()
      else {
        // Extract our parser and add the method's name to our name map
        val contents = tryExtract(mth.invoke(obj))
        val name = if (mth.getName.contains("anonfun")) {
          mth.getName.split("\\$")(2)
        } else if (mth.getName.contains("lzycompute")) {
          mth.getName.split("\\$")(0)
        } else {
          mth.getName
        }

//        println(s"found ${contents.hashCode()} at $name")
        List((contents, name))
      }
    }).toMap

    // Get all fields with a parser.
    val fields = clazz.getDeclaredFields.filter(_.getClass.isAssignableFrom(classOf[LazyParsley[_]]))

    // Make a bunch of search functions from those fields.
    val asMapF = fields.flatMap(fld => {
      // Try to make this field accessible if it is private.
      if (!fld.trySetAccessible()) List()
      else {
        // Extract the internal parser and add its field name into our name map.
        val contents = tryExtract(fld.get(obj))
//        println(s"found ${contents.hashCode()} at ${fld.getName}")
        List((contents, fld.getName))
      }
    }).toMap

    // Add our collected names into the global map.
    Rename.addNames(asMapM)
    Rename.addNames(asMapF)
  }

  private def tryExtract(p: Any): LazyParsley[_] = {
    try {
      p.asInstanceOf[LazyParsley[_]]
    } catch {
      case _: ClassCastException => p.asInstanceOf[Parsley[_]].internal
    }
  }
}
