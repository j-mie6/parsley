/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debug.internal

import scala.collection.mutable

/**
  * The ParserInfo class represents metadata about a parser module obtained
  * using the debuggable macro.
  *
  * @param path The absolute path to a parser.
  * @param positions A list of (start, end) positions of parsers within a 
  *   module.
  */
final private [parsley] case class ParserInfo(val path: String, val positions: List[(Int, Int)])

/**
  * Object with methods to add detected parser information to a mutable set,
  * allowing debugged source files and the locations of their parsers to be
  * known at runtime.
  * 
  * This is mainly to enable a debugger to display source files inside of the
  * application so that debugging faulty code can easily be done in the same 
  * application.
  */
private [parsley] object ParserInfoCollector {
    // The set of collected source file paths
    private lazy val collected: mutable.Set[ParserInfo] = mutable.Set.empty
    
    /**
      * Add an iterable set of parser information to the collection.
      *
      * @param info The parser information to add.
      */
    private [parsley] def addInfos(info: Seq[ParserInfo]): Unit = collected ++= info

    /**
      * Add a single parser information class to the collection.
      *
      * @param info The parser info to add.
      */
    private [parsley] def addInfo(info: ParserInfo): Unit = collected += info

    // Obtain the collected information.
    private [parsley] lazy val info: Set[ParserInfo] = collected.toSet
}
