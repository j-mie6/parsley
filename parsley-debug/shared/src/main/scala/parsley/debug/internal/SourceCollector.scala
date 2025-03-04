/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debug.internal

import scala.collection.mutable
import scala.collection.IterableOnce

private [parsley] case class SourceCollector()

/**
  * Object with methods to add source file paths detected to a mutable set,
  * allowing debugged source files to be known at runtime.
  * 
  * This is mainly to enable a debugger to display source files inside of the
  * application so that debugging faulty code can easily be done in the same 
  * application.
  */
private [parsley] object SourceCollector {
    // The set of collected source file paths
    private lazy val collected: mutable.Set[String] = mutable.Set.empty
    
    /**
      * Add an iterable set of source paths to the collection.
      *
      * @param sources The source paths to add.
      */
    private [parsley] def addSources(sources: IterableOnce[String]): Unit = collected ++= sources

    /**
      * Add a single source path string to the collection.
      *
      * @param source The source paths to add.
      */
    private [parsley] def addSource(source: String): Unit = collected += source

    // Obtain the collected sources.
    private [parsley] lazy val sources: Set[String] = collected.toSet
}
