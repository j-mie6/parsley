/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

import org.typelevel.scalaccompat.annotation.unused

// Abstracts away the logic of XWeakMap so that only minimal interfaces are needed between Scala
// versions. This reduces code duplication, and makes further changes much easier to implement.
private [internal] final class XAbstractWeakMap[K, V] extends Iterable[(K, V)] {
    // If we ever need to change this type, it can be done here instead of in each individual
    // implementation of XWeakMap.
    private val backing: mutable.WeakHashMap[K, V] = new mutable.WeakHashMap()

    def drop(k: K): Unit = {
        backing.remove(k): @unused
        () // Silences unit return warning.
    }

    def push(kv: (K, V)): Unit = {
        backing.put(kv._1, kv._2): @unused
        () // Silences unit return warning.
    }

    def lookup(key: K): Option[V] =
        backing.get(key)

    override def iterator: Iterator[(K, V)] =
        backing.iterator
}
