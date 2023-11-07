/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

// Abstracts away the logic of XWeakMap so that only minimal interfaces are needed between Scala
// versions. This reduces code duplication, and makes further changes much easier to implement.
private [internal] final class XAbstractWeakMap[K, V] extends Iterable[(K, V)] {
    // If we ever need to change this type, it can be done here instead of in each individual
    // implementation of XWeakMap.
    private val backing: mutable.WeakHashMap[K, V] = new mutable.WeakHashMap()

    def drop(k: K): XAbstractWeakMap[K, V] = {
        backing.remove(k)
        this
    }

    def push(kv: (K, V)): XAbstractWeakMap[K, V] = {
        backing.put(kv._1, kv._2)
        this
    }

    def lookup(key: K): Option[V] =
        backing.get(key)

    override def iterator: Iterator[(K, V)] =
        backing.iterator
}
