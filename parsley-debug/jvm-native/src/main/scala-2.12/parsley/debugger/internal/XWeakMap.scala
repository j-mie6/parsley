/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

// For the JVM and Native, its WeakHashMap does the job.
private [parsley] final class XWeakMap[K, V] extends mutable.Map[K, V] {
    private val backing: XAbstractWeakMap[K, V] = new XAbstractWeakMap()

    override def -=(k: K): XWeakMap[K, V] = {
        backing.drop(k)
        this
    }

    override def +=(kv: (K, V)): XWeakMap[K, V] = {
        backing.push(kv._1, kv._2)
        this
    }

    override def get(key: K): Option[V] =
        backing.lookup(key)

    override def iterator: Iterator[(K, V)] =
        backing.iterator
}
