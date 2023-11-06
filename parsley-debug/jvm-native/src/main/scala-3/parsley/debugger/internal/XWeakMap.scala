/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

// For the JVM and Native, its WeakHashMap does the job.
private [parsley] class XWeakMap[K, V] extends mutable.Map[K, V] {
    private val mmap: mutable.WeakHashMap[K, V] = new mutable.WeakHashMap()

    override def subtractOne(k: K): XWeakMap.this.type = {
        mmap.remove(k)
        this
    }

    override def addOne(kv: (K, V)): XWeakMap.this.type = {
        mmap.put(kv._1, kv._2)
        this
    }

    override def get(key: K): Option[V] =
        mmap.get(key)

    override def iterator: Iterator[(K, V)] =
        mmap.iterator
}
