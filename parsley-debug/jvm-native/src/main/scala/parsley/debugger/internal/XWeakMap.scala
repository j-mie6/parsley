/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

import org.typelevel.scalaccompat.annotation.unused
import parsley.debugger.util.XMutMap

// Abstracts away the logic of XWeakMap so that only minimal interfaces are needed between Scala
// versions. This reduces code duplication, and makes further changes much easier to implement.
private [debugger] final class XWeakMap[K, V] extends XMutMap[K, V] with Iterable[(K, V)] { // scalastyle:ignore magic.number
    // If we ever need to change this type, it can be done here instead of in each individual
    // implementation of XWeakMap.
    private val backing: mutable.WeakHashMap[K, V] = new mutable.WeakHashMap()

    // There's not a lot of key dropping happening in this codebase, but this is here to conform with the
    // mutable.Map[K, V] trait more closely.
    // $COVERAGE-OFF$
    override def subtractOne(k: K): XWeakMap.this.type = {
        val _ = backing.remove(k): @unused
        this
    }
    // $COVERAGE-ON$

    override def addOne(kv: (K, V)): XWeakMap.this.type = {
        val _ = backing.put(kv._1, kv._2): @unused
        this
    }

    override def get(key: K): Option[V] =
        backing.get(key)

    // $COVERAGE-OFF$
    override def iterator: Iterator[(K, V)] =
        backing.iterator
    // $COVERAGE-ON$
}
