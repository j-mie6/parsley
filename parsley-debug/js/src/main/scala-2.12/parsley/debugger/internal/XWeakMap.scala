/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

private [parsley] final class XWeakMap[K <: Object, V](startSize: Int = 32) extends mutable.Map[K, V] { // scalastyle:ignore magic.number
    private [internal] val backing: XAbstractWeakMap[K, V] = new XAbstractWeakMap(backing =>
        backing.map(_.filter(_._1.deref().isDefined)),
        startSize
    )

    override def -=(key: K): XWeakMap.this.type = {
        backing.drop(key)
        this
    }

    override def +=(kv: (K, V)): XWeakMap.this.type = {
        backing.push(kv)
        this
    }

    override def get(key: K): Option[V] =
        backing.lookup(key)

    // We don't actually need this, and it's very hard to work this out properly for a map with weak keys.
    override def iterator: Iterator[(K, V)] = ??? // scalastyle:ignore not.implemented.error.usage
}
