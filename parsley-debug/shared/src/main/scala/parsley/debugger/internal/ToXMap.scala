/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

trait ToXMap[M[_, _]] {
    def toXMap[K, V](map: M[K, V]): XMap[K, V]
}

object ToXMap {
    implicit val mtx: ToXMap[Map] = new ToXMap[Map] {
        override def toXMap[K, V](m: Map[K, V]): XMap[K, V] = new XMap[K, V] {
            override def removed(key: K): Map[K, V] = m - key

            override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = {
                val kv = (key, value)
                m + kv
            }

            override def get(key: K): Option[V] = m.get(key)

            override def iterator: Iterator[(K, V)] = m.iterator

            override def size: Int = m.size
        }
    }
}
