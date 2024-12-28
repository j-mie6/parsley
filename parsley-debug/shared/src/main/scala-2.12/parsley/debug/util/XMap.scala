/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

// This is used to give Scala 2.12 access to "removed" and "updated".
// Ideally, this would be done via scala-collection-compat, but that requires integrating ScalaFix.
private [parsley] trait XMap[K, +V] extends Map[K, V] {
    def removed(key: K): Map[K, V]

    def updated[V1 >: V](key: K, value: V1): Map[K, V1]

    override def -(key: K): Map[K, V] = removed(key)

    override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
}
