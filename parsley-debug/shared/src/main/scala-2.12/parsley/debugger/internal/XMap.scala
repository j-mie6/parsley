/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

// Compatibility bridge for Scala 2.12
trait XMap[K, +V] extends Map[K, V] {
  def removed(key: K): Map[K, V]

  def updated[V1 >: V](key: K, value: V1): Map[K, V1]

  override def -(key: K): Map[K, V] =
    removed(key)

  override def updated[V1 >: DebugTree](kv: (K, V1)): Map[String, V1] =
    updated(kv._1, kv._2)
}
