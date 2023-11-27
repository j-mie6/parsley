/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

import org.typelevel.scalaccompat.annotation.unused

// Simple facade type for ECMA 6's WeakMap.
// Source: https://github.com/typelevel/cats-effect/blob/31ad01e798133f9bc62e043aac2e7709d4ba017d/core/js/src/main/scala/cats/effect/unsafe/WeakMap.scala
// Author: Arman Bilge (@armanbilge)
@js.native
@JSGlobal
private [internal] sealed class WeakMap[K <: Object, V] extends js.Object {
    def delete(key: K): Boolean = js.native
    def get(key: K): js.UndefOr[V] = js.native
    def has(key: K): Boolean = js.native
    def set(key: K, value: V): this.type = js.native
}

private [internal] final class XWeakMapImpl[K <: Object, V] extends WeakMap[K, V] { //
    def lookup(key: K): Option[V] = get(key).toOption
    def push(kv: (K, V)): Unit = {
        val _ = set(kv._1, kv._2): @unused
    }
    def drop(key: K): Unit = {
        val _ = delete(key): @unused
    }
}