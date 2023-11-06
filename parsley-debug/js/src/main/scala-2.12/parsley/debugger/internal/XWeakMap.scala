/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scalajs.js
import scalajs.js.WeakRef

private [parsley] class XWeakMap[K, V] extends mutable.Map[K, V] {
    // Constants and helpers.
    private val minBuckets: Int = 8
    private val maxBucketConstant: Double = 4.0
    private val minBucketConstant: Double = 0.25

    private def currentBucketValue(): Double =
        backing.map(_.length).sum.toDouble / backing.length.toDouble

    private def grow(n: Int): Int = Math.max(minBuckets, n + (n >> 1))
    private def shrink(n: Int): Int = Math.max(minBuckets, (n >> 1) + (n >> 2))

    // WeakRef to Option helper.
    private implicit class WeakRefOps[+S](wr: js.WeakRef[S]) {
        def derefAsOption: Option[S] = {
            val x = wr.deref()
            if (x.isDefined) Some(x.get) else None
        }
    }

    private var backing: Array[ListBuffer[(WeakRef[K], V)]] = Array.fill(minBuckets)(new ListBuffer())

    // Run through and expunge all stale weak references from the map.
    private def removeStale(): Unit =
            backing.map(_.filter(_._1.derefAsOption.isDefined))

    private def resize(cmp: Double => Boolean, rf: Int => Int): Unit = {
        if (cmp(currentBucketValue())) {
            // Only remove stale when growing or shrinking.
            removeStale()

            val entries: ListBuffer[(WeakRef[K], V)] = ListBuffer()
            backing.foreach(entries ++= _)

            backing = Array.fill(rf(backing.length))(new ListBuffer())
            entries.foreach { case (wk, v) =>
                wk.derefAsOption match {
                    case None    => ()
                    case Some(k) => backing(k.hashCode() % backing.length).prepend((wk, v))
                }
            }
        }
    }

    override def -=(key: K): XWeakMap.this.type = {
        backing(key.hashCode() % backing.length).filterNot(_._1.derefAsOption.exists(_ == key))
        resize(_ < minBucketConstant, shrink)
        this
    }

    override def +=(kv: (K, V)): XWeakMap.this.type = {
        kv match {
            case (k, v) => backing(k.hashCode() % backing.length).prepend((new WeakRef(k), v))
        }
        resize(_ > maxBucketConstant, grow)

        this
    }

    override def get(key: K): Option[V] = {
        val kh = key.hashCode()
        val lb = backing(kh % backing.length)

        lb.find(_._1.derefAsOption.exists(_ == key)).map(_._2)
    }

    // We don't actually need this, and it's very hard to work this out properly for a map with weak keys.
    override def iterator: Iterator[(K, V)] = ???
}
