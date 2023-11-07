/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.WeakRef

import XAbstractWeakMap.WeakRefOps

private [internal] final class XAbstractWeakMap[K, V](rs: Array[ListBuffer[(WeakRef[K], V)]] => Unit) {
    // Constants and helpers.
    private val minBuckets: Int = 8
    private val maxBucketConstant: Double = 4.0
    private val minBucketConstant: Double = 0.25

    private def currentBucketValue(): Double =
        backing.map(_.length).sum.toDouble / backing.length.toDouble

    private def grow(n: Int): Int = Math.max(minBuckets, n + (n >> 1))
    private def shrink(n: Int): Int = Math.max(minBuckets, (n >> 1) + (n >> 2))

    private var backing: Array[ListBuffer[(WeakRef[K], V)]] = Array.fill(minBuckets)(new ListBuffer())

    // Run through and expunge all stale weak references from the map.
    private def removeStale(): Unit =
        rs(backing)

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
                    case Some(k) => backing(k.hashCode() % backing.length).append((wk, v))
                }
            }
        }
    }

    def drop(key: K): XAbstractWeakMap[K, V] = {
        backing(key.hashCode() % backing.length).filterNot(_._1.derefAsOption.exists(_ == key))
        resize(_ < minBucketConstant, shrink)
        this
    }

    def push(kv: (K, V)): XAbstractWeakMap[K, V] = {
        kv match {
            case (k, v) => backing(k.hashCode() % backing.length).append((new WeakRef(k), v))
        }
        resize(_ > maxBucketConstant, grow)

        this
    }

    def lookup(key: K): Option[V] = {
        val kh = key.hashCode()
        val lb = backing(kh % backing.length)

        lb.find(_._1.derefAsOption.exists(_ == key)).map(_._2)
    }
}

// WeakRef to Option helper.
private [internal] object XAbstractWeakMap {
    implicit class WeakRefOps[+S](wr: js.WeakRef[S]) {
        def derefAsOption: Option[S] = {
            val x = wr.deref()
            if (x.isDefined) Some(x.get) else None
        }
    }
}
