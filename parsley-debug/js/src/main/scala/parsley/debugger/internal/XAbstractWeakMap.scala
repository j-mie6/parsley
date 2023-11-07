/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.WeakRef

import XAbstractWeakMap._ // scalastyle:ignore underscore.import
import org.typelevel.scalaccompat.annotation.unused

private [internal] final class XAbstractWeakMap[K, V](rs: Backing[K, V] => Unit) {
    // Constants and helpers.
    private val minBuckets: Int = 8
    private val maxBucketConstant: Double = 4.0
    private val minBucketConstant: Double = 0.25

    // XXX: It would be better to use a size variable private to the map, but the issue is that
    //      removeStale() will mess with that, so the passed in stale remover function would end up
    //      needing access to that variable in a bit of a messy way.
    private def currentBucketValue(): Double =
        backing.foldLeft(0)(_ + _.length).toDouble / backing.length.toDouble

    private def grow(n: Int): Int = Math.max(minBuckets, n + (n >> 1))
    private def shrink(n: Int): Int = Math.max(minBuckets, (n >> 1) + (n >> 2))

    private var backing: Backing[K, V] = Array.fill(minBuckets)(new ListBuffer())

    // Run through and expunge all stale weak references from the map.
    private def removeStale(): Unit =
        rs(backing)

    // Resize only if cmp returns true after being fed currentBucketValue().
    // This is also the only time stale entries are removed, otherwise we'd accrue an O(n) penalty every single
    // time we want to query the map.
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

    def drop(key: K): Unit = {
        // Filter out the key using a tight loop.
        val lb  = backing(key.hashCode() % backing.length)
        val len = lb.length

        @tailrec def go(ix: Int): Unit =
            if (ix <= len) {
                if (lb(ix)._1.derefAsOption.contains(key)) {
                    lb.remove(ix): @unused
                    resize(_ < minBucketConstant, shrink)
                } else go(ix + 1)
            }

        go(0)
    }

    def push(kv: (K, V)): Unit =
        // Actually remember to replace V if the key K already exists.
        kv match {
            case (k, v) =>
                val lb  = backing(k.hashCode() % backing.length)
                val len = lb.length

                @tailrec def go(ix: Int): Boolean =
                    if (ix <= len) {
                        if (lb(ix)._1.derefAsOption.contains(k)) {
                            lb(ix) = (new WeakRef(k), v)
                            true
                        } else go(ix + 1)
                    } else false

                if (!go(0)) {
                    lb.append((new WeakRef(k), v))
                    resize(_ > maxBucketConstant, grow)
                }
        }

    def lookup(key: K): Option[V] = {
        val kh = key.hashCode()
        val lb = backing(kh % backing.length)

        lb.find(_._1.derefAsOption.contains(key)).map(_._2)
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

    type Backing[K, V] = Array[ListBuffer[(WeakRef[K], V)]]
}
