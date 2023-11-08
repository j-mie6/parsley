/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.scalajs.js.WeakRef

import XAbstractWeakMap.Backing
import org.typelevel.scalaccompat.annotation.unused

// XXX: This map has a heavy dependency on JS's WeakRef. Not all types will be accepted as keys as
//      they must qualify as Objects to be inserted as keys into a weak reference.
//      Simple types like strings will raise a runtime exception.
private [internal] final class XAbstractWeakMap[K <: Object, V](startSize: Int = 32) { // scalastyle:ignore magic.number
    // Constants and helpers.
    private val minBuckets: Int = 8
    private val maxBucketConstant: Double = 4.0
    private val minBucketConstant: Double = 0.25

    // XXX: It would be better to use a size variable private to the map, but the issue is that
    //      removeStale() will mess with that, so the passed in stale remover function would end up
    //      needing access to that variable in a bit of a messy way.
    private def currentBucketValue(): Double =
        trueSize().toDouble / backing.length.toDouble

    private [internal] def trueSize(): Int =
        backing.foldLeft(0)(_ + _.length)

    private def grow(n: Int): Int = Math.max(minBuckets, n + (n >> 1))
    private def shrink(n: Int): Int = Math.max(minBuckets, (n >> 1) + (n >> 2))

    private var backing: Backing[K, V] = Array.fill(
        Math.max(minBuckets, startSize / maxBucketConstant.toInt)
    )(new ListBuffer())

    // Resize only if cmp returns true after being fed currentBucketValue().
    // This is also the only time stale entries are removed, otherwise we'd accrue an O(n) penalty every single
    // time we want to query the map.
    private def resize(cmp: Double => Boolean, rf: Int => Int): Unit = {
        if (cmp(currentBucketValue())) {
            val old: Backing[K, V] = backing
            backing = Array.fill(rf(backing.length))(new ListBuffer())

            old.foreach { entries =>
                @tailrec def go(ix: Int): Unit =
                    if (ix < entries.length) {
                        val (wk, v) = entries(ix)
                        val k       = wk.deref()

                        if (k.isDefined) {
                            backing(k.get.hashCode() % backing.length).append((wk, v))
                            go(ix + 1)
                        } else {
                            // Current index is stale, remove it and try again on the same index.
                            entries.remove(ix): @unused
                            go(ix)
                        }
                    }

                go(0)
            }
        }
    }

    def drop(key: K): Unit = {
        // Filter out the key using a tight loop.
        val lb  = backing(key.hashCode() % backing.length)

        @tailrec def go(ix: Int): Unit =
            if (ix < lb.length) {
                val current = lb(ix)._1.deref()

                if (current.exists(_ == key)) {
                    lb.remove(ix): @unused
                    resize(_ < minBucketConstant, shrink)
                } else if (current.isEmpty) {
                    // Current index is stale, remove it and try again on the same index.
                    lb.remove(ix): @unused
                    go(ix)
                } else {
                    go(ix + 1)
                }
            }

        go(0)
    }

    def push(kv: (K, V)): Unit =
        // Actually remember to replace V if the key K already exists.
        kv match {
            case (k, v) =>
                val lb  = backing(k.hashCode() % backing.length)

                @tailrec def go(ix: Int): Boolean =
                    if (ix < lb.length) {
                        val current = lb(ix)._1.deref()

                        if (current.exists(_ == k)) {
                            lb(ix) = (lb(ix)._1, v)
                            true
                        } else if (current.isEmpty) {
                            // See above in drop().
                            lb.remove(ix): @unused
                            go(ix)
                        } else {
                            go(ix + 1)
                        }
                    } else false

                if (!go(0)) {
                    lb.append((new WeakRef(k), v))
                    resize(_ > maxBucketConstant, grow)
                }
        }

    def lookup(key: K): Option[V] = {
        val kh = key.hashCode()
        val lb = backing(kh % backing.length)

        @tailrec def go(ix: Int): Option[V] =
            if (ix < lb.length) {
                val current = lb(ix)._1.deref()

                if (current.exists(_ == key)) {
                    Some(lb(ix)._2)
                } else if (current.isEmpty) {
                    // See above in drop().
                    lb.remove(ix): @unused
                    go(ix)
                } else {
                    go(ix + 1)
                }
            } else None

        go(0)
    }
}

private object XAbstractWeakMap {
    type Backing[K, V] = Array[ListBuffer[(WeakRef[K], V)]]
}
