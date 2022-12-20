/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.collection.mutable

import scala.annotation.tailrec
import scala.collection.{mutable, BufferedIterator}

import Radix.{Entry, IteratorHelpers, StringHelpers}

@deprecated("Radix is currently not being unit tested, if it is used again remove test coverage lines", "4.0.0")
// $COVERAGE-OFF$
private [internal] class Radix[A] {
    private [this] var x = Option.empty[A]
    private val m = mutable.Map.empty[Char, Entry[A]]

    def get(key: String): Option[A] = {
        if (key.isEmpty) x
        else for {
            e <- m.get(key.head)
            if key.startsWith(e.prefix)
            v <- e.radix.get(key.drop(e.prefix.length))
        } yield v
    }

    def getMax(key: BufferedIterator[Char]): Option[A] = {
        // If there are no forwards paths, and no value at this node
        // we don't need to check for input: it will always return None
        if (m.isEmpty && x.isEmpty) None
        else {
            (for {
                k1 <- key.headOption
                e <- m.get(k1)
                if key.checkPrefixWhileConsuming(e.prefix)
                v <- e.radix.getMax(key)
            } yield v).orElse(x)
        }
    }

    def isEmpty: Boolean = x.isEmpty && m.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def suffixes(c: Char): Radix[A] = m.get(c) match {
        case Some(e) =>
            // We have to form a new root
            if (e.prefix.length > 1) Radix(new Entry(e.prefix.tail, e.radix))
            else e.radix
        case None => Radix.empty
    }

    def contains(key: String): Boolean = get(key).nonEmpty
    def apply(key: String): A = get(key).getOrElse(throw new NoSuchElementException(key)) // scalastyle:ignore throw

    def update(key: String, value: A): Unit =
        if (key.isEmpty) x = Some(value)
        else {
            val e = m.getOrElseUpdate(key.head, new Entry(key, Radix.empty[A]))
            if (key.startsWith(e.prefix)) e.radix(key.drop(e.prefix.length)) = value
            else {
                // Need to split the tree: find their common prefix first
                val common = key.commonPrefix(e.prefix)
                e.dropInPlace(common.length)
                val radix = Radix(e)
                // Continue inserting the key
                radix(key.drop(common.length)) = value
                // Insert our new entry
                m(common.head) = new Entry(common, radix)
            }
        }
}

private [internal] object Radix {
    type RadixSet = Radix[Unit]

    def empty[A]: Radix[A] = new Radix

    private def apply[A](e: Entry[A]): Radix[A] = {
        val radix = empty[A]
        radix.m(e.prefix.head) = e
        radix
    }

    def makeSet(ks: Iterable[String]): RadixSet = apply(ks.view.zip(units))
    def apply[A](kvs: (String, A)*): Radix[A] = apply(kvs)
    def apply[A](kvs: Iterable[(String, A)]): Radix[A] = {
        val r = Radix.empty[A]
        for ((k, v) <- kvs) r(k) = v
        r
    }

    private class Entry[A](var prefix: String, val radix: Radix[A]) {
        def dropInPlace(n: Int): Unit = prefix = prefix.drop(n)
    }

    private [internal] implicit class IteratorHelpers(val it: BufferedIterator[Char]) extends AnyVal {
        @tailrec private final def go(it: BufferedIterator[Char], itPre: BufferedIterator[Char]): Boolean = {
            if (!itPre.hasNext) true
            else if (!it.hasNext || it.head != itPre.head) false
            else {
                it.next()
                itPre.next()
                go(it, itPre)
            }
        }

        def checkPrefixWhileConsuming(prefix: Iterable[Char]): Boolean = go(it, prefix.iterator.buffered)
    }

    private [internal] implicit class StringHelpers(val s1: String) extends AnyVal {
        def commonPrefix(s2: String): String = s1.view.zip(s2).takeWhile(Function.tupled(_ == _)).map(_._1).mkString
    }

    private val units = new Iterable[Unit] {
        def iterator: Iterator[Unit] = unitIterator
    }

    private val unitIterator = new Iterator[Unit] {
        def hasNext = true
        def next() = ()
    }
}
// $COVERAGE-ON$
