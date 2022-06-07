/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal

import LinkedList.Node
import scala.collection.AbstractIterator

private [internal] class LinkedList[A] extends Iterable[A] {
    var start: Node[A] = null
    var end: Node[A] = null

    // This method assumes that this list is non-empty
    private [LinkedList] def unsafeAddOne(x: A): this.type = {
        val next = new Node(x, null)
        if (this.end != null) this.end.next = next
        this.end = next
        this
    }
    def addOne(x: A): this.type = {
        if (this.start == null) {
            this.start = new Node(x, null)
            this.end = this.start
            this
        }
        else unsafeAddOne(x)
    }
    def +=(x: A): this.type = addOne(x)

    def addAll(xs: IterableOnce[A]): this.type = {
        val it = xs.iterator
        if (it.hasNext) {
            addOne(it.next())
            for (x <- it) {
                unsafeAddOne(x)
            }
        }
        this
    }

    def prependOne(x: A): this.type = {
        val start = new Node(x, this.start)
        this.start = start
        if (this.end == null) this.end = this.start
        this
    }

    def stealAll(other: LinkedList[A]): this.type = if (other.nonEmpty) {
        if (this.end != null) this.end.next = other.start
        else this.start = other.start
        this.end = other.end
        other.start = null
        other.end = null
        this
    } else this

    override def isEmpty: Boolean = start == null

    override def iterator: Iterator[A] = new AbstractIterator[A] {
        private[this] var current = start
        def hasNext = current != null
        def next() = { val r = current.x; current = current.next; r }
    }
}

private [internal] object LinkedList {
    private [LinkedList] class Node[A](val x: A, var next: Node[A])
    def empty[A]: LinkedList[A] = new LinkedList[A]
    def apply[A](x: A, xs: A*): LinkedList[A] = {
        val list = new LinkedList[A]
        list += x
        for (x <- xs) list.unsafeAddOne(x)
        list
    }
    // This is pretty inefficient tbh!
    def unapplySeq[A](xs: LinkedList[A]): Some[Seq[A]] = Some(xs.toSeq)
}
