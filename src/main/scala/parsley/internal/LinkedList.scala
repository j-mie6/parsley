/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal

import LinkedList.{Node, LinkedListIterator}

private [internal] class LinkedList[A] private [LinkedList]
    (private [LinkedList] var start: Node[A],
     private [LinkedList] var end: Node[A]) extends Iterable[A] {

    private [LinkedList] def this() = this(null, null)

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

    def ::(x: A): LinkedList[A] = new LinkedList(new Node(x, this.start), this.end)

    def addAll(xs: Iterable[A]): this.type = addAll(xs.iterator)
    def addAll(it: Iterator[A]): this.type = {
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

    override def iterator: LinkedListIterator[A] = new LinkedListIterator[A] {
        override var current = start
        override val end = LinkedList.this.end
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

    abstract class LinkedListIterator[A] extends Iterator[A] {
        protected var current: Node[A]
        protected val end: Node[A]
        def hasNext = current != null
        def next() = { val r = current.x; current = current.next; r }
        def remaining: LinkedList[A] = {
            if (hasNext) {
                new LinkedList(current, end)
            }
            else {
                new LinkedList
            }
        }
    }
}
