/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.collection.mutable

import SinglyLinkedList.{Node, LinkedListIterator}

private [internal] class SinglyLinkedList[A] private [SinglyLinkedList]
    (private [SinglyLinkedList] var start: Node[A],
     private [SinglyLinkedList] var end: Node[A]) extends Iterable[A] {

    private [SinglyLinkedList] def this() = this(null, null)

    // This method assumes that this list is non-empty
    private [SinglyLinkedList] def unsafeAddOne(x: A): this.type = {
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

    def ::(x: A): SinglyLinkedList[A] = new SinglyLinkedList(new Node(x, this.start), this.end)

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

    def stealAll(other: SinglyLinkedList[A]): this.type = if (other.nonEmpty) {
        if (this.end != null) this.end.next = other.start
        else this.start = other.start
        this.end = other.end
        other.start = null
        other.end = null
        this
    } else this

    override def isEmpty: Boolean = start == null

    override def lastOption: Option[A] = if (end != null) Some(end.x) else None
    override def headOption: Option[A] = if (start != null) Some(start.x) else None

    override def iterator: LinkedListIterator[A] = new LinkedListIterator[A] {
        override var current = start
        override val end = SinglyLinkedList.this.end
    }
}

private [internal] object SinglyLinkedList {
    private [SinglyLinkedList] class Node[A](val x: A, var next: Node[A])
    def empty[A]: SinglyLinkedList[A] = new SinglyLinkedList[A]
    def apply[A](x: A, xs: A*): SinglyLinkedList[A] = {
        val list = new SinglyLinkedList[A]
        list += x
        for (x <- xs) list.unsafeAddOne(x)
        list
    }

    abstract class LinkedListIterator[A] extends Iterator[A] {
        protected var current: Node[A]
        protected val end: Node[A]
        def hasNext = current != null
        def next() = { val r = current.x; current = current.next; r }
        def remaining: SinglyLinkedList[A] = {
            if (hasNext) {
                new SinglyLinkedList(current, end)
            }
            else {
                new SinglyLinkedList
            }
        }
    }
}
