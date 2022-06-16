/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.collection.mutable

import DoublyLinkedList.Node
import scala.collection.AbstractIterator

private [internal] class DoublyLinkedList[A] private [DoublyLinkedList]
    (private [DoublyLinkedList] var start: Node[A],
     private [DoublyLinkedList] var end: Node[A]) extends Iterable[A] {

    private [DoublyLinkedList] def this() = this(null, null)

    // This method assumes that this list is non-empty
    private [DoublyLinkedList] def unsafeAddOne(x: A): this.type = {
        val next = new Node(x, null, this.start)
        if (this.end != null) this.end.next = next
        this.end = next
        this
    }
    def addOne(x: A): this.type = {
        if (this.start == null) {
            this.start = new Node(x, null, null)
            this.end = this.start
            this
        }
        else unsafeAddOne(x)
    }
    def +=(x: A): this.type = addOne(x)

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
        val start = new Node(x, this.start, null)
        this.start = start
        if (this.end == null) this.end = this.start
        this
    }

    def stealAll(other: DoublyLinkedList[A]): this.type = if (other.nonEmpty) {
        if (this.end != null) {
            this.end.next = other.start
            other.start.prev = this.end
        }
        else this.start = other.start
        this.end = other.end
        other.start = null
        other.end = null
        this
    } else this

    override def isEmpty: Boolean = start == null

    override def lastOption: Option[A] = if (end != null) Some(end.x) else None
    override def headOption: Option[A] = if (start != null) Some(start.x) else None

    override def iterator: Iterator[A] = new AbstractIterator[A] {
        var current: Node[A] = start
        def hasNext = current != null
        def next() = { val r = current.x; current = current.next; r }
    }

    def reverseIterator: Iterator[A] = new AbstractIterator[A] {
        var current: Node[A] = end
        def hasNext = current != null
        def next() = { val r = current.x; current = current.prev; r }
    }
}

private [internal] object DoublyLinkedList {
    private [DoublyLinkedList] class Node[A](val x: A, var next: Node[A], var prev: Node[A])
    def empty[A]: DoublyLinkedList[A] = new DoublyLinkedList[A]
    def apply[A](x: A, xs: A*): DoublyLinkedList[A] = {
        val list = new DoublyLinkedList[A]
        list += x
        for (x <- xs) list.unsafeAddOne(x)
        list
    }
}
