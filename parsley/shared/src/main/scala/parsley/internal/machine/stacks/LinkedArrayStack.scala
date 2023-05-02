/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

import scala.annotation.tailrec
import scala.collection.mutable

private [machine] final class LinkedArrayStack[A](initialSize: Int = LinkedArrayStack.DefaultSize) {
    private [this] var array: Array[Any] = new Array(initialSize + LinkedArrayStack.ScratchSize)
    private [this] var curCap = initialSize
    private [this] var sp = -1

    def push(x: A): Unit = {
        sp += 1
        if (curCap == sp) {
            curCap <<= 1
            val successor = array(sp+1)
            if (successor != null) array = successor.asInstanceOf[Array[Any]]
            else {
                val newArray: Array[Any] = new Array(curCap + LinkedArrayStack.ScratchSize)
                newArray(curCap) = array
                array(sp+1) = newArray
                array = newArray
            }
            sp = 0
        }
        array(sp) = x
    }

    def upush(x: A): Unit = {
        sp += 1
        if (curCap == sp) {
            curCap <<= 1
            array = array(sp+1).asInstanceOf[Array[Any]]
            sp = 0
        }
        array(sp) = x
    }

    def exchange(x: A): Unit = array(sp) = x
    def peekAndExchange(x: A): Any = {
        val y = array(sp)
        array(sp) = x
        y
    }
    def pop_(): Unit = {
        if (sp > 0 || curCap == initialSize) sp -= 1
        else {
            // crossed a boundary
            array(curCap + 1) = null // clear the next array along, we've reached a quarter of that size
            array = array(curCap).asInstanceOf[Array[Any]]
            curCap >>= 1
            sp = curCap-1
        }
    }
    def upop(): Any = {
        val x = array(sp)
        pop_()
        x
    }
    def pop[B <: A](): B = upop().asInstanceOf[B]
    def upeek: Any = array(sp)
    def peek[B <: A]: B = upeek.asInstanceOf[B]

    @tailrec final def drop(x: Int): Unit = {
        if (sp >= x || curCap == initialSize) sp -= x
        else {
            val y = x - sp
            // crossed a boundary
            array(curCap + 1) = null // clear the next array along, we've reached a quarter of that size
            array = array(curCap).asInstanceOf[Array[Any]]
            curCap >>= 1
            sp = curCap-1
            drop(y)
        }
    }

    // This is off by one, but that's fine, if everything is also off by one :P
    def usize: Int = curCap - initialSize + sp
    // $COVERAGE-OFF$
    def size: Int = usize + 1
    def isEmpty: Boolean = sp == -1
    def mkString(sep: String): String = { // should work?
        val xs = mutable.ListBuffer.empty[Any]
        var arr = array
        xs ++= arr.take(sp + 1).reverse
        arr = arr(arr.length - 2).asInstanceOf[Array[Any]]
        while (arr != null) {
            arr = arr(arr.length - 2).asInstanceOf[Array[Any]]
            xs ++= arr.reverse.drop(2)
        }
        xs.mkString(sep)
    }
    // $COVERAGE-ON$
}
private [machine] object LinkedArrayStack {
    final val DefaultSize = 8
    final val ScratchSize = 2
}
