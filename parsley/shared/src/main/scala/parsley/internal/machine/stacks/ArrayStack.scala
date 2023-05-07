/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

// Designed to replace the operational stack
// Since elements are of type Any, this serves as a optimised implementation
// Its success may result in the deprecation of the Stack class in favour of a generic version of this!
private [machine] final class ArrayStack[A](initialSize: Int = ArrayStack.DefaultSize) {
    private [this] var array: Array[Any] = new Array(initialSize)
    private [this] var sp = -1

    def push(x: A): Unit = {
        sp += 1
        if (array.length == sp) {
            val newArray: Array[Any] = new Array(sp << 1)
            java.lang.System.arraycopy(array, 0, newArray, 0, sp)
            array = newArray
        }
        array(sp) = x
    }

    def upush(x: A): Unit = {
        sp += 1
        array(sp) = x
    }

    def exchange(x: A): Unit = array(sp) = x
    def peekAndExchange(x: A): Any = {
        val y = array(sp)
        array(sp) = x
        y
    }
    def pop_(): Unit = sp -= 1
    def upop(): Any = {
        val x = array(sp)
        sp -= 1
        x
    }
    def pop[B <: A](): B = upop().asInstanceOf[B]
    def upeek: Any = array(sp)
    def peek[B <: A]: B = upeek.asInstanceOf[B]

    def drop(x: Int): Unit = sp -= x

    // This is off by one, but that's fine, if everything is also off by one :P
    def usize: Int = sp
    // $COVERAGE-OFF$
    def size: Int = usize + 1
    def isEmpty: Boolean = sp == -1
    def mkString(sep: String): String = array.take(sp + 1).reverse.mkString(sep)
    // $COVERAGE-ON$
}
private [machine] object ArrayStack {
    final val DefaultSize = 8
}
