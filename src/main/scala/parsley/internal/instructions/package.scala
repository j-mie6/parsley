package parsley.internal

import scala.annotation.tailrec
import scala.language.implicitConversions

package object instructions
{
    private [instructions] sealed abstract class Status
    private [instructions] case object Good extends Status
    private [instructions] case object Recover extends Status
    private [instructions] case object Failed extends Status

    private [internal] abstract class Instr {
        def apply(ctx: Context): Unit
        def relabel(labels: Array[Int]): Unit = ()
        // Instructions should override this if they have mutable state inside!
        def copy: Instr = this
    }

    private [internal] trait Stateful

    private [internal] abstract class JumpInstr extends Instr {
        var label: Int
        override def relabel(labels: Array[Int]): Unit = label = labels(label)
    }

    // It's 2018 and Labels are making a come-back, along with 2 pass assembly
    private [internal] final class Label(val i: Int) extends Instr {
        // $COVERAGE-OFF$
        def apply(ctx: Context): Unit = throw new Exception("Cannot execute label")
        // $COVERAGE-ON$
    }

    // $COVERAGE-OFF$
    final private [parsley] def pretty(instrs: Array[Instr]): String = {
        val n = instrs.length
        val digits = if (n != 0) Math.log10(n).toInt + 1 else 0
        instrs.zipWithIndex.map {
            case (instr, idx) =>
                val paddedIdx = {
                    val str = idx.toString
                    " " * (digits - str.length) + str
                }
                val paddedHex = {
                    val str = instr.##.toHexString
                    "0" * (8 - str.length) + str
                }
                s"$paddedIdx [$paddedHex]: $instr"
        }.mkString(";\n")
    }
    // $COVERAGE-ON$

    final private [internal] def stateSafeCopy(instrs: Array[Instr], pindices: Array[Int]): Array[Instr] = {
        val nstateful = pindices.length
        if (nstateful != 0) {
            val instrs_ = instrs.clone
            for (i <- 0 until nstateful) {
                val j = pindices(i)
                instrs_(j) = instrs(j).copy
            }
            instrs_
        }
        else instrs
    }

    final private [internal] def statefulIndices(instrs: Array[Instr]): Array[Int] = {
        val buff = new ResizableArray[Int]()
        for (i <- 0 until instrs.length) {
            if (instrs(i).isInstanceOf[Stateful]) buff += i
        }
        buff.toArray
    }

    // This stack class is designed to be ultra-fast: no virtual function calls
    // It will crash with NullPointerException if you try and use head or tail of empty stack
    // But that is illegal anyway
    private [instructions] final class Stack[A](var head: A, val tail: Stack[A])
    private [instructions] object Stack {
        def empty[A]: Stack[A] = null
        @inline def isEmpty(s: Stack[_]): Boolean = s == null
        @tailrec def drop[A](s: Stack[A], n: Int): Stack[A] = if (n > 0 && !isEmpty(s)) drop(s.tail, n - 1) else s
        // $COVERAGE-OFF$
        def map[A, B](s: Stack[A], f: A => B): Stack[B] = if (!isEmpty(s)) new Stack(f(s.head), map(s.tail, f)) else empty
        def mkString(s: Stack[_], sep: String): String = if (isEmpty(s)) "" else s.head.toString + sep + mkString(s.tail, sep)
        // $COVERAGE-ON$
        def push[A](s: Stack[A], x: A): Stack[A] = new Stack(x, s)
    }

    // Designed to replace the operational stack
    // Since elements are of type Any, this serves as a optimised implementation
    // Its success may result in the deprecation of the Stack class in favour of a generic version of this!
    private [instructions] final class ArrayStack[A](initialSize: Int = ArrayStack.DefaultSize) {
        private [this] var array: Array[Any] = new Array(initialSize)
        private [this] var sp = -1

        def push(x: A): Unit = {
            sp += 1
            if (array.length == sp) {
                val newArray: Array[Any] = new Array(sp * 2)
                java.lang.System.arraycopy(array, 0, newArray, 0, sp)
                array = newArray
            }
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

        def update(off: Int, x: A): Unit = array(sp - off) = x
        def apply(off: Int): Any = array(sp - off)

        def drop(x: Int): Unit = sp -= x

        // This is off by one, but that's fine, if everything is also off by one :P
        def usize: Int = sp
        // $COVERAGE-OFF$
        def size: Int = usize + 1
        def isEmpty: Boolean = sp == -1
        def mkString(sep: String): String = array.take(sp + 1).reverse.mkString(sep)
        // $COVERAGE-ON$
        def clear(): Unit = {
            sp = -1
            var i = array.length-1
            while (i >= 0) {
                array(i) = null
                i -= 1
            }
        }
    }
    private [instructions] object ArrayStack {
        val DefaultSize = 8
    }
}
