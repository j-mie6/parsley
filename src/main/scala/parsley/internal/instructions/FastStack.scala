package parsley.internal.instructions

import scala.annotation.tailrec

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