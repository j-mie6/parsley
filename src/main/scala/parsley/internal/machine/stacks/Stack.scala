/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

private [machine] abstract class Stack[T] {
    protected type ElemTy
    final def isEmpty(xs: T): Boolean = xs == null
    // $COVERAGE-OFF$
    protected def head(xs: T): ElemTy
    protected def tail(xs: T): T
    protected def show(x: ElemTy): String
    // $COVERAGE-ON$
}
private [machine] object Stack {
    @inline def empty[T >: Null]: T = null
    implicit class StackExt[T](val xs: T) extends AnyVal {
        // $COVERAGE-OFF$
        def mkString(sep: String)(implicit inst: Stack[T]): String = {
            var ys = xs // scalastyle:ignore var.local
            val str = new StringBuilder
            str ++= "["
            while (!inst.isEmpty(ys)) {
                str ++= inst.show(inst.head(ys))
                ys = inst.tail(ys)
                if (!inst.isEmpty(ys)) str ++= sep
            }
            str ++= "]"
            str.result()
        }
        // $COVERAGE-ON$
        @inline def isEmpty(implicit inst: Stack[T]): Boolean = inst.isEmpty(xs)
    }
}
