package parsley.internal.machine.stacks

import scala.annotation.tailrec

private [machine] abstract class Stack[T] {
    protected type ElemTy
    protected def show(x: ElemTy): String
    final def isEmpty(xs: T): Boolean = xs == null
    protected def head(xs: T): ElemTy
    protected def tail(xs: T): T
}
private [machine] object Stack {
    @inline def empty[T >: Null]: T = null
    implicit class StackExt[T](val xs: T) extends AnyVal {
        def mkString(sep: String)(implicit inst: Stack[T]): String = {
            var ys = xs
            val str = new StringBuilder
            str ++= "["
            while (!inst.isEmpty(ys)) {
                str ++= inst.show(inst.head(xs))
                ys = inst.tail(ys)
                if (!inst.isEmpty(ys)) str ++= sep
            }
            str ++= "]"
            str.result()
        }
        @inline def isEmpty(implicit inst: Stack[T]): Boolean = inst.isEmpty(xs)
    }
}