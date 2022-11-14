/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding

import scala.annotation.tailrec

// Trampoline for CPS
private [deepembedding] sealed abstract class Bounce[A] {
    @tailrec final def run: A = this match {
        case thunk: Thunk[A] => thunk.cont().run
        case chunk: Chunk[A] => chunk.x
    }
}
private [deepembedding] final class Chunk[A](val x: A) extends Bounce[A]
private [deepembedding] final class Thunk[A](val cont: () => Bounce[A]) extends Bounce[A]

private [deepembedding] abstract class ContOps[Cont[_, +_]] {
    def wrap[R, A](x: A): Cont[R, A]
    def unwrap[R](wrapped: Cont[R, R]): R
    def map[R, A, B](c: Cont[R, A], f: A => B): Cont[R, B]
    def flatMap[R, A, B](c: Cont[R, A], f: A => Cont[R, B]): Cont[R, B]
    def suspend[R, A](x: =>Cont[R, A]): Cont[R, A]
    // $COVERAGE-OFF$
    // This needs to be lazy, because I'm an idiot when I use it
    def >>[R, A, B](c: Cont[R, A], k: =>Cont[R, B]): Cont[R, B] = flatMap[R, A, B](c, _ => k)
    def |>[R, A, B](c: Cont[R, A], x: =>B): Cont[R, B] = map[R, A, B](c, _ => x)
    // $COVERAGE-ON$
}
private [deepembedding] object ContOps {
    implicit class ContAdapter[R, A, Cont[_, +_]](val c: Cont[R, A]) extends AnyVal {
        @inline def map[B](f: A => B)(implicit ops: ContOps[Cont]): Cont[R, B] = ops.map(c, f)
        @inline def flatMap[B](f: A => Cont[R, B])(implicit ops: ContOps[Cont]): Cont[R, B] = ops.flatMap(c, f)
        // This needs to be lazy, because I'm an idiot when I use it
        @inline def >>[B](k: =>Cont[R, B])(implicit ops: ContOps[Cont]): Cont[R, B] = ops.>>(c, k)
        @inline def |>[B](x: =>B)(implicit ops: ContOps[Cont]): Cont[R, B] = ops.|>(c, x)
    }
    @inline def result[R, A, Cont[_, +_]](x: A)(implicit canWrap: ContOps[Cont]): Cont[R, A] = canWrap.wrap(x)
    @inline def perform[Cont[_, +_], R](wrapped: Cont[R, R])(implicit canUnwrap: ContOps[Cont]): R = canUnwrap.unwrap(wrapped)
    @inline def suspend[Cont[_, +_], R, A](x: =>Cont[R, A])(implicit ops: ContOps[Cont]): Cont[R, A] = ops.suspend(x)
    type GenOps = ContOps[({type C[_, +_]})#C] // scalastyle:ignore structural.type
    def safeCall[A](task: GenOps => A): A = {
        try task(Id.ops.asInstanceOf[GenOps])
        catch { case _: StackOverflowError => task(Cont.ops.asInstanceOf[GenOps]) }
    }
    // $COVERAGE-OFF$
    def sequence[Cont[_, +_]: ContOps, R, A](mxs: List[Cont[R, A]]): Cont[R, List[A]] = mxs match {
        case Nil => result(Nil)
        case mx :: mxs => for { x <- mx; xs <- sequence(mxs) } yield x :: xs
    }
    // $COVERAGE-ON$
}

private [deepembedding] final class Cont[R, +A](val cont: (A => Bounce[R]) => Bounce[R]) extends AnyVal
private [deepembedding] object Cont {
    implicit val ops: ContOps[Cont] = new ContOps[Cont] {
        override def wrap[R, A](x: A): Cont[R, A] = new Cont(k => new Thunk(() => k(x)))
        override def unwrap[R](wrapped: Cont[R, R]): R = wrapped.cont(x => new Chunk(x)).run
        override def map[R, A, B](mx: Cont[R, A], f: A => B): Cont[R, B] = {
            new Cont(k => new Thunk(() => mx.cont(x => new Thunk(() => k(f(x))))))
        }
        override def flatMap[R, A, B](mx: Cont[R, A], f: A => Cont[R, B]): Cont[R, B] = {
            new Cont(k => new Thunk(() => mx.cont(x => f(x).cont(k))))
        }
        override def suspend[R, A](x: =>Cont[R, A]): Cont[R, A] = new Cont(k => new Thunk(() => x.cont(k)))
        override def >>[R, A, B](mx: Cont[R, A], my: =>Cont[R, B]): Cont[R, B] = {
            new Cont(k => new Thunk(() => mx.cont(_ => my.cont(k))))
        }
    }
}

private [deepembedding] final class Id[R, +A](val x: A) extends AnyVal
private [deepembedding] object Id {
    implicit val ops: ContOps[Id] = new ContOps[Id] {
        override def wrap[R, A](x: A): Id[R, A] = new Id(x)
        override def unwrap[R](wrapped: Id[R, R]): R = wrapped.x
        override def map[R, A, B](c: Id[R, A], f: A => B): Id[R, B] = new Id(f(c.x))
        override def flatMap[R, A, B](c: Id[R, A], f: A => Id[R, B]): Id[R, B] = f(c.x)
        override def suspend[R, A](x: =>Id[R, A]): Id[R, A] = x
        override def |>[R, A, B](c: Id[R, A], x: =>B): Id[R, B] = new Id(x)
    }
}
