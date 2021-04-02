package parsley.internal.deepembedding

import scala.language.{higherKinds, reflectiveCalls, implicitConversions}
import scala.annotation.tailrec

// Trampoline for CPS
private [deepembedding] sealed abstract class Bounce[A]
{
    @tailrec final def run: A = this match
    {
        case thunk: Thunk[A] => thunk.cont().run
        case chunk: Chunk[A] => chunk.x
    }
}
private [deepembedding] final class Chunk[A](val x: A) extends Bounce[A]
private [deepembedding] final class Thunk[A](val cont: () => Bounce[A]) extends Bounce[A]

private [deepembedding] abstract class ContOps[Cont[_, +_], R]
{
    def wrap[A](x: A): Cont[R, A]
    def unwrap(wrapped: Cont[R, R]): R
    def map[A, B](c: =>Cont[R, A], f: A => B): Cont[R, B]
    def flatMap[A, B](c: =>Cont[R, A], f: A => Cont[R, B]): Cont[R, B]
    // $COVERAGE-OFF$
    def >>[A, B](c: =>Cont[R, A], k: =>Cont[R, B]): Cont[R, B] = flatMap[A, B](c, _ => k)
    def |>[A, B](c: =>Cont[R, A], x: =>B): Cont[R, B] = map[A, B](c, _ => x)
    // $COVERAGE-ON$
}

private [deepembedding] object ContOps
{
    implicit def rIsPhantom[Cont[_, +_], R](implicit ops: ContOps[Cont, _]): ContOps[Cont, R] = ops.asInstanceOf[ContOps[Cont, R]]
    implicit class ContAdapter[R, A, Cont[_, +_]](c: =>Cont[R, A])(implicit ops: ContOps[Cont, R])
    {
        def map[B](f: A => B): Cont[R, B] = ops.map(c, f)
        def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = ops.flatMap(c, f)
        def >>[B](k: =>Cont[R, B]): Cont[R, B] = ops.>>(c, k)
        def |>[B](x: =>B): Cont[R, B] = ops.|>(c, x)
    }
    def result[R, A, Cont[_, +_]](x: A)(implicit canWrap: ContOps[Cont, R]): Cont[R, A] = canWrap.wrap(x)
    def perform[R, Cont[_, +_]](wrapped: Cont[R, R])(implicit canUnwrap: ContOps[Cont, R]): R = canUnwrap.unwrap(wrapped)
    type GenOps[R] = ContOps[({type C[_, +_]})#C, R]
    def safeCall[R, A](task: GenOps[R] => A): A = {
        try task(Id.ops.asInstanceOf[GenOps[R]])
        catch { case _: StackOverflowError => task(Cont.ops.asInstanceOf[GenOps[R]]) }
    }
}

private [deepembedding] class Cont[R, +A](val cont: (A => Bounce[R]) => Bounce[R]) extends AnyVal
private [deepembedding] object Cont
{
    implicit def ops[R]: ContOps[Cont, R] = new ContOps[Cont, R]
    {
        override def wrap[A](x: A): Cont[R, A] = new Cont(k => new Thunk(() => k(x)))
        override def unwrap(wrapped: Cont[R, R]): R = wrapped.cont(x => new Chunk(x)).run
        override def map[A, B](mx: =>Cont[R, A], f: A => B): Cont[R, B] =
        {
            new Cont(k => new Thunk(() => mx.cont(x => new Thunk(() => k(f(x))))))
        }
        override def flatMap[A, B](mx: =>Cont[R, A], f: A => Cont[R, B]): Cont[R, B] =
        {
            new Cont(k => new Thunk(() => mx.cont(x => f(x).cont(k))))
        }
        override def >>[A, B](mx: => Cont[R, A], my: => Cont[R, B]): Cont[R, B] =
        {
            new Cont(k => new Thunk(() => mx.cont(_ => my.cont(k))))
        }
    }
}

private [deepembedding] class Id[R, +A](val x: A)
private [deepembedding] object Id
{
    implicit def ops[R]: ContOps[Id, R] = new ContOps[Id, R]
    {
        override def wrap[A](x: A): Id[R, A] = new Id(x)
        override def unwrap(wrapped: Id[R, R]): R = wrapped.x
        override def map[A, B](c: =>Id[R, A], f: A => B): Id[R, B] = new Id(f(c.x))
        override def flatMap[A, B](c: =>Id[R, A], f: A => Id[R, B]): Id[R, B] = f(c.x)
        override def >>[A, B](c: => Id[R, A], k: => Id[R, B]): Id[R, B] = {c; k}
        override def |>[A, B](c: => Id[R, A], x: => B): Id[R, B] = {c; new Id(x)}
    }
}
