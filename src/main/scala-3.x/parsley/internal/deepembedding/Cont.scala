package parsley.internal.deepembedding

import scala.language.{higherKinds, reflectiveCalls}
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

private [deepembedding] abstract class ContOps[Cont[_, +_]]
{
    def wrap[R, A](x: A): Cont[R, A]
    def unwrap[R](wrapped: Cont[R, R]): R
    def map[R, A, B](c: =>Cont[R, A], f: A => B): Cont[R, B]
    def flatMap[R, A, B](c: =>Cont[R, A], f: A => Cont[R, B]): Cont[R, B]
    // $COVERAGE-OFF$
    def >>[R, A, B](c: =>Cont[R, A], k: =>Cont[R, B]): Cont[R, B] = flatMap[R, A, B](c, _ => k)
    def |>[R, A, B](c: =>Cont[R, A], x: =>B): Cont[R, B] = map[R, A, B](c, _ => x)
    // $COVERAGE-ON$
}
private [deepembedding] object ContOps
{
    implicit class ContAdapter[R, A, Cont[_, +_]](c: =>Cont[R, A])(implicit ops: ContOps[Cont])
    {
        def map[B](f: A => B): Cont[R, B] = ops.map(c, f)
        def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = ops.flatMap(c, f)
        def >>[B](k: =>Cont[R, B]): Cont[R, B] = ops.>>(c, k)
        def |>[B](x: =>B): Cont[R, B] = ops.|>(c, x)
    }
    def result[R, A, Cont[_, +_]](x: A)(implicit canWrap: ContOps[Cont]): Cont[R, A] = canWrap.wrap(x)
    def perform[R, Cont[_, +_]](wrapped: Cont[R, R])(implicit canUnwrap: ContOps[Cont]): R = canUnwrap.unwrap(wrapped)
    type GenOps = ContOps[({type C[_, +_]})#C]
    def safeCall[A](task: GenOps => A): A = {
        try task(Id.ops.asInstanceOf[GenOps])
        catch { case _: StackOverflowError => task(Cont.ops.asInstanceOf[GenOps]) }
    }
}

private [deepembedding] class Cont[R, +A](val cont: (A => Bounce[R]) => Bounce[R]) extends AnyVal
private [deepembedding] object Cont
{
    implicit val ops: ContOps[Cont] = new ContOps[Cont]
    {
        override def wrap[R, A](x: A): Cont[R, A] = new Cont(k => new Thunk(() => k(x)))
        override def unwrap[R](wrapped: Cont[R, R]): R = wrapped.cont(x => new Chunk(x)).run
        override def map[R, A, B](mx: =>Cont[R, A], f: A => B): Cont[R, B] =
        {
            new Cont(k => new Thunk(() => mx.cont(x => new Thunk(() => k(f(x))))))
        }
        override def flatMap[R, A, B](mx: =>Cont[R, A], f: A => Cont[R, B]): Cont[R, B] =
        {
            new Cont(k => new Thunk(() => mx.cont(x => f(x).cont(k))))
        }
        override def >>[R, A, B](mx: => Cont[R, A], my: => Cont[R, B]): Cont[R, B] =
        {
            new Cont(k => new Thunk(() => mx.cont(_ => my.cont(k))))
        }
    }
}

private [deepembedding] class Id[R, +A](val x: A)
private [deepembedding] object Id
{
    implicit val ops: ContOps[Id] = new ContOps[Id]
    {
        override def wrap[R, A](x: A): Id[R, A] = new Id(x)
        override def unwrap[R](wrapped: Id[R, R]): R = wrapped.x
        override def map[R, A, B](c: =>Id[R, A], f: A => B): Id[R, B] = new Id(f(c.x))
        override def flatMap[R, A, B](c: =>Id[R, A], f: A => Id[R, B]): Id[R, B] = f(c.x)
        override def >>[R, A, B](c: => Id[R, A], k: => Id[R, B]): Id[R, B] = {c; k}
        override def |>[R, A, B](c: => Id[R, A], x: => B): Id[R, B] = {c; new Id(x)}
    }
}
