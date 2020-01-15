package parsley

import scala.language.{higherKinds, reflectiveCalls}

private [parsley] abstract class ContOps[Cont[_, _]]
{
    def wrap[R, A](x: A): Cont[R, A]
    def unwrap[R](wrapped: Cont[R, R]): R
    def map[R, A, B](c: =>Cont[R, A], f: A => B): Cont[R, B]
    def flatMap[R, A, B](c: =>Cont[R, A], f: A => Cont[R, B]): Cont[R, B]
    def >>[R, A, B](c: =>Cont[R, A], k: =>Cont[R, B]): Cont[R, B] = flatMap[R, A, B](c, _ => k)
    def |>[R, A, B](c: =>Cont[R, A], x: =>B): Cont[R, B] = map[R, A, B](c, _ => x)
}
private [parsley] object ContOps
{
    implicit class ContAdapter[R, A, Cont[_, _]](c: =>Cont[R, A])(implicit ops: ContOps[Cont])
    {
        def map[B](f: A => B): Cont[R, B] = ops.map(c, f)
        def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = ops.flatMap(c, f)
        def >>[B](k: =>Cont[R, B]): Cont[R, B] = ops.>>(c, k)
        def |>[B](x: =>B): Cont[R, B] = ops.|>(c, x)
    }
    def result[R, A, Cont[_, _]](x: A)(implicit canWrap: ContOps[Cont]): Cont[R, A] = canWrap.wrap(x)
    def perform[R, Cont[_, _]](wrapped: Cont[R, R])(implicit canUnwrap: ContOps[Cont]): R = canUnwrap.unwrap(wrapped)
    type GenOps = ContOps[({type C[_, _]})#C]
    def safeCall[A](task: GenOps => A): A =
        try task(Id.ops.asInstanceOf[GenOps])
        catch { case _: StackOverflowError => task(Cont.ops.asInstanceOf[GenOps]) }
}

private [parsley] class Cont[R, A](val cont: (A => Bounce[R]) => Bounce[R]) extends AnyVal
private [parsley] object Cont
{
    implicit val ops: ContOps[Cont] = new ContOps[Cont]
    {
        override def wrap[R, A](x: A): Cont[R, A] = new Cont(f => f(x))
        override def unwrap[R](wrapped: Cont[R, R]): R = wrapped.cont(x => new Chunk(x)).run
        override def map[R, A, B](c: =>Cont[R, A], f: A => B): Cont[R, B] =
        {
            new Cont(g => new Thunk(() => c.cont(x => new Thunk(() => g(f(x))))))
        }
        override def flatMap[R, A, B](c: =>Cont[R, A], f: A => Cont[R, B]): Cont[R, B] =
        {
            new Cont(g => new Thunk(() => c.cont(x => f(x).cont(g))))
        }
        override def >>[R, A, B](c: => Cont[R, A], k: => Cont[R, B]): Cont[R, B] =
        {
            new Cont(g => new Thunk(() => c.cont(_ => k.cont(g))))
        }
        override def |>[R, A, B](c: => Cont[R, A], x: => B): Cont[R, B] =
        {
            new Cont(g => new Thunk(() => c.cont(_ => g(x))))
        }
    }

    def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    {
        new Cont[R, A](k => f(x => new Cont[R, B](_ => k(x))).cont(k))
    }
}

private [parsley] class Id[_, A](var x: A)
private [parsley] object Id
{
    implicit val ops: ContOps[Id] = new ContOps[Id]
    {
        override def wrap[R, A](x: A): Id[R, A] = new Id(x)
        override def unwrap[R](wrapped: Id[R, R]): R = wrapped.x
        override def map[R, A, B](c: =>Id[R, A], f: A => B): Id[R, B] = //new Id(f(c.x))
        {
            // I'm sorry, but this /is/ a little faster...
            val i = c
            i.x = f(i.x).asInstanceOf[A]
            i.asInstanceOf[Id[R, B]]
        }
        override def flatMap[R, A, B](c: =>Id[R, A], f: A => Id[R, B]): Id[R, B] = f(c.x)
        override def >>[R, A, B](c: => Id[R, A], k: => Id[R, B]): Id[R, B] = {c; k}
        override def |>[R, A, B](c: => Id[R, A], x: => B): Id[R, B] =
        {
            val i = c
            i.x = x.asInstanceOf[A]
            i.asInstanceOf[Id[R, B]]
        }
    }
}