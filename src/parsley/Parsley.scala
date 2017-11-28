package parsley

import parsley.Parsley._

import scala.annotation.tailrec

// TODO Perform final optimisation stage on end result, likely to perform some extra optimisation, but possibly less
class Parsley[+A](
    // The instructions that shall be executed by this parser
    val instrs: Vector[Instruction],
    // The subroutines currently collected by the compilation
    val subs: Map[String, Vector[Instruction]])
{
    def flatMap[B](f: A => Parsley[B]): Parsley[B] = instrs.last match
    {
        // return x >>= f == f x
        case Push(x: A) => new Parsley(instrs.init ++ f(x).instrs, subs)
        case _ => new Parsley(instrs :+ DynSub[A](x => f(x).instrs), subs)
    }
    def map[B](f: A => B): Parsley[B] = instrs.last match
    {
        // Pure application can be resolved at compile-time
        case Push(x: A) => new Parsley(instrs.init :+ Push(f(x)), subs)
        // p.map(f).map(g) = p.map(g . f) (functor law)
        case Perform(g) => new Parsley(instrs.init :+ Perform((x: C forSome {type C}) => f(g.asInstanceOf[Function[C forSome {type C}, A]](x))), subs)
        case _ => new Parsley(instrs :+ Perform(f), subs)
    }
    def >>[B](p: Parsley[B]): Parsley[B] = this *> p
    def *>[B](p: Parsley[B]): Parsley[B] = instrs.last match
    {
        // pure x *> p == p (consequence of applicative and functor laws)
        case Push(_) => new Parsley(instrs.init ++ p.instrs, subs ++ p.subs)
        case _ => new Parsley((instrs :+ Pop) ++ p.instrs, subs ++ p.subs)
    }
    def <*[B](p: Parsley[B]): Parsley[A] = p.instrs.last match
    {
        // p <* pure x == p (consequence of applicative and functor laws)
        case Push(_) => new Parsley(instrs ++ p.instrs.init, subs ++ p.subs)
        case _ => new Parsley(instrs ++ p.instrs :+ Pop, subs ++ p.subs)
    }
    def <*>:[B](p: Parsley[A => B]): Parsley[B] = instrs.last match
    {
        // pure(f) <*> p == f <#> p (consequence of applicative laws)
        case Push(f) => p.instrs.last match
        {
            // f <#> pure x == pure (f x) (applicative law)
            case Push(x: A) => new Parsley(instrs.init ++ p.instrs.init :+ Push(f.asInstanceOf[Function[A, B]](x)), subs ++ p.subs)
            // p.map(f).map(g) = p.map(g . f) (functor law)
            case Perform(g) =>
                new Parsley(instrs.init ++ p.instrs.init :+
                            Perform((x: C forSome {type C}) =>
                                f.asInstanceOf[Function[A, B]](g.asInstanceOf[Function[C forSome {type C}, A]](x))), subs ++ p.subs)
            case _ => new Parsley(instrs.init ++ p.instrs :+ Perform[A, B](f.asInstanceOf[Function[A, B]]), subs ++ p.subs)
        }
        case _ => new Parsley(instrs ++ p.instrs :+ Apply, subs ++ p.subs)
    }
    def <*>[B, C](p: Parsley[B])(implicit ev: A => (B => C)): Parsley[C] = instrs.last match
    {
        // pure(f) <*> p == f <#> p (consequence of applicative laws)
        case Push(f) => p.instrs.last match
        {
            // f <#> pure x == pure (f x) (applicative law)
            case Push(x: A) => new Parsley(instrs.init ++ p.instrs.init :+ Push(f.asInstanceOf[Function[A, B]](x)), subs ++ p.subs)
            // p.map(f).map(g) = p.map(g . f) (functor law)
            case Perform(g) =>
                new Parsley(instrs.init ++ p.instrs.init :+
                    Perform((x: C forSome {type C}) =>
                        f.asInstanceOf[Function[A, B]](g.asInstanceOf[Function[C forSome {type C}, A]](x))), subs ++ p.subs)
            case _ => new Parsley(instrs.init ++ p.instrs :+ Perform[A, B](f.asInstanceOf[Function[A, B]]), subs ++ p.subs)
        }
        case _ => new Parsley(instrs ++ p.instrs :+ Apply, subs ++ p.subs)
    }
    def <::>[A_ >: A](ps: Parsley[List[A_]]): Parsley[List[A_]] = pure[A => List[A_] => List[A_]](x => xs => x::xs) <*> this <*> ps
    def <|>[A_ >: A](q: Parsley[A_]): Parsley[A_] = instrs match
    {
        // pure results always succeed
        case Vector(Push(_)) => new Parsley[A_](instrs, subs ++ q.subs)
        // empty <|> q == q (alternative law)
        case Vector(Fail(_)) => q
        case _ => q.instrs match
        {
            // p <|> empty = p (alternative law)
            case Vector(Fail(_)) => this
            // I imagine there is space for optimisation of common postfix and prefixes in choice
            // this would allow for further optimisations with surrounding integration
            // does it imply that there is a try scope wrapping p?
            case _ =>
                nextLabel += 1
                new Parsley[A_]((instrs :+ JumpGood(nextLabel)) ++ q.instrs :+ Label(nextLabel), subs ++ q.subs)
        }
    }
    override def toString(): String = s"(${instrs.toString}, ${subs.toString})"
}

object Parsley
{
    def pure[A](a: A): Parsley[A] = new Parsley[A](Vector(Push(a)), Map.empty)
    def fail[A](msg: String): Parsley[A] = new Parsley[A](Vector(Fail(msg)), Map.empty)
    def empty[A](): Parsley[A] = fail("unknown error")
    def tryParse[A](p: Parsley[A]): Parsley[A] = new Parsley(TryBegin +: p.instrs :+ TryEnd, p.subs)

    var knotScope: Set[String] = Set.empty
    var nextLabel: Int = -1
    def reset() =
    {
        knotScope = Set.empty
        nextLabel = -1
    }
    def knot[A](name: String, p_ : =>Parsley[A]): Parsley[A] =
    {
        lazy val p = p_
        if (knotScope.contains(name)) new Parsley(Vector(Call(name)), Map.empty)
        else
        {
            knotScope += name
            // Perform inline expansion optimisation, reduce to minimum knot-tie
            val instrs = p.instrs.flatMap
            {
                case Call(name_) if name != name_ && p.subs.contains(name_) => p.subs(name_)
                case instr => Vector(instr)
            }
            new Parsley(Vector(Call(name)), p.subs + (name -> instrs))
        }
    }

    implicit class Knot[A](name: String)
    {
        def <%>(p: =>Parsley[A]): Parsley[A] = knot(name, p)
    }

    def optimise[A](p: Parsley[A]): Parsley[A] =
    {
        val instrs = p.instrs
        val subs = p.subs
        @tailrec
        def process(instrs: Vector[Instruction]): Vector[Instruction] = process(instrs)
        new Parsley(process(instrs), subs)
    }

    def inf: Parsley[Int] = "inf" <%> inf.map[Int](_+1).map[Int](_+2)
    def expr: Parsley[Int] = "expr" <%> (pure[Int=>Int=>Int](x => y => x + y) <*> pure[Int](10) <*> expr)
    def monad: Parsley[Int] = for (x <- pure[Int](10); y <- pure[Int](20)) yield x + y
    def foo: Parsley[Int] = "foo" <%> (bar <* pure(20))
    def bar: Parsley[Int] = "bar" <%> (foo *> pure(10))
    def many[A](p: Parsley[A]): Parsley[List[A]] = s"many(${p.instrs})" <%> (some(p) <|> pure[List[A]](Nil))
    def some[A](p: Parsley[A]): Parsley[List[A]] = s"some(${p.instrs})" <%> (p <::> many(p))
    def repeat[A](n: Int, p: Parsley[A]): Parsley[List[A]] =
        if (n > 0) (p <::> repeat[A](n-1, p))
        else pure[List[A]](Nil)

    def main(args: Array[String]): Unit =
    {
        println(pure[Int=>Int=>Int](x => y => x + y) <*> pure(10) <*> pure(20))
        println(inf)
        println(expr)
        println(monad)
        println(foo)
        println(many(pure[Int](10)))
        println(repeat(10, pure[Unit](())))
    }
}
