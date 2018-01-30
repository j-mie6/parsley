package parsley

import parsley.Parsley._

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import language.existentials

// TODO Investigate effect of :+= instead of :+ for the buffers
// TODO Perform final optimisation stage on end result, likely to perform some extra optimisation, but possibly less
class Parsley[+A](
    // The instructions that shall be executed by this parser
    val instrs: Buffer[Instruction],
    // The subroutines currently collected by the compilation
    val subs: Map[String, Buffer[Instruction]])
{
    lazy val instrs_ : Array[Instruction] = instrs.toArray
    lazy val subs_ : Map[String, Array[Instruction]] = subs.map{ case (k, v) => k -> v.toArray}
    def flatMap[B](f: A => Parsley[B]): Parsley[B] = instrs.last match
    {
        // return x >>= f == f x
        case Push(x: A @unchecked) => new Parsley(instrs.init ++ f(x).instrs, subs)
        case _ => new Parsley(instrs :+ new DynSub[A](x => f(x).instrs.toArray), subs)
    }
    def map[B](f: A => B): Parsley[B] = instrs.last match
    {
        // Pure application can be resolved at compile-time
        case Push(x: A @unchecked) => new Parsley(instrs.init :+ new Push(f(x)), subs)
        // p.map(f).map(g) = p.map(g . f) (functor law)
        case Perform(g) => new Parsley(instrs.init :+ new Perform(g.asInstanceOf[Function[C forSome {type C}, A]].andThen(f)), subs)
        case _ => new Parsley(instrs :+ new Perform(f), subs)
    }
    @inline def <#>[B](f: =>A => B): Parsley[B] = map(f)
    @inline def <#>:[B](f: =>A => B): Parsley[B] = map(f)
    @inline def >>[B](p: Parsley[B]): Parsley[B] = this *> p
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
    def #>[B](x: B): Parsley[B] = instrs.last match
    {
        // pure x #> y == pure y (consequence of applicative and functor laws)
        case Push(_) => new Parsley(instrs.init :+ new Push(x), subs)
        case _ => new Parsley(instrs :+ Pop :+ new Push(x), subs)
    }
    def <*>:[B](p: Parsley[A => B]): Parsley[B] = p.instrs.last match
    {
        // pure(f) <*> p == f <#> p (consequence of applicative laws)
        case Push(f) => instrs.last match
        {
            // f <#> pure x == pure (f x) (applicative law)
            case Push(x: A @unchecked) => new Parsley(p.instrs.init ++ instrs.init :+ new Push(f.asInstanceOf[Function[A, B]](x)), p.subs ++ subs)
            // p.map(f).map(g) == p.map(g . f) (functor law)
            case Perform(g) =>
                new Parsley(p.instrs.init ++ instrs.init :+
                            new Perform(f.asInstanceOf[Function[A, B]].compose(g.asInstanceOf[Function[C forSome {type C}, A]])), p.subs ++ subs)
            case _ => new Parsley(p.instrs.init ++ instrs :+ new Perform[A, B](f.asInstanceOf[Function[A, B]]), p.subs ++ subs)
        }
        case Perform(f: Function[A, Any=>Any] @unchecked) => instrs.last match
        {
            // fusion law: (f <$> x) <*> pure y == (($y) . f) <$> x
            case Push(y) => new Parsley(p.instrs.init ++ instrs.init :+ new Perform[A, Any](x => f(x)(y)), p.subs ++ subs)
            case _ => new Parsley(p.instrs ++ instrs :+ Apply, p.subs ++ subs)
        }
        case _ => instrs.last match
        {
            // interchange law: u <*> pure y = ($y) <$> u
            case Push(x: A @unchecked) => new Parsley(p.instrs ++ instrs.init :+ new Perform[A => B, B](f => f(x)), p.subs ++ subs)
            case _ => new Parsley(p.instrs ++ instrs :+ Apply, p.subs ++ subs)
        }
    }
    def <*>[B, C](p: Parsley[B])(implicit ev: A => (B => C)): Parsley[C] = instrs.last match
    {
        // pure(f) <*> p == f <#> p (consequence of applicative laws)
        case Push(f) => p.instrs.last match
        {
            // f <#> pure x == pure (f x) (applicative law)
            case Push(x: B @unchecked) => new Parsley(instrs.init ++ p.instrs.init :+ new Push(f.asInstanceOf[Function[B, C]](x)), subs ++ p.subs)
            // p.map(f).map(g) == p.map(g . f) (functor law)
            case Perform(g) =>
                new Parsley(instrs.init ++ p.instrs.init :+
                    new Perform(f.asInstanceOf[Function[B, C]].compose(g.asInstanceOf[Function[A forSome {type A}, B]])), subs ++ p.subs)
            case _ => new Parsley(instrs.init ++ p.instrs :+ new Perform[B, C](f.asInstanceOf[Function[B, C]]), subs ++ p.subs)
        }
        case Perform(f: Function[B, Any=>Any] @unchecked) => p.instrs.last match
        {
            // fusion law: (f <$> x) <*> pure y == (($y) . f) <$> x
            case Push(y) => new Parsley(instrs.init ++ p.instrs.init :+ Perform[B, Any](x => f(x)(y)), subs ++ p.subs)
            case _ => new Parsley(instrs ++ p.instrs :+ Apply, subs ++ p.subs)
        }
        case _ => p.instrs.last match
        {
            // interchange law: u <*> pure y == ($y) <$> u
            case Push(x: B @unchecked) => new Parsley(instrs ++ p.instrs.init :+ Perform[B => C, C](f => f(x)), subs ++ p.subs)
            case _ => new Parsley(instrs ++ p.instrs :+ Apply, subs ++ p.subs)
        }
    }
    @inline def <**>[B](f: Parsley[A => B]): Parsley[B] = lift2[A, A=>B, B]((x, f) => f(x), this, f)
    @inline def <::>[A_ >: A](ps: Parsley[List[A_]]): Parsley[List[A_]] = new Parsley(instrs ++ ps.instrs :+ Cons, subs ++ ps.subs)//lift2[A, List[A_], List[A_]](_::_, this, ps)
    def <|>[A_ >: A](q: Parsley[A_]): Parsley[A_] = instrs match
    {
        // pure results always succeed
        case Buffer(Push(_)) => new Parsley[A_](instrs, subs ++ q.subs)
        // empty <|> q == q (alternative law)
        case Buffer(Fail(_)) => q
        case _ => q.instrs match
        {
            // p <|> empty = p (alternative law)
            case Buffer(Fail(_)) => this
            // p <|> p == p (this needs refinement to be label invariant, we want structure
            case instrs_ if instrs == instrs_ => this
            // I imagine there is space for optimisation of common postfix and prefixes in choice
            // this would allow for further optimisations with surrounding integration
            // does it imply that there is a try scope wrapping p?
            // NOTE: Prefix optimisation appears to be correct i.e.
            //      (x *> y) <|> (x *> z) === x *> (y <|> z) without need of try
            // NOTE: Postfix optimisation is also correct
            //      (y *> x) <|> (z *> x) == (y <|> z) *> x, noting that y and z are necessarily impure but this always holds
            case _ => new Parsley[A_]((new InputCheck(instrs.size+1) +: instrs :+ new JumpGood(q.instrs.size+1)) ++ q.instrs, subs ++ q.subs)
        }
    }
    @inline def </>[A_ >: A](x: A_): Parsley[A_] = this <|> pure(x)
    @inline def <\>[A_ >: A](q: Parsley[A_]): Parsley[A_] = tryParse(this) <|> q
    @inline def <|?>[B](p: Parsley[B], q: Parsley[B])(implicit ev: Parsley[A] => Parsley[Boolean]): Parsley[B] = choose(this, p, q)
    @inline def >?>(pred: A => Boolean, msg: String): Parsley[A] = guard(this, pred, msg)
    @inline def >?>(pred: A => Boolean, msggen: A => String) = guard(this, pred, msggen)
    override def toString: String = s"(${instrs.toString}, ${subs.toString})"
}

object Parsley
{
    def pure[A](a: A): Parsley[A] = new Parsley[A](Buffer(new Push(a)), Map.empty)
    def fail[A](msg: String): Parsley[A] = new Parsley[A](Buffer(new Fail(msg)), Map.empty)
    def fail[A](msggen: Parsley[A], finaliser: A => String): Parsley[A] =  new Parsley[A](msggen.instrs :+ new FastFail(finaliser), msggen.subs)
    def empty[A]: Parsley[A] = fail("unknown error")
    def tryParse[A](p: Parsley[A]): Parsley[A] = new Parsley(new PushHandler(p.instrs.size+1) +: p.instrs :+ Try, p.subs)
    def lookAhead[A](p: Parsley[A]): Parsley[A] = new Parsley(new PushHandler(p.instrs.size+1) +: p.instrs :+ Look, p.subs)
    @inline def lift2[A, B, C](f: (A, B) => C, p: Parsley[A], q: Parsley[B]): Parsley[C] = p.map(x => (y: B) => f(x, y)) <*> q
    //def lift2[A, B, C](f: (A, B) => C, p: Parsley[A], q: Parsley[B]): Parsley[C] = 
    //{
    //    new Parsley(p.instrs ++ q.instrs :+ Lift(f), p.subs ++ q.subs)
    //}
    def char(c: Char): Parsley[Char] = new Parsley(Buffer(new CharTok(c)), Map.empty)
    def satisfy(f: Char => Boolean): Parsley[Char] = new Parsley(Buffer(new Satisfies(f)), Map.empty)
    def string(s: String): Parsley[String] = new Parsley(Buffer(new StringTok(s)), Map.empty)
    @inline
    def choose[A](b: Parsley[Boolean], p: Parsley[A], q: Parsley[A]): Parsley[A] =
    {
        b.flatMap(b => if (b) p else q)
    }
    @inline
    def guard[A](p: Parsley[A], pred: A => Boolean, msg: String): Parsley[A] =
    {
        p.flatMap(x => if (pred(x)) pure(x) else fail(msg))
    }
    @inline
    def guard[A](p: Parsley[A], pred: A => Boolean, msggen: A => String): Parsley[A] =
    {
        p.flatMap(x => if (pred(x)) pure(x) else fail(pure(x), msggen))
    }
    
    def many[A](p: Parsley[A]): Parsley[List[A]] =
    {
        new Parsley(new InputCheck(p.instrs.size+1) +: p.instrs :+ new Many[A](-p.instrs.size), p.subs)
    }

    def skipMany[A](p: Parsley[A]): Parsley[Unit] =
    {
        new Parsley(new InputCheck(p.instrs.size+1) +: p.instrs :+ new SkipMany(-p.instrs.size) :+ Push(()), p.subs)
    }

    def some[A](p: Parsley[A]): Parsley[List[A]] = p <::> many(p)

    var knotScope: Set[String] = Set.empty
    def reset(): Unit =
    {
        knotScope = Set.empty
    }
    def knot[A](name: String, p_ : =>Parsley[A]): Parsley[A] =
    {
        lazy val p = p_
        if (knotScope.contains(name)) new Parsley(Buffer(new Call(name)), Map.empty)
        else
        {
            knotScope += name
            // Perform inline expansion optimisation, reduce to minimum knot-tie
            // FIXME: This does not retrofix any labels!
            val instrs = p.instrs.flatMap
            {
                //case Call(name_) if name != name_ && p.subs.contains(name_) => p.subs(name_)
                case instr => Vector(instr)
            }
            new Parsley(Buffer(new Call(name)), p.subs + (name -> instrs))
        }
    }

    implicit class Knot[A](name: String)
    {
        def <%>(p: =>Parsley[A]): Parsley[A] = knot(name, p)
    }
    
    implicit class Mapper[A, B](f: A => B)
    {
        def <#>(p: Parsley[A]): Parsley[B] = p.map(f)
    }

    //FIXME DO NOT USE
    /*def optimise[A](p: Parsley[A]): Parsley[A] =
    {
        val instrs = p.instrs
        val subs = p.subs
        @tailrec
        // This might be very slow, it might be best to convert to vectors before we remove each element?
        def process(instrs: Buffer[Instruction],
                    labels: Map[Int, Int] = Map.empty,
                    processed: Buffer[Instruction] = Buffer.empty): Buffer[Instruction] = instrs match
        {
            case instrs :+ Label(x) =>
                val idx = instrs.size - x
                process(instrs, labels + (x -> idx), processed)
            case instrs :+ JumpGood(x) => process(instrs, labels, processed :+ JumpGood(labels(x)))
            case instrs :+ InputCheck(handler) => process(instrs, labels, processed :+ InputCheck(labels(handler)))
            case instrs :+ TryBegin(handler) => process(instrs, labels, processed :+ TryBegin(labels(handler)))
            // This peephole is currently disabled, until we can retroactively repair jumps from peephole
            case instrs :+ Pop :+ Push(x) => process(instrs, labels, processed :+ Exchange(x))
            case instrs :+ instr => process(instrs, labels, processed :+ instr)
            case Buffer() => processed.reverse
        }
        new Parsley(process(instrs), subs.mapValues(process(_)))
    }*/

    def inf: Parsley[Int] = "inf" <%> inf.map[Int](_+1).map[Int](_+2)
    def expr: Parsley[Int] = "expr" <%> (pure[Int=>Int=>Int](x => y => x + y) <*> pure[Int](10) <*> expr)
    def monad: Parsley[Int] = for (x <- pure[Int](10); y <- pure[Int](20)) yield x + y
    def foo: Parsley[Int] = "foo" <%> (bar <* pure(20))
    def bar: Parsley[Int] = "bar" <%> (foo *> pure(10))
    def sepEndBy1[A, B](p: Parsley[A], sep: Parsley[B]): Parsley[List[A]] = s"sepEndBy1" <%> (p <::> ((sep >> sepEndBy(p, sep)) </> Nil))
    def sepEndBy[A, B](p: Parsley[A], sep: Parsley[B]): Parsley[List[A]] = s"sepEndBy" <%> (sepEndBy1(p, sep) </> Nil)
    def repeat[A](n: Int, p: Parsley[A]): Parsley[List[A]] =
        if (n > 0) p <::> repeat(n-1, p)
        else pure(Nil)

    def main(args: Array[String]): Unit =
    {
        println(pure[Int=>Int=>Int](x => y => x + y) <*> pure(10) <*> pure(20))
        reset()
        println(inf)
        reset()
        println(expr)
        reset()
        println(monad)
        reset()
        println(foo)
        reset()
        println(many(pure[Int](10)))
        reset()
        println(sepEndBy('x', 'a'))
        reset()
        println(repeat(10, pure[Unit](())))
        reset()
        println(((x: Int) => x * 2) <#> (((x: Char) => x.toInt) <#> '0'))
        reset()
    }
}
