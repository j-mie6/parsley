package parsley

import parsley.Parsley._

import scala.collection.mutable
import language.existentials
import scala.annotation.tailrec

// TODO Investigate effect of :+= instead of :+ for the buffers
final class Parsley[+A] private [Parsley] (
    // The instructions that shall be executed by this parser
    private [Parsley] val instrs: mutable.Buffer[Instr],
    // The subroutines currently collected by the compilation
    private [Parsley] val subs: Map[String, mutable.Buffer[Instr]])
{
    @inline def unsafe() { safe = false }

    // External Core API
    def flatMap[B](f: A => Parsley[B]): Parsley[B] = instrs.last match
    {
        // return x >>= f == f x
        case Push(x: A @unchecked) if safe => new Parsley(instrs.init ++ f(x).instrs, subs)
        case _ => new Parsley(instrs :+ new DynSub[A](x => f(x).instrs.toArray), subs)
    }
    def map[B](f: A => B): Parsley[B] = instrs.last match
    {
        // Pure application can be resolved at compile-time
        case Push(x: A @unchecked) if safe => new Parsley(instrs.init :+ new Push(f(x)), subs)
        // p.map(f).map(g) = p.map(g . f) (functor law)
        case Perform(g) => new Parsley(instrs.init :+ new Perform(g.asInstanceOf[Function[Any, A]].andThen(f)), subs)
        case _ => new Parsley(instrs :+ new Perform(f), subs)
    }
    def *>[B](p: Parsley[B]): Parsley[B] = instrs.last match
    {
        // pure x *> p == p (consequence of applicative and functor laws)
        case _: Push[_] => new Parsley(instrs.init ++ p.instrs, subs ++ p.subs)
        case _ => new Parsley((instrs :+ Pop) ++ p.instrs, subs ++ p.subs)
    }
    def <*[B](p: Parsley[B]): Parsley[A] = p.instrs.last match
    {
        // p <* pure x == p (consequence of applicative and functor laws)
        case _: Push[_] => new Parsley(instrs ++ p.instrs.init, subs ++ p.subs)
        case _ => new Parsley(instrs ++ p.instrs :+ Pop, subs ++ p.subs)
    }
    def #>[B](x: B): Parsley[B] = instrs.last match
    {
        // pure x #> y == pure y (consequence of applicative and functor laws)
        case _: Push[_] => new Parsley(instrs.init :+ new Push(x), subs)
        case _ => new Parsley(instrs :+ Pop :+ new Push(x), subs)
    }
    def <*>:[B](p: Parsley[A => B]): Parsley[B] = p.instrs.last match
    {
        // pure(f) <*> p == f <#> p (consequence of applicative laws)
        case Push(f) => instrs.last match
        {
            // f <#> pure x == pure (f x) (applicative law)
            case Push(x) if safe => new Parsley(p.instrs.init ++ instrs.init :+ new Push(f.asInstanceOf[Function[Any, B]](x)), p.subs ++ subs)
            // p.map(g).map(f) == p.map(f . g) (functor law)
            case Perform(g) =>
                new Parsley(p.instrs.init ++ instrs.init :+
                            new Perform(g.andThen(f.asInstanceOf[Function[Any, B]])), p.subs ++ subs)
            case _ => new Parsley(p.instrs.init ++ instrs :+ new Perform(f.asInstanceOf[Function[A, B]]), p.subs ++ subs)
        }
        case Perform(f: Function[Any, Any=>Any] @unchecked) => instrs.last match
        {
            // fusion law: (f <$> x) <*> pure y == (($y) . f) <$> x
            case Push(y) => new Parsley(p.instrs.init ++ instrs.init :+ new Perform[Any, Any](x => f(x)(y)), p.subs ++ subs)
            case _ => new Parsley(p.instrs ++ instrs :+ Apply, p.subs ++ subs)
        }
        case _ => instrs.last match
        {
            // interchange law: u <*> pure y = ($y) <$> u
            case Push(x) => new Parsley(p.instrs ++ instrs.init :+ new Perform[Any => B, B](f => f(x)), p.subs ++ subs)
            case _ => new Parsley(p.instrs ++ instrs :+ Apply, p.subs ++ subs)
        }
    }
    def <*>[B, C](p: Parsley[B])(implicit ev: A => (B => C)): Parsley[C] = instrs.last match
    {
        // pure(f) <*> p == f <#> p (consequence of applicative laws)
        case Push(f) => p.instrs.last match
        {
            // f <#> pure x == pure (f x) (applicative law)
            case Push(x) if safe => new Parsley(instrs.init ++ p.instrs.init :+ new Push(f.asInstanceOf[Function[Any, C]](x)), subs ++ p.subs)
            // p.map(g).map(f) == p.map(f . g) (functor law)
            case Perform(g) =>
                new Parsley(instrs.init ++ p.instrs.init :+
                    new Perform(g.andThen(f.asInstanceOf[Function[Any, C]])), subs ++ p.subs)
            case _ => new Parsley(instrs.init ++ p.instrs :+ new Perform[B, C](f.asInstanceOf[Function[B, C]]), subs ++ p.subs)
        }
        case Perform(f: Function[Any, Any=>Any] @unchecked) => p.instrs.last match
        {
            // fusion law: (f <$> x) <*> pure y == (($y) . f) <$> x
            case Push(y) => new Parsley(instrs.init ++ p.instrs.init :+ new Perform[Any, Any](x => f(x)(y)), subs ++ p.subs)
            case _ => new Parsley(instrs ++ p.instrs :+ Apply, subs ++ p.subs)
        }
        case _ => p.instrs.last match
        {
            // interchange law: u <*> pure y == ($y) <$> u
            case Push(x) => new Parsley(instrs ++ p.instrs.init :+ new Perform[Any => C, C](f => f(x)), subs ++ p.subs)
            case _ => new Parsley(instrs ++ p.instrs :+ Apply, subs ++ p.subs)
        }
    }
    def <|>[A_ >: A](q: Parsley[A_]): Parsley[A_] = instrs match
    {
        // pure results always succeed
        case mutable.Buffer(Push(_)) => new Parsley[A_](instrs, subs ++ q.subs)
        // empty <|> q == q (alternative law)
        case mutable.Buffer(_: Fail) => q
        case _ => q.instrs match
        {
            // p <|> empty = p (alternative law)
            case mutable.Buffer(_: Fail) => this
            // p <|> p == p (this needs refinement to be label invariant, we want structure
            case instrs_ if instrs == instrs_ => this
            // I imagine there is space for optimisation of common postfix and prefixes in choice
            // this would allow for further optimisations with surrounding integration
            // does it imply that there is a try scope wrapping p?
            // NOTE: Prefix optimisation appears to be correct i.e.
            //      (x *> y) <|> (x *> z) === x *> (y <|> z) without need of try
            // NOTE: Postfix optimisation is also correct
            //      (y *> x) <|> (z *> x) == (y <|> z) *> x, noting that y and z are necessarily impure but this always holds
            case _ =>
                val handler = fresh()
                val skip = fresh()
                new Parsley[A_]((new InputCheck(handler) +: instrs :+ Label(handler) :+ new JumpGood(skip)) ++ q.instrs :+ Label(skip), subs ++ q.subs)
        }
    }

    // Composite/Alias Combinators
    @inline def <**>[B](f: Parsley[A => B]): Parsley[B] = lift2[A, A=>B, B](x => f => f(x), this, f)
    @inline def <#>[B](f: A => B): Parsley[B] = map(f)
    @inline def <#>:[B](f: A => B): Parsley[B] = map(f)
    @inline def >>=[B](f: A => Parsley[B]): Parsley[B] = flatMap(f)
    @inline def >>[B](p: Parsley[B]): Parsley[B] = this *> p
    @inline def </>[A_ >: A](x: A_): Parsley[A_] = this <|> pure(x)
    @inline def <\>[A_ >: A](q: Parsley[A_]): Parsley[A_] = tryParse(this) <|> q

    // Intrinsics
    // A note about intrinsics - by their very definition we can't optimise *to* them, so we need to optimise *around* them
    @inline def <::>[A_ >: A](ps: Parsley[List[A_]]): Parsley[List[A_]] = new Parsley(instrs ++ ps.instrs :+ Cons, subs ++ ps.subs)
    @inline def <|?>[B](p: Parsley[B], q: Parsley[B])(implicit ev: Parsley[A] => Parsley[Boolean]): Parsley[B] = choose(this, p, q)
    @inline def >?>(pred: A => Boolean, msg: String): Parsley[A] = guard(this, pred, msg)
    @inline def >?>(pred: A => Boolean, msggen: A => String): Parsley[A] = guard(this, pred, msggen)

    // Internals
    private [this] var safe = true
    private [parsley] lazy val instrArray: Array[Instr] = {delabel(instrs); instrs.toArray}
    private [parsley] lazy val subsMap: Map[String, Array[Instr]] = subs.map{ case (k, v) => k -> {delabel(v); v.toArray} }
    private [this] lazy val prettyInstrs: String = instrArray.mkString("; ")
    private [this] lazy val prettySubs: String =
    {
        val s = new StringBuilder()
        for ((k, v) <- subsMap)
        {
            s ++= s"def ($k) {"
            s ++= v.mkString("; ")
            s ++= "}, "
        }
        if (subsMap.isEmpty) "no functions" else s.dropRight(2).mkString
    }
    override def toString: String = s"($prettyInstrs, $prettySubs)"
}

object Parsley
{
    def pure[A](a: A): Parsley[A] = new Parsley[A](mutable.Buffer(new Push(a)), Map.empty)
    def fail[A](msg: String): Parsley[A] = new Parsley[A](mutable.Buffer(new Fail(msg)), Map.empty)
    def fail[A](msggen: Parsley[A], finaliser: A => String): Parsley[A] =  new Parsley[A](msggen.instrs :+ new FastFail(finaliser), msggen.subs)
    def empty[A]: Parsley[A] = fail("unknown error")
    def tryParse[A](p: Parsley[A]): Parsley[A] =
    {
        val handler = fresh()
        new Parsley(new PushHandler(handler) +: p.instrs :+ Label(handler) :+ Try, p.subs)
    }
    def lookAhead[A](p: Parsley[A]): Parsley[A] =
    {
        val handler = fresh()
        new Parsley(new PushHandler(handler) +: p.instrs :+ Label(handler) :+ Look, p.subs)
    }
    @inline def lift2[A, B, C](f: A => B => C, p: Parsley[A], q: Parsley[B]): Parsley[C] = p.map(f) <*> q
    @inline def lift2_[A, B, C](f: (A, B) => C, p: Parsley[A], q: Parsley[B]): Parsley[C] = lift2((x: A) => (y: B) => f(x, y), p, q)
    def char(c: Char): Parsley[Char] = new Parsley(mutable.Buffer(new CharTok(c)), Map.empty)
    def satisfy(f: Char => Boolean): Parsley[Char] = new Parsley(mutable.Buffer(new Satisfies(f)), Map.empty)
    def string(s: String): Parsley[String] = new Parsley(mutable.Buffer(new StringTok(s)), Map.empty)
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
        val handler = fresh()
        val back = fresh()
        new Parsley(new InputCheck(handler) +: Label(back) +: p.instrs :+ Label(handler) :+ new Many[A](back), p.subs)
    }
    @tailrec def manyN[A](p: Parsley[A], n: Int)(acc: Parsley[List[A]] = many(p)): Parsley[List[A]] =
    {
        if (n > 0) manyN(p, n-1)(p <::> acc)
        else acc
    }
    @inline def some[A](p: Parsley[A]): Parsley[List[A]] = manyN(p, 1)()

    def skipMany[A](p: Parsley[A]): Parsley[Unit] =
    {
        val handler = fresh()
        val back = fresh()
        new Parsley(new InputCheck(handler) +: Label(back) +: p.instrs :+ Label(handler) :+ new SkipMany(back) :+ new Push(()), p.subs)
    }
    @tailrec def skipManyN[A](p: Parsley[A], n: Int)(acc: Parsley[Unit] = skipMany(p)): Parsley[Unit] =
    {
        if (n > 0) skipManyN(p, n-1)(p *> acc)
        else acc
    }
    @inline def skipSome[A](p: Parsley[A]): Parsley[Unit] = skipManyN(p, 1)()

    private [this] var knotScope: Set[String] = Set.empty
    private [this] var curLabel: Int = 0
    private [Parsley] def fresh(): Int =
    {
        val label = curLabel
        curLabel += 1
        label
    }
    def reset()
    {
        knotScope = Set.empty
    }
    def knot[A](name: String, p_ : =>Parsley[A]): Parsley[A] =
    {
        lazy val p = p_
        if (knotScope.contains(name)) new Parsley(mutable.Buffer(new Call(name)), Map.empty)
        else
        {
            knotScope += name
            // Perform inline expansion optimisation, reduce to minimum knot-tie
            val instrs: mutable.Buffer[Instr] = p.instrs.flatMap
            {
                case Call(name_) if name != name_ && p.subs.contains(name_) => p.subs(name_)
                case instr => List(instr)
            }
            new Parsley(mutable.Buffer(new Call(name)), p.subs + (name -> instrs))
        }
    }

    implicit class Knot[A](name: String)
    {
        @inline def <%>(p: =>Parsley[A]): Parsley[A] = knot(name, p)
    }
    
    implicit class Mapper[A, B](f: A => B)
    {
        @inline def <#>(p: Parsley[A]): Parsley[B] = p.map(f)
    }

    private [Parsley] def delabel(instrs: mutable.Buffer[Instr])
    {
        val n = instrs.size
        @tailrec def find(i: Int = 0, offset: Int = 0, labels: Map[Int, Int] = Map.empty): Map[Int, Int] =
        {
            if (i < n) instrs(i) match
            {
                case Label(l) => find(i+1, offset-1, labels + (l -> (i+offset)))
                case _        => find(i+1, offset, labels)
            }
            else labels
        }
        val labels = find()
        @tailrec def replaceAndRemove(i: Int = n-1)
        {
            if (i >= 0)
            {
                instrs(i) match
                {
                    case _: Label => instrs.remove(i)
                    case PushHandler(l) => instrs.update(i, new PushHandler(labels(l)))
                    case InputCheck(l) => instrs.update(i, new InputCheck(labels(l)))
                    case JumpGood(l) => instrs.update(i, new JumpGood(labels(l)))
                    case Many(l) => instrs.update(i, new Many(labels(l)))
                    case SkipMany(l) => instrs.update(i, new SkipMany(labels(l)))
                    case Chainl(l) => instrs.update(i, new Chainl(labels(l)))
                    case _ =>
                }
                replaceAndRemove(i-1)
            }
        }
        replaceAndRemove()
    }

    //TODO This needs to be reinstated
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

    @inline def chainl1[A](p: Parsley[A], op: Parsley[A => A => A]): Parsley[A] = chainl1_(p, op.map(flip[A, A, A]))
    @inline def chainl1_[A](p : Parsley[A], op: Parsley[A => A => A]): Parsley[A] = chainPost(p, op <*> p)
    def chainPost[A](p: Parsley[A], op: Parsley[A => A]): Parsley[A] =
    {
        lift2((x: A) => (xs: List[A=>A]) => xs.foldLeft(x)((y, f) => f(y)), p, many(op))
        /*val handler = fresh()
        val back = fresh()
        new Parsley((p.instrs :+ InputCheck(handler) :+ Label(back)) ++ op.instrs :+ Label(handler) :+ new Chainl[A](back), p.subs ++ op.subs)*/
    }
    @inline private [parsley] def flip[A, B, C](f: A => B => C)(x: B)(y: A): C = f(y)(x)

    @inline def chainr1[A](p: Parsley[A], op: Parsley[A => A => A]): Parsley[A] =
    {
        //"chain" <%> (p <**> (op.map(flip[A, A, A]) <*> chainr1(p, op) </> identity))
        chainPre(p, tryParse(p <**> op))
    }
    def chainPre[A](p: Parsley[A], op: Parsley[A => A]): Parsley[A] =
    {
        lift2((xs: List[A=>A]) => (x: A) => xs.foldRight(x)((f, y) => f(y)), many(op), p)
    }

    def main(args: Array[String]): Unit =
    {
        println(pure[Int=>Int=>Int](x => y => x + y) <*> pure(10) <*> pure(20))
        reset()
        println(((x: Int) => x * 2) <#> (((x: Char) => x.toInt) <#> '0'))
        reset()
        val atom = 'x' #> 1
        val add = '+' #> ((x: Int) => (y: Int) => x + y)
        val mul = '*' #> ((x: Int) => (y: Int) => x * y)
        val pow = '^' #> ((x: Int) => (y: Int) => math.pow(x, y).toInt)
        println(chainl1(chainl1(chainl1(atom, pow), mul), add))
    }
}
