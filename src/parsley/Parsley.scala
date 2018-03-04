package parsley

import parsley.Parsley._

import scala.collection.mutable
import language.existentials
import scala.annotation.tailrec

// TODO Investigate effect of :+= instead of :+ for the buffers
/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the constructor itself is private.
  *
  * @author Jamie Willis
  * @version 1
  */
class Parsley[+A] private [Parsley] (
    // The instructions that shall be executed by this parser
    private [Parsley] final val instrs: mutable.Buffer[Instr],
    // The subroutines currently collected by the compilation
    private [Parsley] final val subs: Map[String, mutable.Buffer[Instr]])
{
    /**
      * Using this method signifies that the parser it is invoked on is impure and any optimisations which assume purity
      * are disabled.
      */
    @inline final def unsafe(): Unit = safe = false

    // Core API
    /**
      * This is the traditional Monadic binding operator for parsers. When the invokee produces a value, the function
      * `f` is used to produce a new parser that continued the computation.
      *
      * WARNING: There is significant overhead for using flatMap; if possible try to write parsers in an applicative
      * style otherwise try and use the intrinsic parsers provided to replace the flatMap.
      * @param f A function that produces the next parser
      * @return The parser produces from the application of `f` on the result of the last parser
      */
    final def flatMap[B](f: A => Parsley[B]): Parsley[B] = instrs.last match
    {
        // return x >>= f == f x
        case Push(x: A @unchecked) if safe => new Parsley(instrs.init ++ f(x).instrs, subs)
        case _ => new Parsley(instrs :+ new DynSub[A](x => f(x).instrs.toArray), subs)
    }

    /**
      * This is the functorial map operation for parsers. When the invokee produces a value, this value is fed through
      * the function `f`.
      *
      * WARNING: This is subject to aggressive optimisations assuming purity; the compiler is permitted to optimise such
      * that the application of `f` actually only happens once at compile time. In order to preserve the behaviour of
      * impure functions, consider using the `unsafe` method before map; `p.unsafe.map(f)`.
      * @param f The mutator to apply to the result of previous parse
      * @return A new parser which parses the same input as the invokee but mutated by function `f`
      */
    final def map[B](f: A => B): Parsley[B] = instrs.last match
    {
        // Pure application can be resolved at compile-time
        case Push(x: A @unchecked) if safe => new Parsley(instrs.init :+ new Push(f(x)), subs)
        /*FIXME: MOVE*/case CharTok(c) if safe => new Parsley(instrs.init :+ CharTokFastPerform(c, f.asInstanceOf[Function[Any, Any]]), subs)
        /*FIXME: MOVE*/case CharTokFastPerform(c, g) if safe => new Parsley(instrs.init :+ CharTokFastPerform(c, g.andThen(f.asInstanceOf[Function[Any, Any]])), subs)
        // p.map(f).map(g) = p.map(g . f) (functor law)
        case Perform(g) => new Parsley(instrs.init :+ new Perform(g.asInstanceOf[Function[Any, A]].andThen(f)), subs)
        case _ => new Parsley(instrs :+ new Perform(f), subs)
    }

    /**
      * This is the parser that corresponds to a more optimal version of `this.map(_ => x => x) <*> p`. It performs
      * the parse action of both parsers, in order, but discards the result of the invokee.
      * @param p The parser whose result should be returned
      * @return A new parser which first parses the invokee, then `p` and returns the result of `p`
      */
    final def *>[B](p: Parsley[B]): Parsley[B] = instrs.last match
    {
        // pure x *> p == p (consequence of applicative and functor laws)
        case _: Push[_] => new Parsley(instrs.init ++ p.instrs, subs ++ p.subs)
        case _ => new Parsley((instrs :+ Pop) ++ p.instrs, subs ++ p.subs)
    }

    /**
      * This is the parser that corresponds to a more optimal version of `this.map(x => _ => x) <*> p`. It performs
      * the parse action of both parsers, in order, but discards the result of the second parser.
      * @param p The parser who should be executed but then discarded
      * @return A new parser which first parses the invokee, then `p` and returns the result of the invokee
      */
    final def <*[B](p: Parsley[B]): Parsley[A] = p.instrs.last match
    {
        // p <* pure x == p (consequence of applicative and functor laws)
        case _: Push[_] => new Parsley(instrs ++ p.instrs.init, subs ++ p.subs)
        case _ => new Parsley(instrs ++ p.instrs :+ Pop, subs ++ p.subs)
    }

    /**
      * This is the parser that corresponds to a more optimal version of either `this *> pure(x)` or `this.map(_ => x)`.
      * It performs the parse action of the invokee but discards its result and then results the value `x` instead
      * @param x The value to be returned after the execution of the invokee
      * @return A new parser which first parses the invokee, then results `x`
      */
    final def #>[B](x: B): Parsley[B] = instrs.last match
    {
        // pure x #> y == pure y (consequence of applicative and functor laws)
        case _: Push[_] => new Parsley(instrs.init :+ new Push(x), subs)
        case _ => new Parsley(instrs :+ Pop :+ new Push(x), subs)
    }

    /**
      * This is the Applicative application parser. The type of the invokee is `Parsley[A]` which is equivalent to some
      * `Parsley[B => C]`. Assuming this is true then, given a `Parsley[B]`, we can produce a `Parsley[C]` by parsing
      * the invokee to retrieve `f: B => C`, then parse `p` to receive `x: B` then return `f(x): C`.
      *
      * WARNING: `pure(f) <*> p` is subject to the same aggressive optimisations as `map`. When using impure functions
      * the optimiser may decide to cache the result of the function execution, be sure to use `unsafe` in order to
      * prevent these optimisations.
      * @param p A parser of type B, where the invokee is B => C
      * @param ev There must exist some evidence to prove that A == (B => C), this is implicit and can be ignored
      * @return A new parser which parses the invokee, then `p` then applies the value returned by `p` to the function
      *         returned by the invokee
      */
    final def <*>[B, C](p: Parsley[B])(implicit ev: A => (B => C)): Parsley[C] = instrs.last match
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
            /*FIXME: MOVE*/case CharTokFastPerform(c, g) if safe => new Parsley(instrs.init ++ p.instrs.init :+ CharTokFastPerform(c, g.andThen(f.asInstanceOf[Function[Any, Any]])), subs)
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

    /*final def <*>:[B](p: Parsley[A => B]): Parsley[B] = p.instrs.last match
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
    }*/

    final def <|>[A_ >: A](q: Parsley[A_]): Parsley[A_] = instrs match
    {
        // pure results always succeed
        case mutable.Buffer(Push(_)) => new Parsley[A_](instrs, subs ++ q.subs)
        // empty <|> q == q (alternative law)
        case mutable.Buffer(e: Empty) if e.expected.isEmpty => q
        case _ => q.instrs match
        {
            // p <|> empty = p (alternative law)
            case mutable.Buffer(e: Empty) if e.expected.isEmpty => this
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
    /**This combinator is defined as `lift2(x => f => f(x), this, f)`. It is pure syntactic sugar.*/
    @inline final def <**>[B](f: Parsley[A => B]): Parsley[B] = lift2[A, A=>B, B](x => f => f(x), this, f)
    /**This combinator is an alias for `map`*/
    @inline final def <#>[B](f: A => B): Parsley[B] = map(f)
    /**This combinator is an alias for `map`, but the function lives on the lhs*/
    @inline final def <#>:[B](f: A => B): Parsley[B] = map(f)
    /**This combinator is an alias for `flatMap`*/
    @inline final def >>=[B](f: A => Parsley[B]): Parsley[B] = flatMap(f)
    @inline final def ?(msg: String): Parsley[A] = label(this, msg)
    /**This combinator is an alias for `*>`*/
    @inline final def >>[B](p: Parsley[B]): Parsley[B] = this *> p
    /**This combinator is defined as `this <|> pure(x)`. It is pure syntactic sugar.*/
    @inline final def </>[A_ >: A](x: A_): Parsley[A_] = this <|> pure(x)
    /**This combinator is defined as `tryParse(this) <|> q`. It is pure syntactic sugar.*/
    @inline final def <\>[A_ >: A](q: Parsley[A_]): Parsley[A_] = tryParse(this) <|> q

    // Intrinsics
    // A note about intrinsics - by their very definition we can't optimise *to* them, so we need to optimise *around* them
    /**This parser corresponds to lift2_(_::_, this ps) but is far more optimal. It should be preferred to the equivalent*/
    @inline final def <::>[A_ >: A](ps: Parsley[List[A_]]): Parsley[List[A_]] = new Parsley(instrs ++ ps.instrs :+ Cons, subs ++ ps.subs)
    @inline final def <~>[A_ >: A, B](p: Parsley[B]): Parsley[(A_, B)] = lift2((x: A_) => (y: B) => (x, y), this, p)
    @inline final def <|?>[B](p: Parsley[B], q: Parsley[B])(implicit ev: Parsley[A] => Parsley[Boolean]): Parsley[B] = choose(this, p, q)
    @inline final def withFilter(p: A => Boolean): Parsley[A] = this >>= (x => if (p(x)) pure(x) else empty)
    @inline final def filter(p: A => Boolean): Parsley[A] = withFilter(p)
    @inline final def guard(pred: A => Boolean, msg: String): Parsley[A] = flatMap(x => if (pred(x)) pure(x) else fail(msg))
    @inline final def guard(pred: A => Boolean, msggen: A => String): Parsley[A] = flatMap(x => if (pred(x)) pure(x) else fail(pure(x), msggen))
    @inline final def >?>(pred: A => Boolean, msg: String): Parsley[A] = guard(pred, msg)
    @inline final def >?>(pred: A => Boolean, msggen: A => String): Parsley[A] = guard(pred, msggen)

    // Internals
    private [this] final var safe = true
    private [parsley] final lazy val instrArray: Array[Instr] = delabel(instrs)
    private [parsley] final lazy val subsMap: Map[String, Array[Instr]] = subs.map{ case (k, v) => k -> delabel(v) }
    private [this] final lazy val prettyInstrs: String = instrArray.mkString("; ")
    private [this] final lazy val prettySubs: String =
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
    override final def toString: String = s"($prettyInstrs, $prettySubs)"
}

object Parsley
{
    def pure[A](a: A): Parsley[A] = new Parsley[A](mutable.Buffer(new Push(a)), Map.empty)
    def fail[A](msg: String): Parsley[A] = new Parsley[A](mutable.Buffer(new Fail(msg)), Map.empty)
    def fail[A](msggen: Parsley[A], finaliser: A => String): Parsley[A] = new Parsley[A](msggen.instrs :+ new FastFail(finaliser), msggen.subs)
    def empty[A]: Parsley[A] = new Parsley[A](mutable.Buffer(new Empty), Map.empty)
    def unexpected[A](msg: String): Parsley[A] = new Parsley[A](mutable.Buffer(new Unexpected(msg)), Map.empty)
    def label[A](p: Parsley[A], msg: String): Parsley[A] = new Parsley[A](p.instrs.map
    {
        case e: ExpectingInstr =>
            val e_ = e.copy
            e_.expected = msg
            e_
        case i => i
    }, p.subs)
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
    def notFollowedBy[A](p: Parsley[A]): Parsley[Unit] = (tryParse(p) >>= (c => unexpected("\"" + c.toString + "\""))) </> Unit
    @inline def lift2[A, B, C](f: A => B => C, p: Parsley[A], q: Parsley[B]): Parsley[C] = p.map(f) <*> q
    @inline def lift2_[A, B, C](f: (A, B) => C, p: Parsley[A], q: Parsley[B]): Parsley[C] = lift2((x: A) => (y: B) => f(x, y), p, q)
    def char(c: Char): Parsley[Char] = new Parsley(mutable.Buffer(CharTok(c)), Map.empty)
    def satisfy(f: Char => Boolean): Parsley[Char] = new Parsley(mutable.Buffer(new Satisfies(f)), Map.empty)
    def string(s: String): Parsley[String] = new Parsley(mutable.Buffer(new StringTok(s)), Map.empty)
    def anyChar: Parsley[Char] = satisfy(_ => true)
    def eof: Parsley[Unit] = notFollowedBy(anyChar) ? "end of input"
    @inline
    def choose[A](b: Parsley[Boolean], p: Parsley[A], q: Parsley[A]): Parsley[A] =
    {
        b.flatMap(b => if (b) p else q)
    }
    
    def many[A](p: Parsley[A]): Parsley[List[A]] =
    {
        val handler = fresh()
        val back = fresh()
        new Parsley(new InputCheck(handler) +: Label(back) +: p.instrs :+ Label(handler) :+ new Many(back), p.subs)
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
    def reset(): Unit = knotScope = Set.empty
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

    // Intrinsics
    case class Cons[+A](p: Parsley[A], ps: Parsley[List[A]]) extends Parsley[List[A]](mutable.Buffer.empty, Map.empty)
    case class CharPerform[+A](c: Char, f: Char => A) extends Parsley[A](mutable.Buffer.empty, Map.empty)

    private [Parsley] def delabel(instrs_ : mutable.Buffer[Instr]): Array[Instr] =
    {
        val instrs = instrs_.clone()
        type LabelMap = mutable.Map[Int, Vector[Int]]
        val n = instrs.size
        val labels: LabelMap = mutable.Map.empty.withDefaultValue(Vector.empty)
        @tailrec def forwardPass(i: Int = 0, offset: Int = 0): Unit =
        {
            if (i < n) instrs(i) match
            {
                case Label(l) =>
                    labels.update(l, labels(l) :+ (i+offset))
                    forwardPass(i+1, offset-1)
                case instr: FwdJumpInstr =>
                    if (instr.lidx != -1) instrs.update(i, instr.copy(lidx = labels(instr.label).size))
                    else instr.lidx = labels(instr.label).size
                    forwardPass(i+1, offset)
                case Many(l) =>
                    instrs.update(i, new Many(labels(l).last))
                    forwardPass(i+1, offset)
                case SkipMany(l) =>
                    instrs.update(i, new SkipMany(labels(l).last))
                    forwardPass(i+1, offset)
                case Chainl(l) =>
                    instrs.update(i, new Chainl(labels(l).last))
                    forwardPass(i+1, offset)
                case _ => forwardPass(i+1, offset)
            }
        }
        @tailrec def backwardPass(i: Int = n-1): Unit =
        {
            if (i >= 0)
            {
                instrs(i) match
                {
                    case _: Label => instrs.remove(i)
                    case instr@PushHandler(l) => instrs.update(i, new PushHandler(labels(l)(instr.lidx)))
                    case instr@InputCheck(l) => instrs.update(i, new InputCheck(labels(l)(instr.lidx)))
                    case instr@JumpGood(l) => instrs.update(i, new JumpGood(labels(l)(instr.lidx)))
                    case _ =>
                }
                backwardPass(i-1)
            }
        }
        forwardPass()
        backwardPass()
        instrs.toArray
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
        val handler = fresh()
        val back = fresh()
        new Parsley((p.instrs :+ new InputCheck(handler) :+ Label(back)) ++ op.instrs :+ Label(handler) :+ new Chainl(back), p.subs ++ op.subs)
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
        val atom = '1'.map(_.toInt)
        val add = '+' #> ((x: Int) => (y: Int) => x + y)
        val mul = '*' #> ((x: Int) => (y: Int) => x * y)
        val pow = '^' #> ((x: Int) => (y: Int) => math.pow(x, y).toInt)
        println(chainl1(atom, pow))
        println(chainl1(chainl1(atom, pow), mul))
        println(chainl1(chainl1(chainl1(atom, pow), mul), add))
        lazy val p: Parsley[List[String]] = "p" <%> ("correct error message" <::> (p </> Nil))
        println(runParser(p ? "nothing but this :)", ""))
        println(runParser(fail("hi"), "b"))
        println(runParser('a' <|> (fail("oops") ? "hi"), "b"))
        println(runParser(unexpected("bee"), "b"))
        println(runParser('a' <|> unexpected("bee") ? "something less cute", "b"))
        println(runParser(empty, "b"))
        println(runParser(empty ? "something, at least", "b"))
        println(runParser('a' <|> empty ? "something, at least", "b"))
        println(runParser(eof, "a"))
    }
}

object DeepEmbedding
{
    import DeepEmbedding.Parsley._
    object ExecutionTime
    
    // User API
    implicit final class LazyParsley[A](p: =>Parsley[A])
    {
        def map[B](f: A => B): Parsley[B] = pure(f) <*> p
        def flatMap[B](f: A => Parsley[B]): Parsley[B] = new Bind(p, f)
        def >>=[B](f: A => Parsley[B]): Parsley[B] = flatMap(f)
        def <*>[B, C](q: =>Parsley[B])(implicit ev: Parsley[A] => Parsley[B => C]): Parsley[C] = new App(p, q)
        def <**>[B](pf: =>Parsley[A => B]): Parsley[B] = lift2[A, A=>B, B](x => f => f(x), p, pf)
        def <|>[B >: A](q: =>Parsley[B]): Parsley[B] = new Or(p, q)
        def </>[B >: A](x: B): Parsley[B] = this <|> pure(x)
        def orElse[B >: A](q: =>Parsley[B]): Parsley[B] = this <|> q
        def getOrElse[B >: A](x: B): Parsley[B] = p </> x
        def *>[A_ >: A, B](q: =>Parsley[B]) = new Then[A_, B](p, q)
        def <*[B](q: =>Parsley[B]) = new Prev(p, q)
        def #>[B](x: B): Parsley[B] = this *> pure(x)
        def <::>[B >: A](ps: =>Parsley[List[B]]): Parsley[List[B]] = new Cons(p, ps)
        def <~>[A_ >: A, B](q: =>Parsley[B]): Parsley[(A_, B)] = lift2((x: A_) => (y: B) => (x, y), p, q)
    }
    implicit final class LazyAppParsley[A, B](pf: =>Parsley[A => B])
    {
        def <*>(px: =>Parsley[A]): Parsley[B] = new App(pf, px)
    }
    implicit final class LazyFlattenParsley[A](p: =>Parsley[Parsley[A]])
    {
        def flatten: Parsley[A] = p.flatMap(identity[Parsley[A]])
    }
    implicit final class LazyMapParsley[A, B](f: A => B)
    {
        def <#>(p: =>Parsley[A]): Parsley[B] = p.map(f)
    }
    object Parsley
    {
        def pure[A](x: A): Parsley[A] = new Pure(x)
        def char(c: Char): Parsley[Char] = new CharTok(c)
        def satisfies(f: Char => Boolean): Parsley[Char] = new Satisfies(f)
        def string(s: String): Parsley[String] = new StringTok(s)
        def lift2[A, B, C](f: A => B => C, p: =>Parsley[A], q: =>Parsley[B]): Parsley[C] = p.map(f) <*> q
        def join[A](p: =>Parsley[Parsley[A]]) = p.flatten
        def attempt[A](p: =>Parsley[A]): Parsley[A] = new Attempt(p)
        def tryParse[A](p: =>Parsley[A]): Parsley[A] = attempt(p)
        def lookAhead[A](p: =>Parsley[A]): Parsley[A] = new Look(p)
    }
    
    // Internals
    private class LabelCounter
    {
        private [this] var current = 0
        def fresh(): Int =
        {
            val next = current
            current += 1
            next
        }
    }
    abstract class Parsley[+A]
    {
        protected type InstrBuffer = mutable.ListBuffer[Instr]
        def unsafe(): Unit = safe = false
        
        // Internals
        private [this] lazy val _instrs: Array[Instr] = instrsSafe.toArray
        // TODO: Implement optimisation caching, with fixpoint safety!
        //private [this] var _optimised: UnsafeOption[Parsley[A]] = null
        //private [this] var _seenLastOptimised: UnsafeOption[Set[Parsley[_]]] = null
        private [DeepEmbedding] def optimised(implicit seen: Set[Parsley[_]]): Parsley[A] = optimise(seen + this)
        /*{
            val seen_ = if (_optimised != null) seen ++ _seenLastOptimised
            else
            {
                (_optimised, _seenLastOptimised) = optimise(seen)
            }
            _optimised
        }*/
        protected var safe = true
        private [parsley] def instrs(implicit ev: ExecutionTime.type): Array[Instr] = _instrs
        private [Parsley] def instrsSafe: InstrBuffer = 
        {
            val instrs: InstrBuffer = mutable.ListBuffer.empty
            optimised(Set.empty).codeGen(instrs, new LabelCounter)
            instrs
        }
        private [DeepEmbedding] def fix(implicit seen: Set[Parsley[_]]): Parsley[A] = if (seen.contains(this)) new Fixpoint(this) else this
        
        // Abstracts
        // Optimisation and fixpoint calculation - Bottom-up
        protected def optimise(implicit seen: Set[Parsley[_]]): Parsley[A]
        // Peephole optimisation and code generation - Top-down
        private [DeepEmbedding] def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit
    }
    // Core Embedding
    private [DeepEmbedding] final class Pure[+A](private [Pure] val x: A) extends Parsley[A]
    {
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[A] = this
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = instrs += new parsley.Push(x)
    }
    private [DeepEmbedding] final class App[A, +B](_pf: =>Parsley[A => B], _px: =>Parsley[A]) extends Parsley[B]
    {
        private [App] lazy val pf = _pf
        private [App] lazy val px = _px 
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[B] = 
        {
            val pf_ = pf.fix.optimised
            val px_ = px.fix.optimised
            new App(pf_, px_)
        }
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit =
        {
            pf.codeGen
            px.codeGen
            instrs += parsley.Apply
        }
    }
    private [DeepEmbedding] final class Or[A, B >: A](_p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[B]
    {
        private [Or] lazy val p = _p
        private [Or] lazy val q = _q
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[B] = 
        {
            val p_ = p.fix.optimised
            val q_ = q.fix.optimised
            new Or(p_, q_)
        }
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = 
        {
            val handler = labels.fresh()
            val skip = labels.fresh()
            instrs += new InputCheck(handler)
            p.codeGen
            instrs += new Label(handler)
            instrs += new JumpGood(skip)
            q.codeGen
            instrs += new Label(skip)
        }
    }
    private [DeepEmbedding] final class Bind[A, +B](_p: =>Parsley[A], private [Bind] val f: A => Parsley[B]) extends Parsley[B]
    {
        private [Bind] lazy val p = _p
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[B] = 
        {
            val p_ = p.fix.optimised
            new Bind(p_, f)
        }
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = 
        {
            p.codeGen
            instrs += new parsley.DynSub[A](x => f(x).instrs(ExecutionTime))
        }
    }
    private [DeepEmbedding] final class Satisfies(private [Satisfies] val f: Char => Boolean) extends Parsley[Char]
    {
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[Char] = this
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = instrs += new parsley.Satisfies(f)
    }
    private [DeepEmbedding] final class Then[A, +B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[B]
    {
        private [Then] lazy val p = _p
        private [Then] lazy val q = _q
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[B] =
        {
            val p_ = p.fix.optimised
            val q_ = q.fix.optimised
            new Then(p_, q_)
        }
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = 
        {
            p.codeGen
            instrs += parsley.Pop
            q.codeGen
        }
    }
    private [DeepEmbedding] final class Prev[B, +A](_p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[A]
    {
        private [Prev] lazy val p = _p
        private [Prev] lazy val q = _q
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[A] = 
        {
            val p_ = p.fix.optimised
            val q_ = q.fix.optimised
            new Prev(p_, q_)
        }
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = 
        {
            p.codeGen
            q.codeGen
            instrs += parsley.Pop
        }
    }
    private [DeepEmbedding] final class Attempt[A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Attempt] lazy val p = _p
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[A] = new Attempt(p.fix.optimised)
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit =
        {
            val handler = labels.fresh()
            instrs += new parsley.PushHandler(handler)
            p.codeGen
            instrs += new parsley.Label(handler)
            instrs += parsley.Try
        }
    }
    private [DeepEmbedding] final class Look[A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Look] lazy val p = _p
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[A] = new Look(p.fix.optimised)
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = 
        {
            val handler = labels.fresh()
            instrs += new parsley.PushHandler(handler)
            p.codeGen
            instrs += new parsley.Label(handler)
            instrs += parsley.Look
        }
    }
    private [DeepEmbedding] final class Fixpoint[A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Fixpoint] lazy val p = _p
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[A] = this
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = instrs += new parsley.Call_(p)
    }
    // Intrinsic Embedding
    private [DeepEmbedding] final class CharTok(private [CharTok] val c: Char) extends Parsley[Char]
    {
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[Char] = this
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = instrs += parsley.CharTok(c)
    }
    private [DeepEmbedding] final class StringTok(private [StringTok] val s: String) extends Parsley[String]
    {
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[String] = this
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit = instrs += new parsley.StringTok(s)
    }
    private [DeepEmbedding] final class Cons[A, +B >: A](_p: =>Parsley[A], _ps: =>Parsley[List[B]]) extends Parsley[List[B]]
    {
        private [Cons] lazy val p = _p
        private [Cons] lazy val ps = _ps
        override def optimise(implicit seen: Set[Parsley[_]]): Parsley[List[B]] = 
        {
            val p_ = p.fix.optimised
            val ps_ = ps.fix.optimised
            new Cons(p_, ps_)
        }
        override def codeGen(implicit instrs: InstrBuffer, labels: LabelCounter): Unit =
        {
            p.codeGen
            ps.codeGen
            instrs += parsley.Cons
        }
    }
    
    private [DeepEmbedding] object Pure      { def unapply[A](self: Pure[A]): Option[A] = Some(self.x) }
    private [DeepEmbedding] object App       { def unapply[A, B](self: App[A, B]): Option[(Parsley[A=>B], Parsley[A])] = Some((self.pf, self.px)) }
    private [DeepEmbedding] object Or        { def unapply[A, B >: A](self: Or[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q)) }
    private [DeepEmbedding] object Bind      { def unapply[A, B](self: Bind[A, B]): Option[(Parsley[A], A => Parsley[B])] = Some((self.p, self.f)) }
    private [DeepEmbedding] object Satisfies { def unapply(self: Satisfies): Option[Char => Boolean] = Some(self.f) }
    private [DeepEmbedding] object Then      { def unapply[A, B](self: Then[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q)) }
    private [DeepEmbedding] object Prev      { def unapply[A, B](self: Prev[B, A]): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q)) }
    private [DeepEmbedding] object Attempt   { def unapply[A](self: Attempt[A]): Option[Parsley[A]] = Some(self.p) }
    private [DeepEmbedding] object Look      { def unapply[A](self: Look[A]): Option[Parsley[A]] = Some(self.p) }
    private [DeepEmbedding] object CharTok   { def unapply(self: CharTok): Option[Char] = Some(self.c) }
    private [DeepEmbedding] object StringTok { def unapply(self: StringTok): Option[String] = Some(self.s) }
    private [DeepEmbedding] object Cons      { def unapply[A, B >: A](self: Cons[A, B]): Option[(Parsley[A], Parsley[List[B]])] = Some((self.p, self.ps)) }
    
    def main(args: Array[String]): Unit =
    {
        def many[A](p: Parsley[A]): Parsley[List[A]] =
        {
            lazy val manyp: Parsley[List[A]] = (p <::> manyp) </> Nil
            manyp
        }
        lazy val p: Parsley[Int] = p.map((x: Int) => x+1)
        println(p.instrs(ExecutionTime).mkString("; "))
        println(many(p).instrs(ExecutionTime).mkString("; "))
        val q: Parsley[Char] = char('a') <|> char('b')
        println((q <|> q).instrs(ExecutionTime).mkString("; "))
    }
}
