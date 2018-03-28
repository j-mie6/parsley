package parsley

import parsley.Parsley._

import language.existentials
import scala.annotation.tailrec
    
// User API

object Parsley
{
    @inline private def flip[A, B, C](f: A => B => C)(x: B)(y: A): C = f(y)(x)
    implicit final class LazyParsley[P, +A](p: =>P)(implicit con: P => Parsley[A])
    {
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
        def map[B](f: A => B): Parsley[B] = pure(f) <*> p
        /**This combinator is an alias for `map`*/
        def <#>[B](f: A => B): Parsley[B] = map(f)
        /**
          * This is the traditional Monadic binding operator for parsers. When the invokee produces a value, the function
          * `f` is used to produce a new parser that continued the computation.
          *
          * WARNING: There is significant overhead for using flatMap; if possible try to write parsers in an applicative
          * style otherwise try and use the intrinsic parsers provided to replace the flatMap.
          * @param f A function that produces the next parser
          * @return The parser produces from the application of `f` on the result of the last parser
          */
        def flatMap[B](f: A => Parsley[B]): Parsley[B] = new DeepEmbedding.Bind(p, f)
        /**This combinator is an alias for `flatMap`*/
        def >>=[B](f: A => Parsley[B]): Parsley[B] = flatMap(f)
        /**This combinator is defined as `lift2((x, f) => f(x), this, f)`. It is pure syntactic sugar.*/
        def <**>[B](pf: =>Parsley[A => B]): Parsley[B] = lift2[A, A=>B, B]((x, f) => f(x), p, pf)
        /**
          * This is the traditional Alternative choice operator for parsers. Following the parsec semantics precisely,
          * this combinator first tries to parse the invokee. If this is successful, no further action is taken. If the
          * invokee failed *without* consuming input, then `q` is parsed instead. If the invokee did parse input then the
          * whole parser fails. This is done to prevent space leaks and to give good error messages. If this behaviour
          * is not desired, use the `<\>` combinator (or `attempt(this) <|> q`) to parse `q` regardless of how the
          * invokee failed.
          * @param q The parser to run if the invokee failed without consuming input
          * @return The value produced by the invokee if it was successful, or if it failed without consuming input, the
          *         possible result of parsing q.
          */
        def <|>[B >: A](q: =>Parsley[B]): Parsley[B] = new DeepEmbedding.Or(p, q)
        /**This combinator is defined as `this <|> pure(x)`. It is pure syntactic sugar.*/
        def </>[B >: A](x: B): Parsley[B] = this <|> pure(x)
        /**This combinator is an alias for <|>.*/
        def orElse[B >: A](q: =>Parsley[B]): Parsley[B] = this <|> q
        /**This combinator is an alias for </>.*/
        def getOrElse[B >: A](x: B): Parsley[B] = p </> x
        /**This combinator is defined as `attempt(this) <|> q`. It is pure syntactic sugar.*/
        def <\>[B >: A](q: Parsley[B]): Parsley[B] = attempt(p) <|> q
        /**
          * This is the parser that corresponds to a more optimal version of `this.map(_ => x => x) <*> p`. It performs
          * the parse action of both parsers, in order, but discards the result of the invokee.
          * @param q The parser whose result should be returned
          * @return A new parser which first parses the invokee, then `p` and returns the result of `p`
          */
        def *>[A_ >: A, B](q: =>Parsley[B]) = new DeepEmbedding.Then[A_, B](p, q)
        /**
          * This is the parser that corresponds to a more optimal version of `this.map(x => _ => x) <*> p`. It performs
          * the parse action of both parsers, in order, but discards the result of the second parser.
          * @param q The parser who should be executed but then discarded
          * @return A new parser which first parses the invokee, then `p` and returns the result of the invokee
          */
        def <*[B](q: =>Parsley[B]) = new DeepEmbedding.Prev(p, q)
        /**
          * This is the parser that corresponds to `this *> pure(x)` or a more optimal version of `this.map(_ => x)`.
          * It performs the parse action of the invokee but discards its result and then results the value `x` instead
          * @param x The value to be returned after the execution of the invokee
          * @return A new parser which first parses the invokee, then results `x`
          */
        def #>[B](x: B): Parsley[B] = this *> pure(x)
        /**This combinator is an alias for `*>`*/
        def >>[B](q: Parsley[B]): Parsley[B] = *>(q)
        /**This parser corresponds to `lift2(_::_, this, ps)` but is far more optimal. It should be preferred to the equivalent*/
        def <::>[B >: A](ps: =>Parsley[List[B]]): Parsley[List[B]] = new DeepEmbedding.Cons(p, ps)
        /**This parser corresponds to `lift2((_, _), this, q)`. For now it is sugar, but in future may be more optimal*/
        def <~>[A_ >: A, B](q: =>Parsley[B]): Parsley[(A_, B)] = lift2[A_, B, (A_, B)]((_, _), p, q)
        /** Filter the value of a parser; if the value returned by the parser matches the predicate `pred` then the
          * filter succeeded, otherwise the parser fails with an empty error
          * @param pred The predicate that is tested against the parser result
          * @return The result of the invokee if it passes the predicate
          */
        def filter(pred: A => Boolean): Parsley[A] = flatMap(x => if (pred(x)) pure(x) else empty)
        def withFilter(pred: A => Boolean): Parsley[A] = filter(pred)
        /** Similar to `filter`, except the error message desired is also provided. This allows you to name the message
          * itself.
          * @param pred The predicate that is tested against the parser result
          * @param msg The message used for the error if the input failed the check
          * @return The result of the invokee if it passes the predicate
          */
        def guard(pred: A => Boolean, msg: String): Parsley[A] = flatMap(x => if (pred(x)) pure(x) else fail(msg))
        /** Similar to `filter`, except the error message desired is also provided. This allows you to name the message
          * itself. The message is provided as a generator, which allows the user to avoid otherwise expensive
          * computation.
          * @param pred The predicate that is tested against the parser result
          * @param msggen Generator function for error message, generating a message based on the result of the parser
          * @return The result of the invokee if it passes the predicate
          */
        def guard(pred: A => Boolean, msggen: A => String): Parsley[A] = flatMap(x => if (pred(x)) pure(x) else fail(msggen(x)))
        /**Alias for guard combinator, taking a fixed message.*/
        def >?>(pred: A => Boolean, msg: String): Parsley[A] = guard(pred, msg)
        /**Alias for guard combinator, taking a dynamic message generator.*/
        def >?>(pred: A => Boolean, msggen: A => String): Parsley[A] = guard(pred, msggen)
        /**Sets the expected message for a parser. If the parser fails then `expected msg` will added to the error*/
        def ?(msg: String): Parsley[A] = new DeepEmbedding.ErrorRelabel(p, msg)
        /** Same as `fail`, except allows for a message generated from the result of the failed parser. In essence, this
          * is equivalent to `p >>= (x => fail(msggen(x))` but requires no expensive computations from the use of `>>=`.
          * @param msggen The generator function for error message, creating a message based on the result of invokee
          * @return A parser that fails if it succeeds, with the given generator used to produce the error message
          */
        def !(msggen: A => String): Parsley[Nothing] = new DeepEmbedding.FastFail(p, msggen)
        /** Same as `unexpected`, except allows for a message generated from the result of the failed parser. In essence,
          * this is equivalent to `p >>= (x => unexpected(x))` but requires no expensive computations from the use of
          * `>>=`
          * @param msggen The generator function for error message, creating a message based on the result of invokee
          * @return A parser that fails if it succeeds, with the givne generator used to produce an unexpected message
          */
        def unexpected(msggen: A => String): Parsley[Nothing] = flatMap((x: A) => Parsley.unexpected(msggen(x)))
    }
    implicit final class LazyAppParsley[A, +B](pf: =>Parsley[A => B])
    {
        /**
          * This is the Applicative application parser. The type of the invokee is `Parsley[A => B]`. Then, given a
          * `Parsley[A]`, we can produce a `Parsley[B]` by parsing the invokee to retrieve `f: A => B`, then parse `px`
          * to receive `x: A` then return `f(x): B`.
          *
          * WARNING: `pure(f) <*> p` is subject to the same aggressive optimisations as `map`. When using impure functions
          * the optimiser may decide to cache the result of the function execution, be sure to use `unsafe` in order to
          * prevent these optimisations.
          * @param px A parser of type A, where the invokee is A => B
          * @return A new parser which parses the invokee, then `p` then applies the value returned by `p` to the function
          *         returned by the invokee
          */
        def <*>(px: =>Parsley[A]): Parsley[B] = new DeepEmbedding.App(pf, px)
    }
    implicit final class LazyFlattenParsley[+A](p: =>Parsley[Parsley[A]])
    {
        /**This combinator is an alias for `flatMap(identity)`.*/
        def flatten: Parsley[A] = p >>= identity[Parsley[A]]
    }
    implicit final class LazyMapParsley[A, +B](f: A => B)
    {
        /**This combinator is an alias for `map`*/
        def <#>(p: =>Parsley[A]): Parsley[B] = p.map(f)
    }
    implicit final class LazyChooseParsley[+A](pq: =>(Parsley[A], Parsley[A]))
    {
        lazy val (p, q) = pq
        /**
          * This serves as a lifted if statement (hence its similar look to a C-style ternary expression).
          * If the parser on the lhs of the operator it is true then execution continues with parser `p`, else
          * control passes to parser `q`. `b ?: (p, q)` is equivalent to `b >>= (b => if (b) p else q)` but does not
          * involve any expensive monadic operations. NOTE: due to Scala operator associativity laws, this is a
          * right-associative operator, and must be properly bracketed, technically the invokee is the rhs...
          * @param b The parser that yields the condition value
          * @return The result of either `p` or `q` depending on the return value of the invokee
          */
        def ?:(b: =>Parsley[Boolean]): Parsley[A] = b >>= (b => if (b) p else q)
    }
    
    /** This is the traditional applicative pure function (or monadic return) for parsers. It consumes no input and
      * does not influence the state of the parser, but does return the value provided. Useful to inject pure values
      * into the parsing process.
      * @param x The value to be returned from the parser
      * @return A parser which consumes nothing and returns `x`
      */
    def pure[A](x: A): Parsley[A] = new DeepEmbedding.Pure(x)
    /** Reads a character from the input stream and returns it, else fails if the character is not found at the head
      * of the stream.
      * @param c The character to search for
      * @return `c` if it can be found at the head of the input
      */
    def char(c: Char): Parsley[Char] = new DeepEmbedding.CharTok(c)
    /** Reads a character from the head of the input stream if and only if it satisfies the given predicate. Else it
      * fails without consuming the character.
      * @param f The function to test the character on
      * @return `c` if `f(c)` is true.
      */
    def satisfy(f: Char => Boolean): Parsley[Char] = new DeepEmbedding.Satisfy(f)
    /** Reads a string from the input stream and returns it, else fails if the string is not found at the head
      * of the stream.
      * @param s The string to match against
      * @return `s` if it can be found at the head of the input
      */
    def string(s: String): Parsley[String] = new DeepEmbedding.StringTok(s)

    /** Traditionally, `lift2` is defined as `lift2(f, p, q) = p.map(f) <*> q`. However, `f` is actually uncurried,
      * so it's actually more exactly defined as; read `p` and then read `q` then provide their results to function
      * `f`. This is designed to bring higher performance to any curried operations that are not themselves
      * intrinsic.
      * @param f The function to apply to the results of `p` and `q`
      * @param p The first parser to parse
      * @param q The second parser to parser
      * @return `f(x, y)` where `x` is the result of `p` and `y` is the result of `q`.
      */
    def lift2[A, B, C](f: (A, B) => C, p: =>Parsley[A], q: =>Parsley[B]): Parsley[C] = new DeepEmbedding.Lift(f, p, q)
    /**This function is an alias for `_.flatten`. Provides namesake to Haskell.*/
    def join[A](p: =>Parsley[Parsley[A]]): Parsley[A] = p.flatten
    /** Given a parser `p`, attempts to parse `p`. If the parser fails, then `attempt` ensures that no input was
      * consumed. This allows for backtracking capabilities, disabling the implicit cut semantics offered by `<|>`.
      * @param p The parser to run
      * @return The result of `p`, or if `p` failed ensures the parser state was as it was on entry.
      */
    def attempt[A](p: =>Parsley[A]): Parsley[A] = new DeepEmbedding.Attempt(p)
    def lookAhead[A](p: =>Parsley[A]): Parsley[A] = new DeepEmbedding.Look(p)
    /**Alias for `p ? msg`.**/
    def label[A](p: Parsley[A], msg: String): Parsley[A] = p ? msg
    def fail(msg: String): Parsley[Nothing] = new DeepEmbedding.Fail(msg)
    val empty: Parsley[Nothing] = new DeepEmbedding.Empty
    def unexpected(msg: String): Parsley[Nothing] = new DeepEmbedding.Unexpected(msg)
    val unit: Parsley[Unit] = pure(())
    def many[A](p: =>Parsley[A]): Parsley[List[A]] = new DeepEmbedding.Many(p)
    def skipMany[A](p: =>Parsley[A]): Parsley[Unit] = new DeepEmbedding.Then(new DeepEmbedding.SkipMany(p), new DeepEmbedding.Pure(()))
    def sequence[A](ps: Seq[Parsley[A]]): Parsley[List[A]] = ps.foldRight(pure[List[A]](Nil))(_ <::> _)
    def traverse[A, B](f: A => Parsley[B], xs: Seq[A]): Parsley[List[B]] = sequence(xs.map(f))
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
    def size: Int = current
}
/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is abstract
  *
  * @author Jamie Willis
  * @version 1
  */
sealed abstract class Parsley[+A]
{
    final protected type InstrBuffer = ResizableArray[Instr]
    final protected type T = Any
    final protected type U = Any
    final protected type V = Any
    /**
      * Using this method signifies that the parser it is invoked on is impure and any optimisations which assume purity
      * are disabled.
      */
    final def unsafe(): Unit = safe = false
    final def pretty: String = instrs.mkString("; ")
    
    // Internals
    // TODO: Implement optimisation caching, with fixpoint safety!
    //private [this] var _optimised: UnsafeOption[Parsley[A]] = null
    //private [this] var _seenLastOptimised: UnsafeOption[Set[Parsley[_]]] = null
    final private [parsley] def optimised(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
    {
        // 2 is a magic magic number. It yields the fastest speeds possible for the compilation time?!
        if (depth == 2) (if (seen.isEmpty) this else this.fix).preprocess(p => new Thunk(() => cont(p.optimise)))(seen + this, label, 0)
        else (if (seen.isEmpty) this else this.fix).preprocess(p => cont(p.optimise))(seen + this, label, depth+1)
        /*val seen_ = if (_optimised != null) seen ++ _seenLastOptimised
        else
        {
            (_optimised, _seenLastOptimised) = optimise(seen)
        }
        _optimised*/
    }
    final private [parsley] var safe = true
    final private [parsley] var expected: UnsafeOption[String] = _
    final private [parsley] lazy val instrs: Array[Instr] =
    {
        val instrs: InstrBuffer = new ResizableArray()
        val labels = new LabelCounter
        optimised((p: Parsley[A]) => new Chunk(p))(Set.empty, null, 0).run.codeGen(Terminate)(instrs, labels).run()
        val size = instrs.length - labels.size
        val instrs_ = new Array[Instr](size)
        val instrsOversize = instrs.toArray
        val labelMapping = new Array[Int](labels.size)
        @tailrec def findLabels(instrs: Array[Instr], labels: Array[Int], n: Int, i: Int = 0, off: Int = 0): Unit = if (i + off < n) instrs(i + off) match
        {
            case label: Label => labels(label.i) = i; findLabels(instrs, labels, n, i, off+1)
            case _ => findLabels(instrs, labels, n, i+1, off)
        }
        @tailrec def applyLabels(srcs: Array[Instr], labels: Array[Int], dests: Array[Instr], n: Int, i: Int = 0, off: Int = 0): Unit = if (i < n) srcs(i + off) match
        {
            case _: Label => applyLabels(srcs, labels, dests, n, i, off + 1)
            case jump: JumpInstr => 
                jump.label = labels(jump.label)
                dests(i) = jump
                applyLabels(srcs, labels, dests, n, i + 1, off)
            case instr => 
                dests(i) = instr
                applyLabels(srcs, labels, dests, n, i + 1, off)
        }
        findLabels(instrsOversize, labelMapping, instrs.length)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length)
        instrs_
    }
    final private [parsley] def fix(implicit seen: Set[Parsley[_]]): Parsley[A] = if (seen.contains(this)) new DeepEmbedding.Fixpoint(this) else this
    
    // Abstracts
    // Sub-tree optimisation and fixpoint calculation - Bottom-up
    protected def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]]
    // Optimisation - Bottom-up
    private [parsley] def optimise: Parsley[A]
    // Peephole optimisation and code generation - Top-down
    private [parsley] def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation
}
    
object DeepEmbedding
{
    // Core Embedding
    private [parsley] final class Pure[A](private [Pure] val x: A) extends Parsley[A]
    {
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] = cont(this)
        override def optimise: Parsley[A] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.Push(x)
            cont
        }
    }
    private [parsley] final class App[A, B](_pf: =>Parsley[A => B], _px: =>Parsley[A]) extends Parsley[B]
    {
        private [App] lazy val pf = _pf
        private [App] lazy val px = _px 
        override def preprocess(cont: Parsley[B] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            pf.optimised(pf => px.optimised(px => cont(new App(pf, px))))
        override def optimise: Parsley[B] = (pf, px) match
        {
            // Fusion laws
            case (pf, Pure(x)) if pf.isInstanceOf[Pure[_]] || pf.isInstanceOf[App[_, _]] => pf match
            {
                // first position fusion
                case Pure(f) => new Pure(f(x))
                // second position fusion
                case App(Pure(f: (T => A => B) @unchecked), py: Parsley[T]) => new App(new Pure((y: T) => f(y)(x)), py)
                // third position fusion
                case App(App(Pure(f: (T => U => A => B) @unchecked), py: Parsley[T]), pz: Parsley[U]) => new App (new App(new Pure((y: T) => (z: U) => f(y)(z)(x)), py), pz)
                // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
                case _ => new App(new Pure((f: A => B) => f(x)), pf)
            }
            // functor law: fmap f (fmap g p) == fmap (f . g) p where fmap f p = pure f <*> p from applicative
            case (Pure(f), App(Pure(g: (T => A) @unchecked), p: Parsley[T])) => new App(new Pure(f.compose(g)), p)
            // TODO: functor law with lift2!
            /* RE-ASSOCIATION LAWS */
            case (cont: Cont[_, _], px) => cont match
            {
                // re-association law 1: (q *> pf) <*> px = q *> (pf <*> px)
                case Then(q, pf) => new Then(q, new App(pf, px).optimise)
                // re-association law 3: p *> pure x = pure x <* p
                // consequence of re-association law 3: (pure f <* q) <*> px = p *> (pure f <*> px)
                case Prev(pf: Pure[_], q) => new Then(q, new App(pf, px).optimise)
            }
            case (pf, cont: Cont[_, _]) => cont match
            {
                // re-association law 2: pf <*> (px <* q) = (pf <*> px) <* q
                case Prev(px, q) => new Prev(new App(pf, px).optimise, q)
                // re-association law 3: p *> pure x = pure x <* p
                // consequence of re-association law 3: pf <*> (q *> pure x) = (pf <*> pure x) <* q
                case Then(q, px: Pure[_]) => new Prev(new App(pf, px).optimise, q)
            }
            // consequence of left zero law and monadic definition of <*>, preserving error properties of pf
            case (p, z: MZero) => new Then(p, z)
            // right absorption law: mzero <*> p = mzero 
            case (z: MZero, _) => z
            // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
            case (pf, Pure(x)) => new App(new Pure((f: A => B) => f(x)), pf)
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation = (pf, px) match
        {
            // TODO: We are missing out on optimisation opportunities... push fmaps down into or tree branches?
            // pure f <*> p = f <$> p
            case (Pure(f: (Char => B) @unchecked), ct@CharTok(c)) => instrs += parsley.CharTokFastPerform[Char, B](c, f, ct.expected); cont
            case (Pure(f: (String => B) @unchecked), st@StringTok(s)) => instrs += new parsley.StringTokFastPerform(s, f, st.expected); cont
            case (Pure(f: (A => B)), _) =>
                new Suspended(px.codeGen
                {
                    instrs += new parsley.Perform(f)
                    cont
                })
            case _ =>
                new Suspended(pf.codeGen
                {
                    px.codeGen
                    {
                        instrs += parsley.Apply
                        cont
                    }
                })
        }
    }
    private [parsley] final class Or[A, +B >: A](_p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[B]
    {
        private [Or] lazy val p = _p
        private [Or] lazy val q = _q
        override def preprocess(cont: Parsley[B] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => q.optimised(q => cont(new Or(p, q))))
        override def optimise: Parsley[B] = (p, q) match
        {
            // left catch law: pure x <|> p = pure x
            case (p: Pure[_], _) => p
            // alternative law: empty <|> p = p
            case (e: Empty, q) if e.expected == null => q
            // alternative law: p <|> empty = p
            case (p, e: Empty) if e.expected == null => p
            // associative law: (u <|> v) <|> w = u <|> (v <|> w)
            // TODO add this in when brainfuck benchmark is ready, I want to see how this affects it!
            //case (Or(u: Parsley[T], v: Parsley[A]), w) => new Or(u, new Or[A, B](v, w).optimise).asInstanceOf[Or[_, B]]
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val handler = labels.fresh()
            val skip = labels.fresh()
            instrs += new InputCheck(handler)
            new Suspended(p.codeGen
            {
                instrs += new Label(handler)
                instrs += new JumpGood(skip)
                q.codeGen
                {
                    instrs += new Label(skip)
                    cont
                }
            })
        }
    }
    private [parsley] final class Bind[A, +B](_p: =>Parsley[A], private [Bind] val f: A => Parsley[B]) extends Parsley[B]
    {
        private [Bind] lazy val p = _p
        override def preprocess(cont: Parsley[B] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] = p.optimised(p =>
        {
            val b = new Bind(p, f)
            b.expected = label
            cont(b)
        })
        override def optimise: Parsley[B] = p match
        {
            // TODO: We need to try and identify the fixpoints in the optimised binds, so we can remove the call instructions
            // monad law 1: pure x >>= f = f x
            case Pure(x) => val fp = new Fixpoint(f(x).optimise); fp.expected = expected; fp
            // char/string x = char/string x *> pure x and monad law 1
            case p@CharTok(c) => 
                val fp = new Fixpoint(f(c.asInstanceOf[A]).optimise)
                fp.expected = expected
                new Then(p, fp)
            case p@StringTok(s) => 
                val fp = new Fixpoint(f(s.asInstanceOf[A]).optimise)
                fp.expected = expected
                new Then(p, fp)
            // (q *> p) >>= f = q *> (p >>= f) / (p <* q) >>= f = (p >>= f) <* q
            case Cont(q, p) => 
                val b = new Bind(p, f).optimise
                b.expected = expected
                new Then(q, b)
            // monad law 3: (m >>= g) >>= f = m >>= (\x -> g x >>= f) NOTE: this *could* help if g x ended with a pure, since this would be optimised out!
            case Bind(m: Parsley[T] @unchecked, g: (T => A) @unchecked) => 
                val b = new Bind(m, (x: T) => new Bind(g(x), f).optimise)
                b.expected = expected
                b
            // monadplus law (left zero)
            case z: MZero => z
            // TODO: Consider pushing bind into or tree? may find optimisation opportunities
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            new Suspended(p.codeGen
            {
                instrs += new parsley.DynSub[A](x => f(x).instrs, expected)
                cont
            })
        }
    }
    private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean) extends Parsley[Char]
    {
        override def preprocess(cont: Parsley[Char] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[Char] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.Satisfies(f, expected)
            cont
        }
    }
    private [parsley] abstract class Cont[A, +B] extends Parsley[B]
    {
        def result: Parsley[B]
        def discard: Parsley[A]
        def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): Cont[A, B_]
    }
    private [parsley] final class Then[A, +B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Cont[A, B]
    {
        private [Then] lazy val p = _p
        private [Then] lazy val q = _q
        override def preprocess(cont: Parsley[B] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => q.optimised(q => cont(new Then(p, q))))
        override def optimise: Parsley[B] = (p, q) match
        {
            // pure _ *> p = p
            case (_: Pure[_], q) => q
            /*case (ct@CharTok(c), CharTok(d)) => 
                val st = new StringTok(c.toString + d)
                st.expected = ct.expected
                new Then(st, new Pure(d.asInstanceOf[B]))*/
            // p *> pure _ *> q = p *> q = pure _ <* p *> q
            case (Cont(p, _: Pure[_]), q) => new Then(p, q).optimise
            // mzero *> p = mzero (left zero and definition of *> in terms of >>=)
            case (z: MZero, _) => z
            // re-association - normal form of Then chain is to have result at the top of tree
            case (p, Then(q, r)) => new Then(new Then(p, q).optimise, r)
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation = (p, q) match
        {
            case (ct@CharTok(c), Pure(x)) => instrs += parsley.CharTokFastPerform[Char, B](c, _ => x, ct.expected); cont
            case (st@StringTok(s), Pure(x)) => instrs += new parsley.StringTokFastPerform(s, _ => x, st.expected); cont
            case (p, Pure(x)) =>
                new Suspended(p.codeGen
                {
                    instrs += new parsley.Exchange(x)
                    cont
                })
            case (p, q) =>
                new Suspended(p.codeGen
                {
                    instrs += parsley.Pop
                    q.codeGen(cont)
                })
        }
        override def discard: Parsley[A] = p
        override def result: Parsley[B] = q
        override def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): Then[A, B_] = new Then(prev, next)
    }
    private [parsley] final class Prev[B, +A](_p: =>Parsley[A], _q: =>Parsley[B]) extends Cont[B, A]
    {
        private [Prev] lazy val p = _p
        private [Prev] lazy val q = _q
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => q.optimised(q => cont(new Prev(p, q))))
        override def optimise: Parsley[A] = (p, q) match
        {
            // TODO: Consider char(c) <* char(d) => string(cd) *> pure(c) in some form!
            // p <* pure _ == p
            case (p, _: Pure[_]) => p
            // p <* mzero = p *> mzero (by preservation of error messages and failure properties) - This moves the pop instruction after the failure
            case (p, z: MZero) => new Then(p, z)
            // mzero <* p = mzero (left zero law and definition of <* in terms of >>=)
            case (z: MZero, _) => z
            // re-association - normal form of Prev chain is to have result at the top of tree
            case (Prev(r, q), p) => new Prev(r, new Prev(q, p).optimise)
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation = (p, q) match
        {
            case (Pure(x), ct@CharTok(c)) => instrs += parsley.CharTokFastPerform[Char, A](c, _ => x, ct.expected); cont
            case (Pure(x), st@StringTok(s)) => instrs += new parsley.StringTokFastPerform(s, _ => x, st.expected); cont
            case (Pure(x), q) =>
                new Suspended(q.codeGen
                {
                    instrs += new parsley.Exchange(x)
                    cont
                })
            case (p, q) =>
                new Suspended(p.codeGen
                {
                    q.codeGen
                    {
                        instrs += parsley.Pop
                        cont
                    }
                })
        }
        override def discard: Parsley[B] = q
        override def result: Parsley[A] = p
        override def copy[A_ >: A](prev: Parsley[B], next: Parsley[A_]): Prev[B, A_] = new Prev(next, prev)
    }
    private [parsley] final class Attempt[+A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Attempt] lazy val p = _p
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => cont(new Attempt(p)))
        // TODO: Pure and mzeros can be lifted out, attempts can be flattened
        override def optimise: Parsley[A] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val handler = labels.fresh()
            instrs += new parsley.PushHandler(handler)
            p.codeGen
            {
                instrs += new parsley.Label(handler)
                instrs += parsley.Attempt
                cont
            }
        }
    }
    private [parsley] final class Look[+A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Look] lazy val p = _p
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => cont(new Look(p)))
        override def optimise: Parsley[A] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val handler = labels.fresh()
            instrs += new parsley.PushHandler(handler)
            p.codeGen
            {
                instrs += new parsley.Label(handler)
                instrs += parsley.Look
                cont
            }
        }
    }
    private [parsley] sealed trait MZero extends Parsley[Nothing]
    private [parsley] class Empty extends MZero
    {
        override def preprocess(cont: Parsley[Nothing] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[Nothing] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.Empty(expected)
            cont
        }
    }
    private [parsley] final class Fail(private [Fail] val msg: String) extends MZero
    {
        override def preprocess(cont: Parsley[Nothing] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[Nothing] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.Fail(msg, expected)
            cont
        }
    }
    private [parsley] final class Unexpected(private [Unexpected] val msg: String) extends MZero
    {
        override def preprocess(cont: Parsley[Nothing] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[Nothing] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.Unexpected(msg, expected)
            cont
        }
    }
    private [parsley] final class Fixpoint[+A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Fixpoint] lazy val p = _p
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[A] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.Call(p, expected)
            cont
        }
    }
    // Intrinsic Embedding
    private [parsley] final class CharTok(private [CharTok] val c: Char) extends Parsley[Char]
    {
        override def preprocess(cont: Parsley[Char] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[Char] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += parsley.CharTok(c, expected)
            cont
        }
    }
    private [parsley] final class StringTok(private [StringTok] val s: String) extends Parsley[String]
    {
        override def preprocess(cont: Parsley[String] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            expected = label
            cont(this)
        }
        override def optimise: Parsley[String] = s match
        {
            case "" => new Pure("")
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            instrs += new parsley.StringTok(s, expected)
            cont
        }
    }
    private [parsley] final class Lift[A, B, +C](private [Lift] val f: (A, B) => C, _p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[C]
    {
        private [Lift] lazy val p = _p
        private [Lift] lazy val q = _q
        override def preprocess(cont: Parsley[C] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => q.optimised(q => cont(new Lift(f, p, q))))
        // TODO: Perform applicative fusion optimisations
        override def optimise: Parsley[C] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            new Suspended(p.codeGen
            {
                q.codeGen
                {
                    instrs += new parsley.Lift(f)
                    cont
                }
            })
        }
    }
    private [parsley] final class Cons[A, +B >: A](_p: =>Parsley[A], _ps: =>Parsley[List[B]]) extends Parsley[List[B]]
    {
        private [Cons] lazy val p = _p
        private [Cons] lazy val ps = _ps
        override def preprocess(cont: Parsley[List[B]] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => ps.optimised(ps => cont(new Cons(p, ps))))
        // TODO: Right associative normal form
        // TODO: Consider merging char ::s into strings?
        // TODO: Perform applicative fusion
        override def optimise: Parsley[List[B]] = this
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            new Suspended(p.codeGen
            {
                ps.codeGen
                {
                    instrs += parsley.Cons
                    cont
                }
            })
        }
    }
    private [parsley] final class FastFail[A](_p: =>Parsley[A], private [FastFail] val msggen: A => String) extends MZero
    {
        private [FastFail] lazy val p = _p
        override def preprocess(cont: Parsley[Nothing] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] = p.optimised(p =>
        {
            val ff = new FastFail(p, msggen)
            ff.expected = label
            cont(ff)
        })
        override def optimise: Parsley[Nothing] = p match
        {
            case Pure(x) => new Fail(msggen(x))
            case z: MZero => z
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            p.codeGen
            {
                instrs += new parsley.FastFail(msggen, expected)
                cont
            }
        }
    }
    private [parsley] final class Many[+A](_p: =>Parsley[A]) extends Parsley[List[A]]
    {
        private [Many] lazy val p = _p
        override def preprocess(cont: Parsley[List[A]] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => cont(new Many(p)))
        override def optimise: Parsley[List[A]] = p match
        {
            case _: Pure[A] => throw new Exception("many given parser which consumes no input")
            case _: MZero => new Pure(Nil)
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val body = labels.fresh()
            val handler = labels.fresh()
            instrs += new parsley.InputCheck(handler)
            instrs += new parsley.Label(body)
            new Suspended(p.codeGen
            {
                instrs += new parsley.Label(handler)
                instrs += new parsley.Many(body)
                cont
            })
        }
    }
    private [parsley] final class SkipMany[+A](_p: =>Parsley[A]) extends Parsley[Unit]
    {
        private [SkipMany] lazy val p = _p
        override def preprocess(cont: Parsley[Unit] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => cont(new SkipMany(p)))
        override def optimise: Parsley[Unit] = p match
        {
            case _: Pure[A] => throw new Exception("skipMany given parser which consumes no input")
            case _: MZero => new Pure(())
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val body = labels.fresh()
            val handler = labels.fresh()
            instrs += new parsley.InputCheck(handler)
            instrs += new parsley.Label(body)
            new Suspended(p.codeGen
            {
                instrs += new parsley.Label(handler)
                instrs += new parsley.SkipMany(body)
                cont
            })
        }
    }
    private [parsley] final class Chainl[A](_p: =>Parsley[A], _op: =>Parsley[A => A]) extends Parsley[A]
    {
        private [Chainl] lazy val p = _p
        private [Chainl] lazy val op = _op
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => op.optimised(op => cont(new Chainl(p, op))))
        override def optimise: Parsley[A] = op match
        {
            case _: Pure[A => A] => throw new Exception("left chain given parser which consumes no input")
            case _: MZero => p
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val body = labels.fresh()
            val handler = labels.fresh()
            new Suspended(p.codeGen
            {
                instrs += new parsley.InputCheck(handler)
                instrs += new parsley.Label(body)
                op.codeGen
                {
                    instrs += new parsley.Label(handler)
                    instrs += new parsley.Chainl(body)
                    cont
                }
            })
        }
    }
    private [parsley] final class Chainr[A](_p: =>Parsley[A], _op: =>Parsley[A => A]) extends Parsley[A]
    {
        private [Chainr] lazy val p = _p
        private [Chainr] lazy val op = _op
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
            p.optimised(p => op.optimised(op => cont(new Chainr(p, op))))
        override def optimise: Parsley[A] = op match
        {
            case _: Pure[A => A] => throw new Exception("right chain given parser which consumes no input")
            case _: MZero => p
            case _ => this
        }
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation =
        {
            val body = labels.fresh()
            val handler = labels.fresh()
            instrs += new parsley.InputCheck(handler)
            instrs += new parsley.Label(body)
            new Suspended(op.codeGen
            {
                instrs += new parsley.Label(handler)
                instrs += new parsley.Chainr(body)
                p.codeGen
                {
                    instrs += parsley.Apply
                    cont
                }
            })
        }
    }
    private [parsley] final class ErrorRelabel[+A](_p: =>Parsley[A], msg: String) extends Parsley[A]
    {
        private [ErrorRelabel] lazy val p = _p
        override def preprocess(cont: Parsley[A] => Bounce[Parsley[_]])(implicit seen: Set[Parsley[_]], label: UnsafeOption[String], depth: Int): Bounce[Parsley[_]] =
        {
            if (label == null) p.optimised(p => cont(p))(seen, msg, depth)
            else p.optimised(p => cont(p))
        }
        override def optimise: Parsley[A] = throw new Exception("Error relabelling should not be in optimisation!")
        override def codeGen(cont: =>Continuation)(implicit instrs: InstrBuffer, labels: LabelCounter): Continuation = throw new Exception("Error relabelling should not be in code gen!")
    }
    
    private [DeepEmbedding] object Pure       { def unapply[A](self: Pure[A]): Option[A] = Some(self.x) }
    private [DeepEmbedding] object App        { def unapply[A, B](self: App[A, B]): Option[(Parsley[A=>B], Parsley[A])] = Some((self.pf, self.px)) }
    private [DeepEmbedding] object Or         { def unapply[A, B >: A](self: Or[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q)) }
    private [DeepEmbedding] object Bind       { def unapply[A, B](self: Bind[A, B]): Option[(Parsley[A], A => Parsley[B])] = Some((self.p, self.f)) }
    private [DeepEmbedding] object Satisfy    { def unapply(self: Satisfy): Option[Char => Boolean] = Some(self.f) }
    private [DeepEmbedding] object Cont       { def unapply[A, B](self: Cont[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.discard, self.result)) }
    private [DeepEmbedding] object Then       { def unapply[A, B](self: Then[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q)) }
    private [DeepEmbedding] object Prev       { def unapply[A, B](self: Prev[B, A]): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q)) }
    private [DeepEmbedding] object Attempt    { def unapply[A](self: Attempt[A]): Option[Parsley[A]] = Some(self.p) }
    private [DeepEmbedding] object Look       { def unapply[A](self: Look[A]): Option[Parsley[A]] = Some(self.p) }
    private [DeepEmbedding] object Fail       { def unapply(self: Fail): Option[String] = Some(self.msg) }
    private [DeepEmbedding] object Unexpected { def unapply(self: Unexpected): Option[String] = Some(self.msg) }
    private [DeepEmbedding] object CharTok    { def unapply(self: CharTok): Option[Char] = Some(self.c) }
    private [DeepEmbedding] object StringTok  { def unapply(self: StringTok): Option[String] = Some(self.s) }
    private [DeepEmbedding] object Lift       { def unapply[A, B, C](self: Lift[A, B, C]): Option[((A, B) => C, Parsley[A], Parsley[B])] = Some((self.f, self.p, self.q))}
    private [DeepEmbedding] object Cons       { def unapply[A, B >: A](self: Cons[A, B]): Option[(Parsley[A], Parsley[List[B]])] = Some((self.p, self.ps)) }
    private [DeepEmbedding] object FastFail   { def unapply[A](self: FastFail[A]): Option[(Parsley[A], A=>String)] = Some((self.p, self.msggen)) }
    private [DeepEmbedding] object Many       { def unapply[A](self: Many[A]): Option[Parsley[A]] = Some(self.p) }
    private [DeepEmbedding] object SkipMany   { def unapply[A](self: SkipMany[A]): Option[Parsley[A]] = Some(self.p) }
    private [DeepEmbedding] object Chainl     { def unapply[A](self: Chainl[A]): Option[(Parsley[A], Parsley[A => A])] = Some((self.p, self.op)) }
    private [DeepEmbedding] object Chainr     { def unapply[A](self: Chainr[A]): Option[(Parsley[A], Parsley[A => A])] = Some((self.p, self.op)) }
    
    def main(args: Array[String]): Unit =
    {
        import parsley.Combinator._
        def many_[A](p: Parsley[A]): Parsley[List[A]] =
        {
            lazy val manyp: Parsley[List[A]] = (p <::> manyp) </> Nil
            manyp
        }
        lazy val p: Parsley[Int] = p.map((x: Int) => x+1)
        println(p.pretty)
        println(many_(p).pretty)
        val q: Parsley[Char] = 'a' <|> 'b'
        println((q <|> q <|> q <|> q).pretty)
        println((('a' >>= (_ => pure((x: Int) => x + 1))) <*> pure(7)).pretty)
        val chain = //chainl1(char('1') <#> (_.toInt), char('+') #> ((x: Int) => (y: Int) => x + y))
           chainPost('1' <#> (_.toInt), "+1" #> ((x: Int) => x+49))
        val start = System.currentTimeMillis()
        for (_ <- 0 to 10000000)
        {
            //(q <|> q <|> q <|> q).instrs
            runParserFastUnsafe(chain, "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1")
        }
        println(System.currentTimeMillis() - start)
        println(chain.pretty)
        println(runParser(many('a'), "aaaa"))
        lazy val uhoh: Parsley[Unit] = 'a' >>= (_ => uhoh)
        println(uhoh.pretty)
        println((for (a <- 'a';
                      b <- 'b')
                 yield (a, b)).pretty)
        try
        {
            many(pure(5)).pretty
            println("MANY ALLOWED NO-INPUT PARSER")
        }
        catch { case _: Throwable => }
        
        // Error message testing
        println(runParser('a' ? "ay!", "b"))
        lazy val r: Parsley[List[String]] = "correct error message" <::> (r </> Nil)
        println(runParser(r ? "nothing but this :)", ""))
        println(runParser(fail("hi"), "b"))
        println(runParser('a' <|> (fail("oops") ? "hi"), "b"))
        println(runParser(unexpected("bee"), "b"))
        println(runParser('a' <|> unexpected("bee") ? "something less cute", "b"))
        println(runParser(empty, "b"))
        println(runParser(empty ? "something, at least", "b"))
        println(runParser('a' <|> empty ? "something, at least", "b"))
        println(eof.pretty)
        println(runParser(eof, "a"))
        println("hi")
        println(runParser(('a' *> 'b') <|> 'a', "a"))
        // This is a stack overflow test
        def repeat(n: Int, p: Parsley[Char]): Parsley[Char] =
        {
            if (n > 0) p *> repeat(n-1, p)
            else p
        }
        //println(repeat(100000, 'a').pretty)
    }
}
