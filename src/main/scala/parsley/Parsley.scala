package parsley

import parsley.internal.instructions.Context
import parsley.internal.deepembedding
import parsley.expr.chain
import parsley.combinator.{option, some}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.io.Source
import java.io.File
import scala.language.{higherKinds, implicitConversions}

// User API
/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is abstract
  *
  * @author Jamie Willis
  * @version 1
  */
final class Parsley[+A] private [parsley] (private [parsley] val internal: deepembedding.Parsley[A]) extends AnyVal
{
    /**
      * Using this method signifies that the parser it is invoked on is impure and any optimisations which assume purity
      * are disabled.
      */
    def unsafe(): Unit = internal.unsafe()

    /**
      * Forces the compilation of a parser as opposed to the regular lazy evaluation.
      */
    def force(): Unit = internal.force()

    /**
      *
      * Provides an indicator that this parser is likely to stack-overflow
      */
    def overflows(): Unit = internal.overflows()

    //TODO: we need a way to set source pos and filename
    /** This method is responsible for actually executing parsers. Given an input
      * string, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param input The input to run against
      * @return Either a success with a value of type `A` or a failure with error message
      */
    def runParser(input: String): Result[A] = runParser(input.toCharArray)
    /** This method is responsible for actually executing parsers. Given an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param input The input to run against
      * @return Either a success with a value of type `A` or a failure with error message
      */
    def runParser(input: Array[Char]): Result[A] = new Context(internal.threadSafeInstrs, input).runParser()
    /** This method executes a parser, but collects the input to the parser from the given file.
      * The file name is used to annotate any error messages.
      * @param file The file to load and run against
      * @return Either a success with a value of type `A` or a failure with error message
      */
    def parseFromFile(file: File): Result[A] = new Context(internal.threadSafeInstrs, Source.fromFile(file).toArray, Some(file.getName)).runParser()
}
/** This object contains the core "function-style" combinators as well as the implicit classes which provide
  * the "method-style" combinators. All parsers will likely require something from within! */
object Parsley
{
    implicit final class LazyParsley[P, +A](p: =>P)(implicit con: P => Parsley[A])
    {
        /**
          * This is the functorial map operation for parsers. When the invokee produces a value, this value is fed through
          * the function `f`.
          *
          * @note This is subject to aggressive optimisations assuming purity; the compiler is permitted to optimise such
          * that the application of `f` actually only happens once at compile time. In order to preserve the behaviour of
          * impure functions, consider using the `unsafe` method before map; `p.unsafe.map(f)`.
          * @param f The mutator to apply to the result of previous parse
          * @return A new parser which parses the same input as the invokee but mutated by function `f`
          */
        def map[B](f: A => B): Parsley[B] = pure(f) <*> p
        /**This combinator is an alias for `map`*/
        def <#>[B](f: A => B): Parsley[B] = this.map(f)
        /**
          * This is the Applicative application parser. The type of `pf` is `Parsley[A => B]`. Then, given a
          * `Parsley[A]`, we can produce a `Parsley[B]` by parsing `pf` to retrieve `f: A => B`, then parse `px`
          * to receive `x: A` then return `f(x): B`.
          *
          * @note `pure(f) <*> p` is subject to the same aggressive optimisations as `map`. When using impure functions
          * the optimiser may decide to cache the result of the function execution, be sure to use `unsafe` in order to
          * prevent these optimisations.
          * @param px A parser of type A, where the invokee is A => B
          * @return A new parser which parses `pf`, then `px` then applies the value returned by `px` to the function
          *         returned by `pf`
          */
        def <*>[B, C](px: =>Parsley[B])(implicit ev: P <:< Parsley[B=>C]): Parsley[C] = new Parsley(new deepembedding.<*>[B, C](ev(p).internal, px.internal))
        /**
          * This is the traditional Monadic binding operator for parsers. When the invokee produces a value, the function
          * `f` is used to produce a new parser that continued the computation.
          *
          * @note There is significant overhead for using flatMap; if possible try to write parsers in an applicative
          * style otherwise try and use the intrinsic parsers provided to replace the flatMap.
          * @param f A function that produces the next parser
          * @return The parser produces from the application of `f` on the result of the last parser
          */
        def flatMap[B](f: A => Parsley[B]): Parsley[B] = new Parsley(new deepembedding.>>=(p.internal, f.andThen(_.internal)))
        /**This combinator is an alias for `flatMap(identity)`.*/
        def flatten[B](implicit ev: A <:< Parsley[B]): Parsley[B] = this.flatMap[B](ev)

        /**This combinator is an alias for `flatMap`*/
        def >>=[B](f: A => Parsley[B]): Parsley[B] = this.flatMap(f)
        /**This combinator is defined as `lift2((x, f) => f(x), p, f)`. It is pure syntactic sugar.*/
        def <**>[B](pf: =>Parsley[A => B]): Parsley[B] = lift.lift2[A, A=>B, B]((x, f) => f(x), p, pf)
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
        def <|>[B >: A](q: =>Parsley[B]): Parsley[B] = new Parsley(new deepembedding.<|>(p.internal, q.internal))
        /**This combinator is defined as `p <|> pure(x)`. It is pure syntactic sugar.*/
        def </>[B >: A](x: B): Parsley[B] = this <|> pure(x)
        /**This combinator is an alias for `<|>`.*/
        def orElse[B >: A](q: =>Parsley[B]): Parsley[B] = this <|> q
        /**This combinator is an alias for `</>`.*/
        def getOrElse[B >: A](x: B): Parsley[B] = this </> x
        /**This combinator is defined as `attempt(p) <|> q`. It is pure syntactic sugar.*/
        def <\>[B >: A](q: Parsley[B]): Parsley[B] = attempt(p) <|> q
        /**
          * This is the parser that corresponds to a more optimal version of `p.map(_ => x => x) <*> q`. It performs
          * the parse action of both parsers, in order, but discards the result of the invokee.
          * @param q The parser whose result should be returned
          * @return A new parser which first parses `p`, then `q` and returns the result of `q`
          */
        def *>[A_ >: A, B](q: =>Parsley[B]): Parsley[B] = new Parsley(new deepembedding.*>[A_, B](p.internal, q.internal))
        /**
          * This is the parser that corresponds to a more optimal version of `p.map(x => _ => x) <*> q`. It performs
          * the parse action of both parsers, in order, but discards the result of the second parser.
          * @param q The parser who should be executed but then discarded
          * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
          */
        def <*[B](q: =>Parsley[B]): Parsley[A] = new Parsley(new deepembedding.<*(p.internal, q.internal))
        /**
          * This is the parser that corresponds to `p *> pure(x)` or a more optimal version of `p.map(_ => x)`.
          * It performs the parse action of the invokee but discards its result and then results the value `x` instead
          * @param x The value to be returned after the execution of the invokee
          * @return A new parser which first parses the invokee, then results `x`
          */
        def #>[B](x: B): Parsley[B] = this *> pure(x)
        /**This combinator is an alias for `*>`*/
        def >>[B](q: Parsley[B]): Parsley[B] = this *> q
        /**
          * This is the parser that corresponds to a more optimal version of `(p <~> q).map(_._2)`. It performs
          * the parse action of both parsers, in order, but discards the result of the invokee.
          * @param q The parser whose result should be returned
          * @return A new parser which first parses `p`, then `q` and returns the result of `q`
          */
        def ~>[B](q: Parsley[B]): Parsley[B] = this *> q
        /**
          * This is the parser that corresponds to a more optimal version of `(p <~> q).map(_._1)`. It performs
          * the parse action of both parsers, in order, but discards the result of the second parser.
          * @param q The parser who should be executed but then discarded
          * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
          */
        def <~[B](q: Parsley[B]): Parsley[A] = this <* q
        /**This parser corresponds to `lift2(_+:_, p, ps)`.*/
        def <+:>[B >: A](ps: =>Parsley[Seq[B]]): Parsley[Seq[B]] = lift.lift2[A, Seq[B], Seq[B]](_ +: _, p, ps)
        /**This parser corresponds to `lift2(_::_, p, ps)`.*/
        def <::>[B >: A](ps: =>Parsley[List[B]]): Parsley[List[B]] = lift.lift2[A, List[B], List[B]](_ :: _, p, ps)
        /**This parser corresponds to `lift2((_, _), p, q)`. For now it is sugar, but in future may be more optimal*/
        def <~>[A_ >: A, B](q: =>Parsley[B]): Parsley[(A_, B)] = lift.lift2[A_, B, (A_, B)]((_, _), p, q)
        /**This combinator is an alias for `<~>`*/
        def zip[A_ >: A, B](q: =>Parsley[B]): Parsley[(A_, B)] = this <~> q
        /** Filter the value of a parser; if the value returned by the parser matches the predicate `pred` then the
          * filter succeeded, otherwise the parser fails with an empty error
          * @param pred The predicate that is tested against the parser result
          * @return The result of the invokee if it passes the predicate
          */
        def filter(pred: A => Boolean): Parsley[A] = new Parsley(new deepembedding.Filter(p.internal, pred))
        /** Filter the value of a parser; if the value returned by the parser does not match the predicate `pred` then the
          * filter succeeded, otherwise the parser fails with an empty error
          * @param pred The predicate that is tested against the parser result
          * @return The result of the invokee if it passes the predicate
          */
        def filterNot(pred: A => Boolean): Parsley[A] = this.filter(!pred(_))
        def withFilter(pred: A => Boolean): Parsley[A] = this.filter(pred)
        /** Attempts to first filter the parser to ensure that `pf` is defined over it. If it is, then the function `pf`
          * is mapped over its result. Roughly the same as a `filter` then a `map`.
          * @param pf The partial function
          * @return The result of applying `pf` to this parsers value (if possible), or fails
          * @since 1.7
          */
        def collect[B](pf: PartialFunction[A, B]): Parsley[B] = this.filter(pf.isDefinedAt).map(pf)
        /** Attempts to first filter the parser to ensure that `pf` is defined over it. If it is, then the function `pf`
          * is mapped over its result. Roughly the same as a `guard` then a `map`.
          * @param pf The partial function
          * @param msg The message used for the error if the input failed the check
          * @return The result of applying `pf` to this parsers value (if possible), or fails
          * @since 1.7
          */
        def collect[B](msg: String)(pf: PartialFunction[A, B]): Parsley[B] = this.guard(pf.isDefinedAt, msg).map(pf)
        /** Attempts to first filter the parser to ensure that `pf` is defined over it. If it is, then the function `pf`
          * is mapped over its result. Roughly the same as a `guard` then a `map`.
          * @param pf The partial function
          * @param msggen Generator function for error message, generating a message based on the result of the parser
          * @return The result of applying `pf` to this parsers value (if possible), or fails
          * @since 1.7
          */
        def collect[B](msggen: A => String)(pf: PartialFunction[A, B]): Parsley[B] = this.guard(pf.isDefinedAt, msggen).map(pf)
        /** Similar to `filter`, except the error message desired is also provided. This allows you to name the message
          * itself.
          * @param pred The predicate that is tested against the parser result
          * @param msg The message used for the error if the input failed the check
          * @return The result of the invokee if it passes the predicate
          */
        def guard(pred: A => Boolean, msg: String): Parsley[A] = new Parsley(new deepembedding.Guard(p.internal, pred, msg))
        /** Similar to `filter`, except the error message desired is also provided. This allows you to name the message
          * itself. The message is provided as a generator, which allows the user to avoid otherwise expensive
          * computation.
          * @param pred The predicate that is tested against the parser result
          * @param msggen Generator function for error message, generating a message based on the result of the parser
          * @return The result of the invokee if it passes the predicate
          */
        def guard(pred: A => Boolean, msggen: A => String): Parsley[A] = new Parsley(new deepembedding.FastGuard(p.internal, pred, msggen))
        /** Similar to `filterNot`, except the error message desired is also provided. This allows you to name the message
         * itself.
         * @param pred The predicate that is tested against the parser result
         * @param msg The message used for the error if the input failed the check
         * @return The result of the invokee if it passes the predicate
         */
        def guardNot(pred: A => Boolean, msg: String): Parsley[A] = this.guard((a: A) => !pred(a), msg)
        /** Similar to `filterNot`, except the error message desired is also provided. This allows you to name the message
         * itself. The message is provided as a generator, which allows the user to avoid otherwise expensive
         * computation.
         * @param pred The predicate that is tested against the parser result
         * @param msggen Generator function for error message, generating a message based on the result of the parser
         * @return The result of the invokee if it passes the predicate
         */
        def guardNot(pred: A => Boolean, msggen: A => String): Parsley[A] = this.guard((a: A) => !pred(a), msggen)
        /**Alias for guard combinator, taking a fixed message.*/
        def >?>(pred: A => Boolean, msg: String): Parsley[A] = this.guard(pred, msg)
        /**Alias for guard combinator, taking a dynamic message generator.*/
        def >?>(pred: A => Boolean, msggen: A => String): Parsley[A] = this.guard(pred, msggen)
        /**Sets the expected message for a parser. If the parser fails then `expected msg` will added to the error*/
        def ?(msg: String): Parsley[A] = new Parsley(new deepembedding.ErrorRelabel(p.internal, msg))
        /**Hides the "expected" error message for a parser.*/
        def hide: Parsley[A] = ?("")
        /** Same as `fail`, except allows for a message generated from the result of the failed parser. In essence, this
          * is equivalent to `p >>= (x => fail(msggen(x))` but requires no expensive computations from the use of `>>=`.
          * @param msggen The generator function for error message, creating a message based on the result of invokee
          * @return A parser that fails if it succeeds, with the given generator used to produce the error message
          */
        def !(msggen: A => String): Parsley[Nothing] = new Parsley(new deepembedding.FastFail(p.internal, msggen))
        /** Same as `unexpected`, except allows for a message generated from the result of the failed parser. In essence,
          * this is equivalent to `p >>= (x => unexpected(x))` but requires no expensive computations from the use of
          * `>>=`
          * @param msggen The generator function for error message, creating a message based on the result of invokee
          * @return A parser that fails if it succeeds, with the given generator used to produce an unexpected message
          */
        def unexpected(msggen: A => String): Parsley[Nothing] = new Parsley(new deepembedding.FastUnexpected(p.internal, msggen))
        /**
          * A fold for a parser: `p.foldRight(k)(f)` will try executing `p` many times until it fails, combining the
          * results with right-associative application of `f` with a `k` at the right-most position
          *
          * @example {{{p.foldRight(Nil)(_::_) == many(p) //many is more efficient, however}}}
          *
          * @param k base case for iteration
          * @param f combining function
          * @return the result of folding the results of `p` with `f` and `k`
          */
        def foldRight[B](k: B)(f: (A, B) => B): Parsley[B] = chain.prefix(this.map(f.curried), pure(k))
        /**
          * A fold for a parser: `p.foldLeft(k)(f)` will try executing `p` many times until it fails, combining the
          * results with left-associative application of `f` with a `k` on the left-most position
          *
          * @example {{{val natural: Parsley[Int] = digit.foldLeft(0)((x, d) => x * 10 + d.toInt)}}}
          *
          * @param k base case for iteration
          * @param f combining function
          * @return the result of folding the results of `p` with `f` and `k`
          */
        def foldLeft[B](k: B)(f: (B, A) => B): Parsley[B] = new Parsley(new deepembedding.Chainl(pure(k).internal, p.internal, pure(f).internal))
        /**
          * A fold for a parser: `p.foldRight1(k)(f)` will try executing `p` many times until it fails, combining the
          * results with right-associative application of `f` with a `k` at the right-most position. It must parse `p`
          * at least once.
          *
          * @example {{{p.foldRight1(Nil)(_::_) == some(p) //some is more efficient, however}}}
          *
          * @param k base case for iteration
          * @param f combining function
          * @return the result of folding the results of `p` with `f` and `k`
          */
        def foldRight1[B](k: B)(f: (A, B) => B): Parsley[B] = {
            lazy val q: Parsley[A] = p
            lift.lift2(f, q, q.foldRight(k)(f))
        }
        /**
          * A fold for a parser: `p.foldLeft1(k)(f)` will try executing `p` many times until it fails, combining the
          * results with left-associative application of `f` with a `k` on the left-most position. It must parse `p`
          * at least once.
          *
          * @example {{{val natural: Parsley[Int] = digit.foldLeft1(0)((x, d) => x * 10 + d.toInt)}}}
          *
          * @param k base case for iteration
          * @param f combining function
          * @return the result of folding the results of `p` with `f` and `k`
          */
        def foldLeft1[B](k: B)(f: (B, A) => B): Parsley[B] = {
            lazy val q: Parsley[A] = p
            new Parsley(new deepembedding.Chainl(q.map(f(k, _)).internal, q.internal, pure(f).internal))
        }
        /**
          * A reduction for a parser: `p.reduceRight(op)` will try executing `p` many times until it fails, combining the
          * results with right-associative application of `op`. It must parse `p` at least once.
          *
          * @param op combining function
          * @return the result of reducing the results of `p` with `op`
          */
        def reduceRight[B >: A](op: (A, B) => B): Parsley[B] = some(p).map(_.reduceRight(op))
        /**
          * A reduction for a parser: `p.reduceRight(op)` will try executing `p` many times until it fails, combining the
          * results with right-associative application of `op`. If there is no `p`, it returns `None`, otherwise it returns
          * `Some(x)` where `x` is the result of the reduction.
          *
          * @param op combining function
          * @return the result of reducing the results of `p` with `op` wrapped in `Some` or `None` otherwise
          */
        def reduceRightOption[B >: A](op: (A, B) => B): Parsley[Option[B]] = option(this.reduceRight(op))
        /**
          * A reduction for a parser: `p.reduceLeft(op)` will try executing `p` many times until it fails, combining the
          * results with left-associative application of `op`. It must parse `p` at least once.
          *
          * @param op combining function
          * @return the result of reducing the results of `p` with `op`
          */
        def reduceLeft[B >: A](op: (B, A) => B): Parsley[B] = chain.left1(p, pure(op))
        /**
          * A reduction for a parser: `p.reduceLeft(op)` will try executing `p` many times until it fails, combining the
          * results with left-associative application of `op`. If there is no `p`, it returns `None`, otherwise it returns
          * `Some(x)` where `x` is the result of the reduction.
          *
          * @param op combining function
          * @return the result of reducing the results of `p` with `op` wrapped in `Some` or `None` otherwise
          */
        def reduceLeftOption[B >: A](op: (B, A) => B): Parsley[Option[B]] = option(this.reduceLeft(op))
        /**
          * This casts the result of the parser into a new type `B`. If the value returned by the parser
          * is castable to type `B`, then this cast is performed. Otherwise the parser fails.
          * @tparam B The type to attempt to cast into
          * @since 1.7
          */
        def cast[B: ClassTag]: Parsley[B] = this.collect {
            case x: B => x
        }
    }
    implicit final class LazyMapParsley[A, +B](f: A => B)
    {
        /**This combinator is an alias for `map`*/
        def <#>(p: =>Parsley[A]): Parsley[B] = p.map(f)
    }
    implicit final class LazyChooseParsley[P, +A](pq: =>(P, P))(implicit con: P => Parsley[A])
    {
        private lazy val (p, q) = pq
        /**
          * This is an if statement lifted to the parser level. Formally, this is a selective functor operation,
          * equivalent to (branch b.map(boolToEither) (p.map(const)) (q.map(const))).
          * Note: due to Scala operator associativity laws, this is a right-associative operator, and must be properly
          * bracketed, technically the invokee is the rhs...
          * @param b The parser that yields the condition value
          * @return The result of either `p` or `q` depending on the return value of the invokee
          */
        def ?:(b: =>Parsley[Boolean]): Parsley[A] = new Parsley(new deepembedding.If(b.internal, p.internal, q.internal))
    }

    /** This is the traditional applicative pure function (or monadic return) for parsers. It consumes no input and
      * does not influence the state of the parser, but does return the value provided. Useful to inject pure values
      * into the parsing process.
      * @param x The value to be returned from the parser
      * @return A parser which consumes nothing and returns `x`
      */
    def pure[A](x: A): Parsley[A] = new Parsley(new deepembedding.Pure(x))
    // $COVERAGE-OFF$
    /** `lift1(f, p)` is an alias for `p.map(f)`. It is provided for symmetry with lift2 and lift3 */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.lift.lift1` instead", "v2.2.0")
    def lift1[A, B](f: A => B, p: =>Parsley[A]): Parsley[B] = lift.lift1(f, p)
    /** Traditionally, `lift2` is defined as `lift2(f, p, q) = p.map(f) <*> q`. However, `f` is actually uncurried,
      * so it's actually more exactly defined as; read `p` and then read `q` then provide their results to function
      * `f`. This is designed to bring higher performance to any curried operations that are not themselves
      * intrinsic.
      * @param f The function to apply to the results of `p` and `q`
      * @param p The first parser to parse
      * @param q The second parser to parse
      * @return `f(x, y)` where `x` is the result of `p` and `y` is the result of `q`.
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.lift.lift2` instead", "v2.2.0")
    def lift2[A, B, C](f: (A, B) => C, p: =>Parsley[A], q: =>Parsley[B]): Parsley[C] = lift.lift2(f, p, q)
    /** Traditionally, `lift2` is defined as `lift3(f, p, q, r) = p.map(f) <*> q <*> r`. However, `f` is actually uncurried,
      * so it's actually more exactly defined as; read `p` and then read `q` and then read 'r' then provide their results
      * to function `f`. This is designed to bring higher performance to any curried operations that are not themselves
      * intrinsic.
      * @param f The function to apply to the results of `p` and `q`
      * @param p The first parser to parse
      * @param q The second parser to parse
      * @param r The third parser to parse
      * @return `f(x, y, z)` where `x` is the result of `p`, `y` is the result of `q` and `z` is the result of `r`.
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.lift.lift3` instead", "v2.2.0")
    def lift3[A, B, C, D](f: (A, B, C) => D, p: =>Parsley[A], q: =>Parsley[B], r: =>Parsley[C]): Parsley[D] = lift.lift3(f, p, q, r)
    // $COVERAGE-ON$
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `p` and `q`
      * depending on the result from `b`. This can be used to implement conditional choice within a parser without
      * relying on expensive monadic operations.
      * @param b The first parser to parse
      * @param p If `b` returns `Left` then this parser is executed with the result
      * @param q If `b` returns `Right` then this parser is executed with the result
      * @return Either the result from `p` or `q` depending on `b`.
      */
    def branch[A, B, C](b: =>Parsley[Either[A, B]], p: =>Parsley[A => C], q: =>Parsley[B => C]): Parsley[C] =
        // TODO: This should be converted to use Case instruction from Haskell Parsley, this is too inefficient right now
        // We can then apply some laws and optimisations to it...
        b >>= {
            case Left(x) => p <*> pure(x)
            case Right(y) => q <*> pure(y)
        }
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `q` depending on
      * whether or not `p` returns a `Left`. It can be used to implement `branch` and other selective operations, however
      * it is more efficiently implemented with `branch` itself.
      * @param p The first parser to parse
      * @param q If `p` returns `Left` then this parser is executed with the result
      * @return Either the result from `p` if it returned `Left` or the result of `q` applied to the `Right` from `p`
      */
    def select[A, B](p: =>Parsley[Either[A, B]], q: =>Parsley[A => B]): Parsley[B] = branch(p, q, pure(identity[B](_)))
    /**This function is an alias for `_.flatten`. Provides namesake to Haskell.*/
    def join[A](p: =>Parsley[Parsley[A]]): Parsley[A] = p.flatten
    /** Given a parser `p`, attempts to parse `p`. If the parser fails, then `attempt` ensures that no input was
      * consumed. This allows for backtracking capabilities, disabling the implicit cut semantics offered by `<|>`.
      * @param p The parser to run
      * @return The result of `p`, or if `p` failed ensures the parser state was as it was on entry.
      */
    def attempt[A](p: =>Parsley[A]): Parsley[A] = new Parsley(new deepembedding.Attempt(p.internal))
    /** Parses `p` without consuming any input. If `p` fails and consumes input then so does `lookAhead(p)`. Combine with
      * `attempt` if this is undesirable.
      * @param p The parser to look ahead at
      * @return The result of the lookahead
      */
    def lookAhead[A](p: =>Parsley[A]): Parsley[A] = new Parsley(new deepembedding.Look(p.internal))
    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new Parsley(new deepembedding.NotFollowedBy(p.internal))
    /**Alias for `p ? msg`.*/
    def label[A](p: Parsley[A], msg: String): Parsley[A] = p ? msg
    /** The `fail(msg)` parser consumes no input and fails with `msg` as the error message */
    def fail(msg: String): Parsley[Nothing] = new Parsley(new deepembedding.Fail(msg))
    /** The `empty` parser consumes no input and fails softly (that is to say, no error message) */
    val empty: Parsley[Nothing] = new Parsley(new deepembedding.Empty)
    /** The `unexpected(msg)` parser consumes no input and fails with `msg` as an unexpected error */
    def unexpected(msg: String): Parsley[Nothing] = new Parsley(new deepembedding.Unexpected(msg))
    /** Returns `()`. Defined as `pure(())` but aliased for sugar*/
    val unit: Parsley[Unit] = pure(())
    /** converts a parser's result to () */
    def void(p: Parsley[_]): Parsley[Unit] = p *> unit
    // $COVERAGE-OFF$
    /** `many(p)` executes the parser `p` zero or more times. Returns a list of the returned values of `p`. */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.many` instead", "v2.2.0")
    def many[A](p: =>Parsley[A]): Parsley[List[A]] = combinator.many(p)
    /** `skipMany(p)` executes the parser `p` zero or more times and ignores the results. Returns `()` */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.skipMany` instead", "v2.2.0")
    def skipMany[A](p: =>Parsley[A]): Parsley[Unit] = combinator.skipMany(p)
    // $COVERAGE-ON$
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, collecting the results.
      * @param ps Parsers to be sequenced
      * @return The list containing results, one from each parser, in order
      */
    def sequence[A](ps: Parsley[A]*): Parsley[List[A]] = ps.foldRight(pure[List[A]](Nil))(_ <::> _)
    /**
      * Like `sequence` but produces a list of parsers to sequence by applying the function `f` to each
      * element in `xs`.
      * @param f The function to map on each element of `xs` to produce parsers
      * @param xs Values to generate parsers from
      * @return The list containing results formed by executing each parser generated from `xs` and `f` in sequence
      */
    def traverse[A, B](f: A => Parsley[B], xs: A*): Parsley[List[B]] = sequence(xs.map(f): _*)
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, ignoring the results.
      * @param ps Parsers to be performed
      */
    def skip(ps: Parsley[_]*): Parsley[Unit] = ps.foldRight(unit)(_ *> _)
    /**
      * This parser consumes no input and returns the current line number reached in the input stream
      * @return The line number the parser is currently at
      */
    val line: Parsley[Int] = new Parsley(deepembedding.Line)
    /**
      * This parser consumes no input and returns the current column number reached in the input stream
      * @return The column number the parser is currently at
      */
    val col: Parsley[Int] = new Parsley(deepembedding.Col)
    /**
      * This parser consumes no input and returns the current position reached in the input stream
      * @return Tuple of line and column number that the parser has reached
      */
    val pos: Parsley[(Int, Int)] = line <~> col
    // $COVERAGE-OFF$
    /**
      * Consumes no input and returns the value stored in one of the parser registers.
      * @note There are only 4 registers at present.
      * @param r The index of the register to collect from
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @return The value stored in register `r` of type `S`
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.get` instead", "v2.2.0")
    def get[S](r: registers.Reg[S]): Parsley[S] = registers.get(r)
    /**
      * Consumes no input and returns the value stored in one of the parser registers after applying a function.
      * @note There are only 4 registers at present.
      * @param r The index of the register to collect from
      * @param f The function used to transform the value in the register
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `r` applied to `f`
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.gets` instead", "v2.2.0")
    def gets[S, A](r: registers.Reg[S], f: S => A): Parsley[A] = registers.gets(r, f)
    /**
      * Returns the value stored in one of the parser registers after applying a function obtained from given parser.
      * @note There are only 4 registers at present. The value is fetched before `pf` is executed
      * @param r The index of the register to collect from
      * @param pf The parser which provides the function to transform values
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `r` applied to `f` from `pf`
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.gets` instead", "v2.2.0")
    def gets[S, A](r: registers.Reg[S], pf: Parsley[S => A]): Parsley[A] = get(r) <**> pf
    /**
      * Consumes no input and places the value `x` into register `r`.
      * @note There are only 4 registers at present.
      * @param r The index of the register to place the value in
      * @param x The value to place in the register
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.put` instead", "v2.2.0")
    def put[S](r: registers.Reg[S], x: S): Parsley[Unit] = registers.put(r, x)
    /**
      * Places the result of running `p` into register `r`.
      * @note There are only 4 registers at present.
      * @param r The index of the register to place the value in
      * @param p The parser to derive the value from
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.put` instead", "v2.2.0")
    def put[S](r: registers.Reg[S], p: =>Parsley[S]): Parsley[Unit] = registers.put(r, p)
    /**
      * Modifies the value contained in register `r` using function `f`.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param f The function used to modify the register
      * @tparam S The type of value currently assumed to be in the register
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.modify` instead", "v2.2.0")
    def modify[S](r: registers.Reg[S], f: S => S): Parsley[Unit] = registers.modify(r, f)
    /**
      * For the duration of parser `p` the state stored in register `r` is instead set to `x`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param x The value to place in the register `r`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.local` instead", "v2.2.0")
    def local[R, A](r: registers.Reg[R], x: R, p: =>Parsley[A]): Parsley[A] = registers.local(r, x, p)
    /**
      * For the duration of parser `q` the state stored in register `r` is instead set to the return value of `p`. The
      * change is undone after `q` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param p The parser whose return value is placed in register `r`
      * @param q The parser to execute with the adjusted state
      * @return The parser that performs `q` with the modified state
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.local` instead", "v2.2.0")
    def local[R, A](r: registers.Reg[R], p: =>Parsley[R], q: =>Parsley[A]): Parsley[A] = registers.local(r, p, q)
    /**
      * For the duration of parser `p` the state stored in register `r` is instead modified with `f`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param f The function used to modify the value in register `r`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.local` instead", "v2.2.0")
    def local[R, A](r: registers.Reg[R], f: R => R, p: =>Parsley[A]): Parsley[A] = registers.local(r, f, p)

    /** `rollback(reg, p)` will perform `p`, but if it fails without consuming input, any changes to the register `reg` will
      * be reverted.
      * @param p The parser to perform
      * @param reg The register to rollback on failure of `p`
      * @return The result of the parser `p`, if any
      * @since 2.0
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.registers.rollback` instead", "v2.2.0")
    def rollback[A, B](reg: registers.Reg[A], p: Parsley[B]): Parsley[B] = registers.rollback(reg, p)
    // $COVERAGE-ON$
}
