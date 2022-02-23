package parsley

import parsley.internal.machine.Context
import parsley.internal.deepembedding
import parsley.expr.{chain, infix}
import parsley.combinator.{option, some}
import parsley.Parsley.pure
import parsley.errors.ErrorBuilder

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.language.{higherKinds, implicitConversions}

// User API
/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `parse` will
  * parse the string given as input to `parse`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is opaque.
  *
  * @author Jamie Willis
  * @version 3.0.0
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

    // $COVERAGE-OFF$
    /**
      *
      * Provides an indicator that this parser is likely to stack-overflow
      */
    def overflows(): Unit = internal.overflows()
    // $COVERAGE-ON$

    /** This method is responsible for actually executing parsers. Given an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param input The input to run against
      * @return Either a success with a value of type `A` or a failure with error message
      * @since 3.0.0
      */
    def parse[Err: ErrorBuilder](input: String): Result[Err, A] = new Context(internal.threadSafeInstrs, input).runParser()

    /* COMBINATORS */
    /**
      * This is the functorial map operation for parsers. When the receiver produces a value, this value is fed through
      * the function `f`.
      *
      * @note This is subject to aggressive optimisations assuming purity; the compiler is permitted to optimise such
      * that the application of `f` actually only happens once at compile time. In order to preserve the behaviour of
      * impure functions, consider using the `unsafe` method before map; `p.unsafe.map(f)`.
      * @param f The mutator to apply to the result of previous parse
      * @return A new parser which parses the same input as the receiver but mutated by function `f`
      */
    def map[B](f: A => B): Parsley[B] = pure(f) <*> this
    /**
      * This is the Applicative application parser. The type of `pf` is `Parsley[A => B]`. Then, given a
      * `Parsley[A]`, we can produce a `Parsley[B]` by parsing `pf` to retrieve `f: A => B`, then parse `px`
      * to receive `x: A` then return `f(x): B`.
      *
      * @note `pure(f) <*> p` is subject to the same aggressive optimisations as `map`. When using impure functions
      * the optimiser may decide to cache the result of the function execution, be sure to use `unsafe` in order to
      * prevent these optimisations.
      * @param px A parser of type A, where the receiver is A => B
      * @return A new parser which parses `pf`, then `px` then applies the value returned by `px` to the function
      *         returned by `pf`
      */
    def <*>[B, C](px: =>Parsley[B])
                 (implicit ev: Parsley[A] <:< Parsley[B=>C]): Parsley[C] = new Parsley(new deepembedding.<*>[B, C](ev(this).internal, px.internal))

    /**
      * This is the traditional Monadic binding operator for parsers. When the receiver produces a value, the function
      * `f` is used to produce a new parser that continued the computation.
      *
      * @note There is significant overhead for using flatMap; if possible try to write parsers in an applicative
      * style otherwise try and use the intrinsic parsers provided to replace the flatMap.
      * @param f A function that produces the next parser
      * @return The parser produces from the application of `f` on the result of the last parser
      */
    def flatMap[B](f: A => Parsley[B]): Parsley[B] = new Parsley(new deepembedding.>>=(this.internal, f.andThen(_.internal)))
    /**This combinator is an alias for `flatMap(identity)`.*/
    def flatten[B](implicit ev: A <:< Parsley[B]): Parsley[B] = this.flatMap[B](ev)
    /**This combinator is an alias for `flatMap`*/
    def >>=[B](f: A => Parsley[B]): Parsley[B] = this.flatMap(f)
    /**This combinator is defined as `lift2((x, f) => f(x), p, f)`. It is pure syntactic sugar.*/
    def <**>[B](pf: =>Parsley[A => B]): Parsley[B] = lift.lift2[A, A=>B, B]((x, f) => f(x), this, pf)

    /**
      * This is the traditional choice operator for parsers. Following the parsec semantics precisely,
      * this combinator first tries to parse the receiver. If this is successful, no further action is taken. If the
      * receiver failed *without* consuming input, then `q` is parsed instead. If the receiver did parse input then the
      * whole parser fails. This is done to prevent space leaks and to give good error messages. If this behaviour
      * is not desired, use `attempt(this) <|> q` to parse `q` regardless of how the
      * receiver failed.
      * @param q The parser to run if the receiver failed without consuming input
      * @return The value produced by the receiver if it was successful, or if it failed without consuming input, the
      *         possible result of parsing q.
      */
    def <|>[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = new Parsley(new deepembedding.<|>(this.internal, q.internal))
    /**This combinator is an alias for `<|>`.
      * @since 4.0.0
      */
    def |[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = this <|> q
    /**This combinator is defined as `p <|> pure(x)`. It is pure syntactic sugar.*/
    def </>[Aʹ >: A](x: Aʹ): Parsley[Aʹ] = this <|> pure(x)
    /**This combinator is an alias for `<|>`.*/
    def orElse[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = this <|> q
    /**This combinator is an alias for `</>`.*/
    def getOrElse[Aʹ >: A](x: Aʹ): Parsley[Aʹ] = this </> x
    /**
      * This combinator, pronounced "sum", is similar to `<|>`, except it allows the
      * types of either side of the combinator to vary by returning their result as
      * part of an `Either`.
      *
      * @param q The parser to run if the receiver failed without consuming input
      * @return the result of the parser which succeeded, if any
      */
    def <+>[B](q: Parsley[B]): Parsley[Either[A, B]] = this.map(Left(_)) <|> q.map(Right(_))
    /**
      * This is the parser that corresponds to a more optimal version of `lift2(_ => x => x, p, q)`. It performs
      * the parse action of both parsers, in order, but discards the result of the receiver.
      * @param q The parser whose result should be returned
      * @return A new parser which first parses `p`, then `q` and returns the result of `q`
      */
    def *>[B](q: =>Parsley[B]): Parsley[B] = new Parsley(new deepembedding.*>(this.internal, q.internal))
    /**
      * This is the parser that corresponds to a more optimal version of `lift2(x => _ => x, p, q)`. It performs
      * the parse action of both parsers, in order, but discards the result of the second parser.
      * @param q The parser who should be executed but then discarded
      * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
      */
    def <*[B](q: =>Parsley[B]): Parsley[A] = new Parsley(new deepembedding.<*(this.internal, q.internal))
    /**
      * This is the parser that corresponds to `p *> pure(x)` or a more optimal version of `p.map(_ => x)`.
      * It performs the parse action of the receiver but discards its result and then results the value `x` instead
      * @param x The value to be returned after the execution of the receiver
      * @return A new parser which first parses the receiver, then results `x`
      */
    def #>[B](x: B): Parsley[B] = this *> pure(x)
    /**
      * This is the parser that corresponds to a more optimal version of `(p <~> q).map(_._2)`. It performs
      * the parse action of both parsers, in order, but discards the result of the receiver.
      * @param q The parser whose result should be returned
      * @return A new parser which first parses `p`, then `q` and returns the result of `q`
      * @since 2.4.0
      */
    def ~>[B](q: =>Parsley[B]): Parsley[B] = this *> q
    /**
      * This is the parser that corresponds to a more optimal version of `(p <~> q).map(_._1)`. It performs
      * the parse action of both parsers, in order, but discards the result of the second parser.
      * @param q The parser who should be executed but then discarded
      * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
      * @since 2.4.0
      */
    def <~[B](q: =>Parsley[B]): Parsley[A] = this <* q
    /**This parser corresponds to `lift2(_+:_, p, ps)`.*/
    def <+:>[Aʹ >: A](ps: =>Parsley[Seq[Aʹ]]): Parsley[Seq[Aʹ]] = lift.lift2[A, Seq[Aʹ], Seq[Aʹ]](_ +: _, this, ps)
    /**This parser corresponds to `lift2(_::_, p, ps)`.*/
    def <::>[Aʹ >: A](ps: =>Parsley[List[Aʹ]]): Parsley[List[Aʹ]] = lift.lift2[A, List[Aʹ], List[Aʹ]](_ :: _, this, ps)
    /**This parser corresponds to `lift2((_, _), p, q)`. For now it is sugar, but in future may be more optimal*/
    def <~>[B](q: =>Parsley[B]): Parsley[(A, B)] = lift.lift2[A, B, (A, B)]((_, _), this, q)
    /** This combinator is an alias for `<~>`
      * @since 2.3.0
      */
    def zip[B](q: =>Parsley[B]): Parsley[(A, B)] = this <~> q
    /** Filter the value of a parser; if the value returned by the parser matches the predicate `pred` then the
      * filter succeeded, otherwise the parser fails with an empty error
      * @param pred The predicate that is tested against the parser result
      * @return The result of the receiver if it passes the predicate
      */
    def filter(pred: A => Boolean): Parsley[A] = new Parsley(new deepembedding.Filter(this.internal, pred))
    /** Filter the value of a parser; if the value returned by the parser does not match the predicate `pred` then the
      * filter succeeded, otherwise the parser fails with an empty error
      * @param pred The predicate that is tested against the parser result
      * @return The result of the receiver if it fails the predicate
      */
    def filterNot(pred: A => Boolean): Parsley[A] = this.filter(!pred(_))
    /** Attempts to first filter the parser to ensure that `pf` is defined over it. If it is, then the function `pf`
      * is mapped over its result. Roughly the same as a `filter` then a `map`.
      * @param pf The partial function
      * @return The result of applying `pf` to this parsers value (if possible), or fails
      * @since 2.0.0
      */
    def collect[B](pf: PartialFunction[A, B]): Parsley[B] = this.filter(pf.isDefinedAt).map(pf)
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
    def foldLeft[B](k: B)(f: (B, A) => B): Parsley[B] = new Parsley(new deepembedding.Chainl(pure(k).internal, this.internal, pure(f).internal))
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
      * @since 2.1.0
      */
    def foldRight1[B](k: B)(f: (A, B) => B): Parsley[B] = {
        lift.lift2(f, this, this.foldRight(k)(f))
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
      * @since 2.1.0
      */
    def foldLeft1[B](k: B)(f: (B, A) => B): Parsley[B] = {
        new Parsley(new deepembedding.Chainl(this.map(f(k, _)).internal, this.internal, pure(f).internal))
    }
    /**
      * A reduction for a parser: `p.reduceRight(op)` will try executing `p` many times until it fails, combining the
      * results with right-associative application of `op`. It must parse `p` at least once.
      *
      * @param op combining function
      * @return the result of reducing the results of `p` with `op`
      * @since 2.3.0
      */
    def reduceRight[B >: A](op: (A, B) => B): Parsley[B] = some(this).map(_.reduceRight(op))
    /**
      * A reduction for a parser: `p.reduceRightOption(op)` will try executing `p` many times until it fails, combining the
      * results with right-associative application of `op`. If there is no `p`, it returns `None`, otherwise it returns
      * `Some(x)` where `x` is the result of the reduction.
      *
      * @param op combining function
      * @return the result of reducing the results of `p` with `op` wrapped in `Some` or `None` otherwise
      * @since 2.3.0
      */
    def reduceRightOption[B >: A](op: (A, B) => B): Parsley[Option[B]] = option(this.reduceRight(op))
    /**
      * A reduction for a parser: `p.reduceLeft(op)` will try executing `p` many times until it fails, combining the
      * results with left-associative application of `op`. It must parse `p` at least once.
      *
      * @param op combining function
      * @return the result of reducing the results of `p` with `op`
      * @since 2.3.0
      */
    def reduceLeft[B >: A](op: (B, A) => B): Parsley[B] = infix.left1(this, pure(op))
    /**
      * A reduction for a parser: `p.reduceLeftOption(op)` will try executing `p` many times until it fails, combining the
      * results with left-associative application of `op`. If there is no `p`, it returns `None`, otherwise it returns
      * `Some(x)` where `x` is the result of the reduction.
      *
      * @param op combining function
      * @return the result of reducing the results of `p` with `op` wrapped in `Some` or `None` otherwise
      * @since 2.3.0
      */
    def reduceLeftOption[B >: A](op: (B, A) => B): Parsley[Option[B]] = option(this.reduceLeft(op))
    /**
      * This casts the result of the parser into a new type `B`. If the value returned by the parser
      * is castable to type `B`, then this cast is performed. Otherwise the parser fails.
      * @tparam B The type to attempt to cast into
      * @since 2.0.0
      */
    def cast[B: ClassTag]: Parsley[B] = this.collect {
        case x: B => x
    }
}

/** This object contains the core "function-style" combinators as well as the implicit classes which provide
  * the "method-style" combinators. All parsers will likely require something from within! */
object Parsley
{
    /**
      * This class exposes the `<#>` combinator on functions.
      *
      * @param f The function that is used for the map
      * @version 1.0.0
      */
    implicit final class LazyMapParsley[-A, +B](val f: A => B) extends AnyVal
    {
        /**This combinator is an alias for `map`*/
        def <#>(p: Parsley[A]): Parsley[B] = p.map(f)
    }
    /**
      * This class exposes a ternary operator on pairs of parsers.
      *
      * @param pq The parsers which serve the branches of the if
      * @param con A conversion (if required) to turn elements of `pq` into parsers
      * @version 1.0.0
      */
    implicit final class LazyChooseParsley[P, +A](pq: =>(P, P))(implicit con: P => Parsley[A])
    {
        private lazy val (p, q) = pq
        /**
          * This is an if statement lifted to the parser level. Formally, this is a selective functor operation,
          * equivalent to (branch b.map(boolToEither) (p.map(const)) (q.map(const))).
          * Note: due to Scala operator associativity laws, this is a right-associative operator, and must be properly
          * bracketed, technically the receiver is the rhs...
          * @param b The parser that yields the condition value
          * @return The result of either `p` or `q` depending on the return value of the receiver
          */
        def ?:(b: Parsley[Boolean]): Parsley[A] = new Parsley(new deepembedding.If(b.internal, con(p).internal, con(q).internal))
    }

    /** This is the traditional applicative `pure` function for parsers. It consumes no input and
      * does not influence the state of the parser, but does return the value provided. Useful to inject pure values
      * into the parsing process.
      * @param x The value to be returned from the parser
      * @return A parser which consumes nothing and returns `x`
      */
    def pure[A](x: A): Parsley[A] = new Parsley(new deepembedding.Pure(x))
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `p` and `q`
      * depending on the result from `b`. This can be used to implement conditional choice within a parser without
      * relying on expensive monadic operations.
      * @param b The first parser to parse
      * @param p If `b` returns `Left` then this parser is executed with the result
      * @param q If `b` returns `Right` then this parser is executed with the result
      * @return Either the result from `p` or `q` depending on `b`.
      */
    def branch[A, B, C](b: Parsley[Either[A, B]], p: =>Parsley[A => C], q: =>Parsley[B => C]): Parsley[C] = {
        new Parsley(new deepembedding.Branch(b.internal, p.internal, q.internal))
    }
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `q` depending on
      * whether or not `p` returns a `Left`. It can be used to implement `branch` and other selective operations, however
      * it is more efficiently implemented with `branch` itself.
      * @param p The first parser to parse
      * @param q If `p` returns `Left` then this parser is executed with the result
      * @return Either the result from `p` if it returned `Left` or the result of `q` applied to the `Right` from `p`
      */
    def select[A, B](p: Parsley[Either[A, B]], q: =>Parsley[A => B]): Parsley[B] = branch(p, q, pure(identity[B](_)))
    /**This function is an alias for `_.flatten`. Provides namesake to Haskell.*/
    def join[A](p: Parsley[Parsley[A]]): Parsley[A] = p.flatten
    /** Given a parser `p`, attempts to parse `p`. If the parser fails, then `attempt` ensures that no input was
      * consumed. This allows for backtracking capabilities, disabling the implicit cut semantics offered by `<|>`.
      * @param p The parser to run
      * @return The result of `p`, or if `p` failed ensures the parser state was as it was on entry.
      */
    def attempt[A](p: Parsley[A]): Parsley[A] = new Parsley(new deepembedding.Attempt(p.internal))
    /** Parses `p` without consuming any input. If `p` fails and consumes input then so does `lookAhead(p)`. Combine with
      * `attempt` if this is undesirable.
      * @param p The parser to look ahead at
      * @return The result of the lookahead
      */
    def lookAhead[A](p: Parsley[A]): Parsley[A] = new Parsley(new deepembedding.Look(p.internal))
    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new Parsley(new deepembedding.NotFollowedBy(p.internal))
    /** The `empty` parser consumes no input and fails softly (that is to say, no error message) */
    val empty: Parsley[Nothing] = new Parsley(deepembedding.Empty)
    /** Returns `()`. Defined as `pure(())` but aliased for sugar*/
    val unit: Parsley[Unit] = pure(())
    /** converts a parser's result to () */
    def void(p: Parsley[_]): Parsley[Unit] = p *> unit
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
}
