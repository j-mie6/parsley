package parsley

import parsley.internal.machine.Context
import parsley.internal.deepembedding.{singletons, frontend}
import parsley.expr.{chain, infix}
import parsley.combinator.{option, some, many}
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
  * @note In order to construct an object of this class you must use the combinators; the class itself is opaque.
  *
  * @author Jamie Willis
  * @version 4.0.0
  *
  * @groupprio special 100
  * @groupname special Special Methods
  * @groupdesc special These are methods that should be rarely needed.
  *
  * @groupprio run 0
  * @groupname run Running Parsers
  * @groupdesc run These methods allow for a parser to be executed.
  *
  * @groupprio fold 50
  * @groupname fold Folding Combinators
  * @groupdesc fold
  *     These combinators repeatedly execute a parser (at least zero or one times depending on
  *     the specific combinator) until it fails. The results of the successes are then combined
  *     together using a folding function. An initial value for the accumulation may be given
  *     (for the `fold`s), or the first successful result is the initial accumulator (for the
  *     `reduce`s). These are implemented efficiently and do not need to construct any intermediate
  *     list with which to store the results.
  *
  * @groupprio map 20
  * @groupname map Result Changing Combinators
  * @groupdesc map
  *     These combinators change the result of the parser they are called on into a value
  *     of a different type. This new result value may or may not be derived from the previous
  *     result.
  *
  * @groupprio alt 25
  * @groupname alt Branching Combinators
  * @groupdesc alt
  *     These combinators allow for parsing one alternative or another. All of these combinators
  *     are ''left-biased'', which means that the left-hand side of the combinator is tried first:
  *     the right-hand side of the combinator will only be tried when the left-hand one failed
  *     (and did not consume input in the process).
  *
  * @groupprio seq 25
  * @groupname seq Sequencing Combinators
  * @groupdesc seq
  *     These combinators all combine two parsers in sequence. The receiver of the combinator
  *     will be executed first, then the argument second. The results of both parsers are combined
  *     in some way (depending on the individual combinator). If one of the parsers fails, the
  *     combinator as a whole fails.
  *
  * @groupprio monad 75
  * @groupname monad Expensive Sequencing Combinators
  * @groupdesc monad
  *     These combinators can sequence two parsers, where the first parser's result influences
  *     the structure of the second one. This may be because the second parser is generated
  *     from the result of the first, or that the first parser ''returns'' the second parser.
  *     Either way, the second parser cannot be known until runtime, when the first parser
  *     has been executed: this means that Parsley is forced to compile the second parser during
  *     parse-time, which is '''very''' expensive to do repeatedly. These combinators are only
  *     needed in exceptional circumstances, and should be avoided otherwise.
  *
  * @groupprio filter 50
  * @groupname filter Filtering Combinators
  * @groupdesc filter
  *     These combinators perform filtering on the results of a parser. This means that, given
  *     the result of a parser, they will perform some function on that result, and the success
  *     of that function effects whether or not the parser fails.
  *
  * @define or
  *    This is the traditional choice operator for parsers: try to parse the left-hand side, and parses
  *    `q` if it fails without consuming input.
  *
  *    Following the parsec semantics precisely, this combinator first tries to parse the receiver. If this
  *    is successful, no further action is taken. If the receiver failed ''without'' consuming input,
  *    then `q` is parsed instead. If the receiver did consume any input then the whole parser fails.
  *    This is done to prevent space leaks and to give good error messages. If this behaviour
  *    is not desired, use `attempt(this)` as the receiver instead to parse `q` regardless of how much
  *    input was consumed by the reciever.
  */
final class Parsley[+A] private [parsley] (private [parsley] val internal: frontend.LazyParsley[A]) extends AnyVal
{
    /**
      * Using this method signifies that the parser it is invoked on is impure and any optimisations which assume purity
      * are disabled.
      *
      * @group special
      */
    def unsafe(): Unit = internal.unsafe()

    /**
      * Forces the compilation of a parser as opposed to the regular lazy evaluation.
      *
      * @group special
      */
    def force(): Unit = internal.force()

    // $COVERAGE-OFF$
    /**
      * Provides an indicator that this parser is likely to stack-overflow
      *
      * @group special
      */
    def overflows(): Unit = internal.overflows()
    // $COVERAGE-ON$

    /** This method is responsible for actually executing parsers. Given an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param input The input to run against
      * @return Either a success with a value of type `A` or a failure with error message
      * @since 3.0.0
      * @group run
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
      * @group map
      */
    // TODO: improve
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
      * @group seq
      */
    // TODO: improve
    def <*>[B, C](px: =>Parsley[B])
                 (implicit ev: Parsley[A] <:< Parsley[B=>C]): Parsley[C] = new Parsley(new frontend.<*>[B, C](ev(this).internal, px.internal))

    /**
      * This is the traditional Monadic binding operator for parsers. When the receiver produces a value, the function
      * `f` is used to produce a new parser that continued the computation.
      *
      * @note There is significant overhead for using flatMap; if possible try to write parsers in an applicative
      * style otherwise try and use the intrinsic parsers provided to replace the flatMap.
      * @param f A function that produces the next parser
      * @return The parser produces from the application of `f` on the result of the last parser
      * @group monad
      */
    // TODO: improve
    def flatMap[B](f: A => Parsley[B]): Parsley[B] = new Parsley(new frontend.>>=(this.internal, f.andThen(_.internal)))
    /** This combinator is an alias for `flatMap(identity)`.
      *
      * @group monad
      */
    // TODO: improve
    def flatten[B](implicit ev: A <:< Parsley[B]): Parsley[B] = this.flatMap[B](ev)
    /** This combinator is an alias for `flatMap`
      *
      * @group monad
      */
    // TODO: improve
    def >>=[B](f: A => Parsley[B]): Parsley[B] = this.flatMap(f)
    /** This combinator is defined as `lift2((x, f) => f(x), p, f)`. It is pure syntactic sugar.
      *
      * @group seq
      */
    // TODO: improve
    def <**>[B](pf: =>Parsley[A => B]): Parsley[B] = lift.lift2[A, A=>B, B]((x, f) => f(x), this, pf)

    /** $or
      *
      * @param q The parser to run if the receiver failed without consuming input
      * @group alt
      */
    def <|>[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = new Parsley(new frontend.<|>(this.internal, q.internal))
    /** $or
      *
      * @since 4.0.0
      * @param q The parser to run if the receiver failed without consuming input
      * @note Just an alias for `<|>`.
      * @group alt
      */
    def |[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = this <|> q
    /** This combinator is defined as `this <|> pure(x)`: it is pure syntactic sugar.
      *
      * @group alt
      */
    def </>[Aʹ >: A](x: Aʹ): Parsley[Aʹ] = this <|> pure(x)
    /** $or
      *
      * @since 4.0.0
      * @param q The parser to run if the receiver failed without consuming input
      * @note Just an alias for `<|>`.
      * @group alt
      */
    def orElse[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = this <|> q
    /** This combinator is defined as `this <|> pure(x)`: it is pure syntactic sugar.
      *
      * @group alt
      * @note Just an alias for `</>`
      */
    def getOrElse[Aʹ >: A](x: Aʹ): Parsley[Aʹ] = this </> x
    /**
      * This combinator, pronounced "sum", is similar to `<|>`, except it allows the
      * types of either side of the combinator to vary by returning their result as
      * part of an `Either`.
      *
      * @param q The parser to run if the receiver failed without consuming input
      * @group alt
      */
    def <+>[B](q: Parsley[B]): Parsley[Either[A, B]] = this.map(Left(_)) <|> q.map(Right(_))

    /**
      * This is the parser that corresponds to a more optimal version of `lift2(_ => x => x, p, q)`. It performs
      * the parse action of both parsers, in order, but discards the result of the receiver.
      * @param q The parser whose result should be returned
      * @return A new parser which first parses `p`, then `q` and returns the result of `q`
      * @group seq
      */
    // TODO: improve
    def *>[B](q: =>Parsley[B]): Parsley[B] = new Parsley(new frontend.*>(this.internal, q.internal))
    /**
      * This is the parser that corresponds to a more optimal version of `lift2(x => _ => x, p, q)`. It performs
      * the parse action of both parsers, in order, but discards the result of the second parser.
      * @param q The parser who should be executed but then discarded
      * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
      * @group seq
      */
    // TODO: improve
    def <*[B](q: =>Parsley[B]): Parsley[A] = new Parsley(new frontend.<*(this.internal, q.internal))
    /**
      * This is the parser that corresponds to `p *> pure(x)` or a more optimal version of `p.map(_ => x)`.
      * It performs the parse action of the receiver but discards its result and then results the value `x` instead
      * @param x The value to be returned after the execution of the receiver
      * @return A new parser which first parses the receiver, then results `x`
      * @group map
      */
    // TODO: improve
    def #>[B](x: B): Parsley[B] = this *> pure(x)
    /** converts the parser's result to ().
      *
      * @group map
      */
    def void: Parsley[Unit] = this #> ()
    /**
      * This is the parser that corresponds to a more optimal version of `(p <~> q).map(_._2)`. It performs
      * the parse action of both parsers, in order, but discards the result of the receiver.
      * @param q The parser whose result should be returned
      * @return A new parser which first parses `p`, then `q` and returns the result of `q`
      * @since 2.4.0
      * @group seq
      */
    // TODO: improve
    def ~>[B](q: =>Parsley[B]): Parsley[B] = this *> q
    /**
      * This is the parser that corresponds to a more optimal version of `(p <~> q).map(_._1)`. It performs
      * the parse action of both parsers, in order, but discards the result of the second parser.
      * @param q The parser who should be executed but then discarded
      * @return A new parser which first parses `p`, then `q` and returns the result of the `p`
      * @since 2.4.0
      * @group seq
      */
    // TODO: improve
    def <~[B](q: =>Parsley[B]): Parsley[A] = this <* q
    /** This parser corresponds to `lift2(_+:_, p, ps)`.
      *
      * @group seq
      */
    // TODO: improve
    def <+:>[Aʹ >: A](ps: =>Parsley[Seq[Aʹ]]): Parsley[Seq[Aʹ]] = lift.lift2[A, Seq[Aʹ], Seq[Aʹ]](_ +: _, this, ps)
    /** This parser corresponds to `lift2(_::_, p, ps)`.
      *
      * @group seq
      */
    // TODO: improve
    def <::>[Aʹ >: A](ps: =>Parsley[List[Aʹ]]): Parsley[List[Aʹ]] = lift.lift2[A, List[Aʹ], List[Aʹ]](_ :: _, this, ps)
    /** This parser corresponds to `lift2((_, _), p, q)`. For now it is sugar, but in future may be more optimal
      *
      * @group seq
      */
    // TODO: improve
    def <~>[B](q: =>Parsley[B]): Parsley[(A, B)] = lift.lift2[A, B, (A, B)]((_, _), this, q)
    /** This combinator is an alias for `<~>`
      * @since 2.3.0
      * @group seq
      */
    // TODO: improve
    def zip[B](q: =>Parsley[B]): Parsley[(A, B)] = this <~> q
    /** Filter the value of a parser; if the value returned by the parser matches the predicate `pred` then the
      * filter succeeded, otherwise the parser fails with an empty error
      * @param pred The predicate that is tested against the parser result
      * @return The result of the receiver if it passes the predicate
      * @group filter
      */
    // TODO: improve
    def filter(pred: A => Boolean): Parsley[A] = new Parsley(new frontend.Filter(this.internal, pred))
    /** Filter the value of a parser; if the value returned by the parser does not match the predicate `pred` then the
      * filter succeeded, otherwise the parser fails with an empty error
      * @param pred The predicate that is tested against the parser result
      * @return The result of the receiver if it fails the predicate
      * @group filter
      */
    // TODO: improve
    def filterNot(pred: A => Boolean): Parsley[A] = this.filter(!pred(_))
    /** Attempts to first filter the parser to ensure that `pf` is defined over it. If it is, then the function `pf`
      * is mapped over its result. Roughly the same as a `filter` then a `map`.
      * @param pf The partial function
      * @return The result of applying `pf` to this parsers value (if possible), or fails
      * @since 2.0.0
      * @group filter
      */
    // TODO: improve
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
      * @group fold
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
      * @group fold
      */
    def foldLeft[B](k: B)(f: (B, A) => B): Parsley[B] = new Parsley(new frontend.Chainl(pure(k).internal, this.internal, pure(f).internal))
    /**
      * A fold for a parser: `p.foldRight1(k)(f)` will try executing `p` many times until it fails, combining the
      * results with right-associative application of `f` with a `k` at the right-most position: it must parse `p`
      * at least once.
      *
      * @example {{{p.foldRight1(Nil)(_::_) == some(p) //some is more efficient, however}}}
      *
      * @param k base case for iteration
      * @param f combining function
      * @return the result of folding the results of `p` with `f` and `k`
      * @since 2.1.0
      * @group fold
      */
    def foldRight1[B](k: B)(f: (A, B) => B): Parsley[B] = {
        lift.lift2(f, this, this.foldRight(k)(f))
    }
    /**
      * A fold for a parser: `p.foldLeft1(k)(f)` will try executing `p` many times until it fails, combining the
      * results with left-associative application of `f` with a `k` on the left-most position: it must parse `p`
      * at least once.
      *
      * @example {{{val natural: Parsley[Int] = digit.foldLeft1(0)((x, d) => x * 10 + d.toInt)}}}
      *
      * @param k base case for iteration
      * @param f combining function
      * @return the result of folding the results of `p` with `f` and `k`
      * @since 2.1.0
      * @group fold
      */
    def foldLeft1[B](k: B)(f: (B, A) => B): Parsley[B] = {
        new Parsley(new frontend.Chainl(this.map(f(k, _)).internal, this.internal, pure(f).internal))
    }
    /**
      * A reduction for a parser: `p.reduceRight(op)` will try executing `p` many times until it fails, combining the
      * results with right-associative application of `op`: it must parse `p` at least once.
      *
      * @param op combining function
      * @return the result of reducing the results of `p` with `op`
      * @since 2.3.0
      * @group fold
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
      * @group fold
      */
    def reduceRightOption[B >: A](op: (A, B) => B): Parsley[Option[B]] = option(this.reduceRight(op))
    /**
      * A reduction for a parser: `p.reduceLeft(op)` will try executing `p` many times until it fails, combining the
      * results with left-associative application of `op`: it must parse `p` at least once.
      *
      * @param op combining function
      * @return the result of reducing the results of `p` with `op`
      * @since 2.3.0
      * @group fold
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
      * @group fold
      */
    def reduceLeftOption[B >: A](op: (B, A) => B): Parsley[Option[B]] = option(this.reduceLeft(op))
    /**
      * This casts the result of the parser into a new type `B`: if the value returned by the parser
      * is castable to type `B`, then this cast is performed; otherwise, the parser fails.
      * @tparam B The type to attempt to cast into
      * @since 2.0.0
      * @group filter
      */
    def cast[B: ClassTag]: Parsley[B] = this.collect {
        case x: B => x
    }
}

/** This object contains the core "function-style" combinators: all parsers will likely require something from within!
  *
  * @groupprio cond 25
  * @groupname cond Conditional Combinators
  * @groupdesc cond
  *     These combinators will decide which branch to take next based on the result of another parser.
  *     This differs from combinators like `<|>` which make decisions based on the success/failure of
  *     a parser: here the result of a ''successful'' parse will direct which option is done. These
  *     are sometimes known as "selective" combinators.
  *
  * @groupprio prim 0
  * @groupname prim Primitive Combinators
  * @groupdesc prim
  *     These combinators are specific to parser combinators. In one way or another, they influence how a
  *     parser consumes input, or under what conditions a parser does or does not fail. These are really
  *     important for most practical parsing considerations, although `lookAhead` is much less well used.
  *
  * @groupprio basic 5
  * @groupname basic Consumptionless Parsers
  * @groupdesc basic
  *     These combinators and parsers do not consume input: they are the most primitive ways of producing
  *     successes and failures with the minimal possible effect on the parse. They are, however, reasonably
  *     useful; in particular, `pure` and `unit` can be put to good use in injecting results into a parser
  *     without needing to consume anything, or mapping another parser.
  *
  * @groupprio pos 10
  * @groupname pos Position-Tracking Parsers
  * @groupdesc pos
  *     These parsers provide a way to extract position information during a parse. This can be important
  *     for when the final result of the parser needs to encode position information for later consumption:
  *     this is particularly useful for abstract syntax trees.
  *
  * @groupprio seq 50
  * @groupname seq Sequencing Combinators
  * @groupdesc seq
  *     These combinators sequence a large number of parsers in one go. Be careful, however, these are
  *     variadic combinators and are necessarily (for compatibility with Scala 2) '''not lazy'''.
  *
  *     In such a case where laziness is desired without resorting to the other lazier combinators, there
  *     is a neat trick: unroll the first iteration of the combinator, and use the regular combinator
  *     to do that (probably `<::>` or `*>`): since these will have a lazy right-hand side, the remaining
  *     variadic arguments will be kept lazily suspended until later.
  *
  * @groupprio monad 100
  * @groupname monad Expensive Sequencing Combinators
  * @groupdesc monad
  *     These combinators can sequence two parsers, where the first parser's result influences
  *     the structure of the second one. This may be because the second parser is generated
  *     from the result of the first, or that the first parser ''returns'' the second parser.
  *     Either way, the second parser cannot be known until runtime, when the first parser
  *     has been executed: this means that Parsley is forced to compile the second parser during
  *     parse-time, which is '''very''' expensive to do repeatedly. These combinators are only
  *     needed in exceptional circumstances, and should be avoided otherwise.
  */
object Parsley
{
    /** This is the traditional applicative `pure` function for parsers. It consumes no input and
      * does not influence the state of the parser, but does return the value provided. Useful to inject pure values
      * into the parsing process.
      * @param x The value to be returned from the parser
      * @return A parser which consumes nothing and returns `x`
      * @group basic
      */
    def pure[A](x: A): Parsley[A] = new Parsley(new singletons.Pure(x))

    /** This is one of the core operations of a selective functor. It will conditionally execute one of `p` and `q`
      * depending on the result from `b`. This can be used to implement conditional choice within a parser without
      * relying on expensive monadic operations.
      * @param b The first parser to parse
      * @param p If `b` returns `Left` then this parser is executed with the result
      * @param q If `b` returns `Right` then this parser is executed with the result
      * @return Either the result from `p` or `q` depending on `b`.
      * @group cond
      */
    def branch[A, B, C](b: Parsley[Either[A, B]], p: =>Parsley[A => C], q: =>Parsley[B => C]): Parsley[C] = {
        new Parsley(new frontend.Branch(b.internal, p.internal, q.internal))
    }
    /**
      * This is an if statement lifted to the parser level. Formally, this is a selective functor operation,
      * equivalent to `branch(b.map(boolToEither), p.map(const), q.map(const))`.
      *
      * @param b The parser that yields the condition value
      * @param p
      * @param q
      * @group cond
      */
    def ite[A](b: Parsley[Boolean], p: =>Parsley[A], q: =>Parsley[A]): Parsley[A] = {
        new Parsley(new frontend.If(b.internal, p.internal, q.internal))
    }
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `q` depending on
      * whether or not `p` returns a `Left`. It can be used to implement `branch` and other selective operations, however
      * it is more efficiently implemented with `branch` itself.
      * @param p The first parser to parse
      * @param q If `p` returns `Left` then this parser is executed with the result
      * @return Either the result from `p` if it returned `Left` or the result of `q` applied to the `Right` from `p`
      * @group cond
      */
    def select[A, B](p: Parsley[Either[A, B]], q: =>Parsley[A => B]): Parsley[B] = branch(p, q, pure(identity[B](_)))
    /** This function is an alias for `_.flatten`: provides namesake to Haskell.
      *
      * @group monad
      */
    def join[A](p: Parsley[Parsley[A]]): Parsley[A] = p.flatten
    /** Given a parser `p`, attempts to parse `p`. If the parser fails, then `attempt` ensures that no input was
      * consumed. This allows for backtracking capabilities, disabling the implicit cut semantics offered by `<|>`.
      *
      * @param p The parser to run
      * @return The result of `p`, or if `p` failed ensures the parser state was as it was on entry.
      * @group prim
      */
    def attempt[A](p: Parsley[A]): Parsley[A] = new Parsley(new frontend.Attempt(p.internal))
    /** Parses `p` without consuming any input. If `p` fails and consumes input then so does `lookAhead(p)`. Combine with
      * `attempt` if this is undesirable.
      *
      * @param p The parser to look ahead at
      * @return The result of the lookahead
      * @group prim
      */
    def lookAhead[A](p: Parsley[A]): Parsley[A] = new Parsley(new frontend.Look(p.internal))
    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}
      *
      * @group prim
      */
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new Parsley(new frontend.NotFollowedBy(p.internal))
    /**
      * The `empty` parser consumes no input and fails softly (that is to say, no error message)
      *
      * @group basic
      */
    val empty: Parsley[Nothing] = new Parsley(singletons.Empty)
    /**
      * Returns `()`; defined as `pure(())` but aliased for sugar
      *
      * @group basic
      */
    val unit: Parsley[Unit] = pure(())
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, collecting the results.
      * @param ps Parsers to be sequenced
      * @return The list containing results, one from each parser, in order
      * @group seq
      */
    def sequence[A](ps: Parsley[A]*): Parsley[List[A]] = ps.foldRight(pure[List[A]](Nil))(_ <::> _)
    /**
      * Like `sequence` but produces a list of parsers to sequence by applying the function `f` to each
      * element in `xs`.
      * @param f The function to map on each element of `xs` to produce parsers
      * @param xs Values to generate parsers from
      * @return The list containing results formed by executing each parser generated from `xs` and `f` in sequence
      * @group seq
      */
    def traverse[A, B](f: A => Parsley[B], xs: A*): Parsley[List[B]] = sequence(xs.map(f): _*)
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, ignoring the results.
      * @param ps Parsers to be performed
      * @group seq
      */
    def skip(ps: Parsley[_]*): Parsley[Unit] = ps.foldRight(unit)(_ *> _)
    /**
      * This parser consumes no input and returns the current line number reached in the input stream
      * @return The line number the parser is currently at
      * @group pos
      */
    val line: Parsley[Int] = new Parsley(singletons.Line)
    /**
      * This parser consumes no input and returns the current column number reached in the input stream
      * @return The column number the parser is currently at
      * @group pos
      */
    val col: Parsley[Int] = new Parsley(singletons.Col)
    /**
      * This parser consumes no input and returns the current position reached in the input stream
      * @return Tuple of line and column number that the parser has reached
      * @group pos
      */
    val pos: Parsley[(Int, Int)] = line <~> col
}
