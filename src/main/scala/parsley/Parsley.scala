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

/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `parse` will
  * parse the string given as input to `parse`.
  *
  * @note In order to construct an object of this class you must use the combinators; the class itself is opaque.
  *
  * @author Jamie Willis
  * @version 4.0.0
  *
  * @groupprio run 0
  * @groupname run Running Parsers
  * @groupdesc run These methods allow for a parser to be executed.
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
  * @groupprio filter 50
  * @groupname filter Filtering Combinators
  * @groupdesc filter
  *     These combinators perform filtering on the results of a parser. This means that, given
  *     the result of a parser, they will perform some function on that result, and the success
  *     of that function effects whether or not the parser fails.
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
  * @groupprio special 100
  * @groupname special Special Methods
  * @groupdesc special These are methods that should be rarely needed.
  *
  * @define attemptreason
  *     The reason for this behaviour is that it prevents space leaks and improves error messages. If this behaviour
  *     is not desired, use `attempt(this)` to rollback any input consumed on failure.
  *
  * @define or
  *     parses `q` if this parser fails '''without''' consuming input.
  *
  *     If this parser is successful, then this combinator is successful and no further action is taken. Otherwise, if this parser
  *     fails '''without''' consuming input, then `q` is parsed instead. If this parser fails having consumed
  *     input, this combinator fails.
  *
  * @define orconst
  *     returns `x` if this parser fails '''without''' consuming input.
  *
  *     If this parser is successful, then this combinator is successful and no further action is taken. Otherwise, if this parser
  *     fails '''without''' consuming input, then `x` is unconditionally returned. If this parser fails having consumed
  *     input, this combinator fails. Functionally the same as `this <|> pure(x)`.
  */
final class Parsley[+A] private [parsley] (private [parsley] val internal: frontend.LazyParsley[A]) extends AnyVal
{
    /** This method is responsible for actually executing parsers. Given an input
      * array, will parse the string with the parser. The result is either a `Success` or a `Failure`.
      * @param input The input to run against
      * @return Either a success with a value of type `A` or a failure with error message
      * @since 3.0.0
      * @group run
      */
    def parse[Err: ErrorBuilder](input: String): Result[Err, A] = new Context(internal.threadSafeInstrs, input).runParser()

    // RESULT CHANGING COMBINATORS
    /**
      * This combinator allows the result of this parser to be changed using a given function.
      *
      * When this parser succeeds, `map(f)` will adjust its result using the function `f`, which can
      * potentially change its type. This can be used to build more complex results from parsers, instead
      * of just characters or strings.
      *
      * ''In Haskell, this combinator is known as `fmap` or `(<$>)`''.
      *
      * @example {{{
      * scala> import parsley.character.digit
      * scala> digit.map(_.asDigit).parse("7")
      * val res0 = Success(7)
      * }}}
      *
      * @note This is subject to aggressive optimisations assuming purity; the compiler is permitted to optimise such
      * that the application of `f` actually only happens once at compile time. In order to preserve the behaviour of
      * impure functions, consider using the `unsafe` method before map; `p.unsafe.map(f)`.
      * @param f the function to apply to the result of the parse
      * @return a new parser that behaves the same as this parser, but with the given function `f` applied to its result.
      * @group map
      */
    def map[B](f: A => B): Parsley[B] = pure(f) <*> this
        /**
      * This combinator, pronounced "as", replaces the result of this parser, ignoring the old result.
      *
      * Similar to `map`, except the old result of this parser is not required to
      * compute the new result. This is useful when the result is a constant value (or function!).
      * Functionally the same as `this *> pure(x)` or `this.map(_ => x)`.
      *
      * ''In Haskell, this combinator is known as `($>)`''.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> (string("true") #> true).parse("true")
      * val res0 = Success(true)
      * }}}
      *
      * @param x the new result of this parser.
      * @return a new parser that behaves the same as this parser, but always succeeds with `x` as the result.
      * @group map
      */
    def #>[B](x: B): Parsley[B] = this *> pure(x)
    /** Replaces the result of this parser with `()`.
      *
      * This combinator is useful when the result of this parser is not required, and the
      * type must be `Parsley[Unit]`. Functionally the same as `this #> ()`.
      *
      * @return a new parser that behaves the same as this parser, but always returns `()` on success.
      * @group map
      */
    def void: Parsley[Unit] = this #> ()

    // BRANCHING COMBINATORS
    /** This combinator, pronounced "or", $or
      *
      * $attemptreason
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> val p = string("a") <|> string("b")
      * scala> p.parse("a")
      * val res0 = Success("a")
      * scala> p.parse("b")
      * val res1 = Success("b")
      * scala> val q = string("ab") <|> string("ac")
      * scala> q.parse("ac")
      * val res2 = Failure(..) // first parser consumed an 'a'!
      * }}}
      *
      * @param q the parser to run if this parser fails having not consumed input.
      * @tparam Aʹ the type returned by `q`, which must be a supertype of the result type of this parser: this allows for weakening of the result type.
      * @return a parser which either parses this parser or parses `q`.
      * @group alt
      */
    def <|>[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = new Parsley(new frontend.<|>(this.internal, q.internal))
    /** This combinator, pronounced "or", $or
      *
      * $attemptreason
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> val p = string("a") | string("b")
      * scala> p.parse("a")
      * val res0 = Success("a")
      * scala> p.parse("b")
      * val res1 = Success("b")
      * scala> val q = string("ab") | string("ac")
      * scala> q.parse("ac")
      * val res2 = Failure(..) // first parser consumed an 'a'!
      * }}}
      *
      * @since 4.0.0
      * @param q the parser to run if this parser fails having not consumed input.
      * @tparam Aʹ the type returned by `q`, which must be a supertype of the result type of this parser: this allows for weakening of the result type.
      * @return a parser which either parses this parser or parses `q`.
      * @note just an alias for `<|>`.
      * @group alt
      */
    def |[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = this <|> q
    /** This combinator $or
      *
      * $attemptreason
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> val p = string("a").orElse(string("b"))
      * scala> p.parse("a")
      * val res0 = Success("a")
      * scala> p.parse("b")
      * val res1 = Success("b")
      * scala> val q = string("ab").orElse(string("ac"))
      * scala> q.parse("ac")
      * val res2 = Failure(..) // first parser consumed an 'a'!
      * }}}
      *
      * @since 4.0.0
      * @param q the parser to run if this parser fails having not consumed input.
      * @tparam Aʹ the type returned by `q`, which must be a supertype of the result type of this parser: this allows for weakening of the result type.
      * @return a parser which either parses this parser or parses `q`.
      * @note just an alias for `<|>`.
      * @group alt
      */
    def orElse[Aʹ >: A](q: Parsley[Aʹ]): Parsley[Aʹ] = this <|> q
    /** This combinator, pronounced "or constant", $orconst
      *
      * $attemptreason
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> val p = string("aa") </> "b"
      * scala> p.parse("aa")
      * val res0 = Success("a")
      * scala> p.parse("xyz")
      * val res1 = Success("b")
      * scala> p.parse("ab")
      * val res2 = Failure(..) // first parser consumed an 'a'!
      * }}}
      *
      * @param x the value to return if this parser fails having not consumed input.
      * @tparam Aʹ the type of `x`, which must be a supertype of the result type of this parser: this allows for weakening of the result type.
      * @return a parser which either parses this parser or returns `x`.
      * @group alt
      */
    def </>[Aʹ >: A](x: Aʹ): Parsley[Aʹ] = this <|> pure(x)
    /** This combinator $orconst
      *
      * $attemptreason
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> val p = string("aa").getOrElse("b")
      * scala> p.parse("aa")
      * val res0 = Success("a")
      * scala> p.parse("xyz")
      * val res1 = Success("b")
      * scala> p.parse("ab")
      * val res2 = Failure(..) // first parser consumed an 'a'!
      * }}}
      *
      * @group alt
      * @note just an alias for `</>`
      */
    def getOrElse[Aʹ >: A](x: Aʹ): Parsley[Aʹ] = this </> x
    /**
      * This combinator, pronounced "sum", wraps this parser's result in `Left` if it succeeds, and parses `q` if it failed '''without''' consuming input,
      * wrapping the result in `Right`.
      *
      * If this parser is successful, then its result is wrapped up using `Left(_)` and no further action is taken.
      * Otherwise, if this parser fails '''without''' consuming input, then `q` is parsed instead and its result is
      * wrapped up using `Right(_)`. If this parser fails having consumed input, this combinator fails.
      * This is functionally equivalent to `this.map(Left(_)) <|> q.map(Right(_))`.
      *
      * $attemptreason
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> val p = string("abc") <+> char("xyz")
      * scala> p.parse("abc")
      * val res0 = Success(Left("abc"))
      * scala> p.parse("xyz")
      * val res1 = Success(Right("xyz"))
      * scala> p.parse("ab")
      * val res2 = Failure(..) // first parser consumed an 'a'!
      * }}}
      *
      * @param q the parser to run if this parser fails having not consumed input.
      * @return a parser which either parses this parser or parses `q` projecting their results into an `Either[A, B]`.
      * @group alt
      */
    def <+>[B](q: Parsley[B]): Parsley[Either[A, B]] = this.map(Left(_)) <|> q.map(Right(_))

    // SEQUENCING COMBINATORS
    /** This combinator, pronounced "ap", first parses this parser then parses `px`: if both succeed then the function
      * returned by this parser is applied to the value returned by `px`.
      *
      * The implicit (compiler-provided) evidence proves that this parser really has type `Parsley[B => C]`.
      * First, this parser is ran, yielding a function `f` on success, then `px` is ran, yielding a value `x`
      * on success. If both are successful, then `f(x)` is returned. If either fail then the entire combinator
      * fails.
      *
      * @example {{{
      * scala> import parsley.Parsley, parsley.character.char
      * scala> val sign: Parsley[Int => Int] = char('+') #> (identity[Int] _) <|> char('-') #> (x => -x)
      * scala> val nat: Parsley[Int] = ..
      * scala> val int = sign <*> nat
      * scala> int.parse("-7")
      * val res0 = Success(-7)
      * scala> int.parse("+42")
      * val res1 = Success(42)
      * scala> int.parse("2")
      * val res2 = Failure(..) // `sign` failed: no + or -
      * scala> int.parse("-a")
      * val res3 = Failure(..) // `nat` failed
      * }}}
      *
      * @param px the parser to run second, which returns a value applicable to this parser's result.
      * @param ev witnesses that the type of this parser, `A`, is actually `B => C`.
      * @return a parser that sequences this parser with `px` and combines their results with function application.
      * @note equivalent to `lift2((f, x) => f(x), this, px)`.
      * @group seq
      */
    def <*>[B, C](px: =>Parsley[B])
                 (implicit ev: A <:< (B=>C)): Parsley[C] = new Parsley(new frontend.<*>[B, C](ev.substituteCo(this).internal, px.internal))
    /** This combinator, pronounced "reverse ap", first parses this parser then parses `pf`: if both succeed then the value
      * returned by this parser is applied to the function returned by `pf`.
      *
      * First, this parser is ran, yielding a value `x` on success, then `pf` is ran, yielding a function `f`
      * on success. If both are successful, then `f(x)` is returned. If either fail then the entire combinator
      * fails.
      *
      * Compared with `<*>`, this combinator is useful for left-factoring: when two branches of a parser share
      * a common prefix, this can often be factored out; but the result of that factored prefix may be required
      * to help generate the results of each branch. In this case, the branches can return functions that, when
      * given the factored result, can produce the original results from before the factoring.
      *
      * @example {{{
    *  // this has a common prefix "term" and requires backtracking
      * val expr1 = attempt(lift2(Add, term <* char('+'), expr2)) <|> term
      * // common prefix factored out, and branches return a function to recombine
      * val expr2 = term <**> (char('+') *> expr2.map(y => Add(_, y)) </> (identity[Expr] _))
      * }}}
      *
      * @param pf the parser to run second, which returns a function this parser's result can be applied to.
      * @return a parser that sequences this parser with `pf` and combines their results with function application.
      * @note equivalent to `lift2((x, f) => f(x), this, pf)`.
      * @group seq
      */
    def <**>[B](pf: =>Parsley[A => B]): Parsley[B] = lift.lift2[A, A=>B, B]((x, f) => f(x), this, pf)
    /** This combinator, pronounced "then", first parses this parser then parses `q`: if both succeed then the result
      * of `q` is returned.
      *
      * First, this parser is ran, yielding `x` on success, then `q` is ran, yielding `y` on success. If both
      * are successful then `y` is returned and `x` is ignored. If either fail then the entire combinator fails.
      *
      * ''Identical to `~>`: `*>` is more common in Haskell, wheras `~>` is more common in Scala.''
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> ('a' *> 'b').parse("ab")
      * val res0 = Success('b')
      * }}}
      *
      * @param q the parser to run second, which returns the result of this combinator.
      * @return a parser that sequences this parser with `q` and returns `q`'s result.
      * @group seq
      */
    def *>[B](q: =>Parsley[B]): Parsley[B] = new Parsley(new frontend.*>(this.internal, q.internal))
    /** This combinator, pronounced "then discard", first parses this parser then parses `q`: if both succeed then the result
      * of this parser is returned.
      *
      * First, this parser is ran, yielding `x` on success, then `q` is ran, yielding `y` on success. If both
      * are successful then `x` is returned and `y` is ignored. If either fail then the entire combinator fails.
      *
      * ''Identical to `<~`: `<*` is more common in Haskell, wheras `<~` is more common in Scala.''
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> ('a' <* 'b').parse("ab")
      * val res0 = Success('a')
      * }}}
      *
      * @param q the parser to run second, which returns the result of this combinator.
      * @return a parser that sequences this parser with `q` and returns this parser's result.
      * @group seq
      */
    def <*[B](q: =>Parsley[B]): Parsley[A] = new Parsley(new frontend.<*(this.internal, q.internal))
    /** This combinator, pronounced "then", first parses this parser then parses `q`: if both succeed then the result
      * of `q` is returned.
      *
      * First, this parser is ran, yielding `x` on success, then `q` is ran, yielding `y` on success. If both
      * are successful then `y` is returned and `x` is ignored. If either fail then the entire combinator fails.
      *
      * ''Identical to `*>`: `*>` is more common in Haskell, wheras `~>` is more common in Scala.''
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> ('a' ~> 'b').parse("ab")
      * val res0 = Success('b')
      * }}}
      *
      * @param q the parser to run second, which returns the result of this combinator.
      * @return a parser that sequences this parser with `q` and returns `q`'s result.
      * @since 2.4.0
      * @group seq
      */
    def ~>[B](q: =>Parsley[B]): Parsley[B] = this *> q
    /** This combinator, pronounced "then discard", first parses this parser then parses `q`: if both succeed then the result
      * of this parser is returned.
      *
      * First, this parser is ran, yielding `x` on success, then `q` is ran, yielding `y` on success. If both
      * are successful then `x` is returned and `y` is ignored. If either fail then the entire combinator fails.
      *
      * ''Identical to `<*`: `<*` is more common in Haskell, wheras `<~` is more common in Scala.''
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> ('a' <~ 'b').parse("ab")
      * val res0 = Success('a')
      * }}}
      *
      * @param q the parser to run second, which returns the result of this combinator.
      * @return a parser that sequences this parser with `q` and returns this parser's result.
      * @since 2.4.0
      * @group seq
      */
    def <~[B](q: =>Parsley[B]): Parsley[A] = this <* q
    /** This combinator, pronounced "prepend", first parses this parser then parses `ps`: if both succeed the result of this
      * parser is prepended onto the result of `ps`.
      *
      * First, this parser is ran, yielding `x` on success, then `ps` is ran, yielding `xs` on success. If both
      * are successful then `x +: xs` is returned. If either fail then the entire combinator fails.
      *
      * @example {{{
      * def some[A](p: Parsley[A]): Parsley[List[A]] = {
      *     p <+:> many(p) // since List[A] <: Seq[A]
      * }
      * }}}
      *
      * @param ps the parser to run second, which returns a sequence
      * @tparam Aʹ the type of the elements in the result sequence, which must be a supertype of
      *            the result type of this parser: this allows for weakening of the result type.
      * @return a parser that sequences this parser with `ps` and prepends its result onto `ps` result.
      * @note equivalent to `lift2(_ +: _, this, ps)`.
      * @group seq
      */
    def <+:>[Aʹ >: A](ps: =>Parsley[Seq[Aʹ]]): Parsley[Seq[Aʹ]] = lift.lift2[A, Seq[Aʹ], Seq[Aʹ]](_ +: _, this, ps)
    /** This combinator, pronounced "cons", first parses this parser then parses `ps`: if both succeed the result of this
      * parser is prepended onto the result of `ps`.
      *
      * First, this parser is ran, yielding `x` on success, then `ps` is ran, yielding `xs` on success. If both
      * are successful then `x :: xs` is returned. If either fail then the entire combinator fails.
      *
      * @example {{{
      * def some[A](p: Parsley[A]): Parsley[List[A]] = {
      *     p <::> many(p)
      * }
      * }}}
      *
      * @param ps the parser to run second, which returns a list
      * @tparam Aʹ the type of the elements in the result list, which must be a supertype of
      *            the result type of this parser: this allows for weakening of the result type.
      * @return a parser that sequences this parser with `ps` and prepends its result onto `ps` result.
      * @note equivalent to `lift2(_ :: _, this, ps)`.
      * @group seq
      */
    def <::>[Aʹ >: A](ps: =>Parsley[List[Aʹ]]): Parsley[List[Aʹ]] = lift.lift2[A, List[Aʹ], List[Aʹ]](_ :: _, this, ps)
    /** This combinator, pronounced "zip", first parses this parser then parses `q`: if both succeed the result of this
      * parser is paired with the result of `q`.
      *
      * First, this parser is ran, yielding `x` on success, then `q` is ran, yielding `y` on success. If both
      * are successful then `(x, y)` is returned. If either fail then the entire combinator fails.
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> val p = char('a') <~> char('b')
      * scala> p.parse("ab")
      * val res0 = Success(('a', 'b'))
      * scala> p.parse("b")
      * val res1 = Failure(..)
      * scala> p.parse("a")
      * val res2 = Failure(..)
      * }}}
      *
      * @param q the parser to run second
      * @return a parser that sequences this parser with `q` and pairs their results together.
      * @note equivalent to `lift2((_, _), this, q)`.
      * @group seq
      */
    def <~>[B](q: =>Parsley[B]): Parsley[(A, B)] = lift.lift2[A, B, (A, B)]((_, _), this, q)
    /** This combinator first parses this parser then parses `q`: if both succeed the result of this
      * parser is paired with the result of `q`.
      *
      * First, this parser is ran, yielding `x` on success, then `q` is ran, yielding `y` on success. If both
      * are successful then `(x, y)` is returned. If either fail then the entire combinator fails.
      *
      * @example {{{
      * scala> import parsley.character.char
      * scala> val p = char('a').zip(char('b'))
      * scala> p.parse("ab")
      * val res0 = Success(('a', 'b'))
      * scala> p.parse("b")
      * val res1 = Failure(..)
      * scala> p.parse("a")
      * val res2 = Failure(..)
      * }}}
      *
      * @param q the parser to run second
      * @return a parser that sequences this parser with `q` and pairs their results together.
      * @note alias for `<~>`.
      * @since 2.3.0
      * @group seq
      */
    def zip[B](q: =>Parsley[B]): Parsley[(A, B)] = this <~> q

    // FILTERING COMBINATORS
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
      * This casts the result of the parser into a new type `B`: if the value returned by the parser
      * is castable to type `B`, then this cast is performed; otherwise, the parser fails.
      * @tparam B The type to attempt to cast into
      * @since 2.0.0
      * @group filter
      */
    // TODO: improve?
    def cast[B: ClassTag]: Parsley[B] = this.collect {
        case x: B => x
    }

    // FOLDING COMBINATORS
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
    // TODO: improve?
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
    // TODO: improve?
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
    // TODO: improve?
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
    // TODO: improve?
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
    // TODO: improve?
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
    // TODO: improve?
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
    // TODO: improve?
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
    // TODO: improve?
    def reduceLeftOption[B >: A](op: (B, A) => B): Parsley[Option[B]] = option(this.reduceLeft(op))

    // EXPENSIVE SEQUENCING COMBINATORS
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

    // SPECIAL METHODS
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

    /**
      * Using this method signifies that the parser it is invoked on is impure and any optimisations which assume purity
      * are disabled.
      *
      * @group special
      */
    def unsafe(): Unit = internal.unsafe()
}

/** This object contains the core "function-style" combinators: all parsers will likely require something from within!
  *
  * In particular, it contains combinators for: controlling how input is consumed; injecting values into
  * the parser, or failing; extracting position information from the parser; conditional execution of
  * parsers; and more.
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
    // TODO: improve
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
    // TODO: improve
    def branch[A, B, C](b: Parsley[Either[A, B]], p: =>Parsley[A => C], q: =>Parsley[B => C]): Parsley[C] = {
        new Parsley(new frontend.Branch(b.internal, p.internal, q.internal))
    }
    /** This is one of the core operations of a selective functor. It will conditionally execute one of `q` depending on
      * whether or not `p` returns a `Left`. It can be used to implement `branch` and other selective operations, however
      * it is more efficiently implemented with `branch` itself.
      * @param p The first parser to parse
      * @param q If `p` returns `Left` then this parser is executed with the result
      * @return Either the result from `p` if it returned `Left` or the result of `q` applied to the `Right` from `p`
      * @group cond
      */
    // TODO: improve
    def select[A, B](p: Parsley[Either[A, B]], q: =>Parsley[A => B]): Parsley[B] = branch(p, q, pure(identity[B](_)))
    /** This function is an alias for `_.flatten`: provides namesake to Haskell.
      *
      * @group monad
      */
    // TODO: improve
    def join[A](p: Parsley[Parsley[A]]): Parsley[A] = p.flatten
    /** Given a parser `p`, attempts to parse `p`. If the parser fails, then `attempt` ensures that no input was
      * consumed. This allows for backtracking capabilities, disabling the implicit cut semantics offered by `<|>`.
      *
      * @param p The parser to run
      * @return The result of `p`, or if `p` failed ensures the parser state was as it was on entry.
      * @group prim
      */
    // TODO: improve
    def attempt[A](p: Parsley[A]): Parsley[A] = new Parsley(new frontend.Attempt(p.internal))
    /** Parses `p` without consuming any input. If `p` fails and consumes input then so does `lookAhead(p)`. Combine with
      * `attempt` if this is undesirable.
      *
      * @param p The parser to look ahead at
      * @return The result of the lookahead
      * @group prim
      */
    // TODO: improve
    def lookAhead[A](p: Parsley[A]): Parsley[A] = new Parsley(new frontend.Look(p.internal))
    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}
      *
      * @group prim
      */
    // TODO: improve
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new Parsley(new frontend.NotFollowedBy(p.internal))
    /**
      * The `empty` parser consumes no input and fails softly (that is to say, no error message)
      *
      * @group basic
      */
    // TODO: improve
    val empty: Parsley[Nothing] = new Parsley(singletons.Empty)
    /**
      * Returns `()`; defined as `pure(())` but aliased for sugar
      *
      * @group basic
      */
    // TODO: improve
    val unit: Parsley[Unit] = pure(())
    /**
      * This parser consumes no input and returns the current line number reached in the input stream
      * @return The line number the parser is currently at
      * @group pos
      */
    // TODO: improve
    val line: Parsley[Int] = new Parsley(singletons.Line)
    /**
      * This parser consumes no input and returns the current column number reached in the input stream
      * @return The column number the parser is currently at
      * @group pos
      */
    // TODO: improve
    val col: Parsley[Int] = new Parsley(singletons.Col)
    /**
      * This parser consumes no input and returns the current position reached in the input stream
      * @return Tuple of line and column number that the parser has reached
      * @group pos
      */
    // TODO: improve
    val pos: Parsley[(Int, Int)] = line <~> col
}
