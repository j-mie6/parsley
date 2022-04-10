package parsley

import parsley.internal.machine.Context
import parsley.internal.deepembedding.{singletons, frontend}
import parsley.expr.{chain, infix}
import parsley.combinator.{option, some, many}
import parsley.Parsley.pure
import parsley.errors.ErrorBuilder
import parsley.XCompat._

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
  *
  * @define bind
  *     first performs this parser, then uses its result to generate and execute a new parser.
  *
  *     First, this combinator runs this parser. If it succeeded, the result `x` is given to the function `f`
  *     to produce a new parser `f(x)`. This new parser is then executed and its result returned. If either
  *     of this parser or the new parser fails, then the entire combinator fails.
  *
  *     This is a very powerful combinator (Turing-powerful, in fact): it is only necessary (and not ''all''
  *     the time) to use this for context-sensitive parsing. The idea is that it can make any form of
  *     decision based on the result of a parser, allowing it to perform a different parser for each
  *     possible output of another without exhaustively enumerating them all.
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
    /** This combinator filters the result of this parser using a given predicate, succeeding only if the predicate returns `true`.
      *
      * First, parse this parser. If it succeeds then take its result `x` and apply it to the predicate `pred`. If `pred(x)` is
      * true, then return `x`. Otherwise, the combinator fails.
      *
      * @example {{{
      * scala> import parsley.character.letter
      * scala> val keywords = Set("if", "then", "else")
      * scala> val identifier = some(letter).map(_.mkString)
      *                                     .filter(!keywords.contains(_))
      * scala> identifier.parse("hello")
      * val res0 = Success("hello")
      * scala> idenfitier.parse("if")
      * val res1 = Failure(..)
      * }}}
      *
      * @param pred the predicate that is tested against the parser result
      * @return a parser that returns the result of this parser if it passes the predicate
      * @group filter
      */
    def filter(pred: A => Boolean): Parsley[A] = new Parsley(new frontend.Filter(this.internal, pred))
    /** This combinator filters the result of this parser using a given predicate, succeeding only if the predicate returns `false`.
      *
      * First, parse this parser. If it succeeds then take its result `x` and apply it to the predicate `pred`. If `pred(x) is
      * false, then return `x`. Otherwise, the combinator fails.
      *
      * @example {{{
      * scala> import parsley.character.letter
      * scala> val keywords = Set("if", "then", "else")
      * scala> val identifier = some(letter).map(_.mkString)
      *                                     .filterNot(keywords.contains(_))
      * scala> identifier.parse("hello")
      * val res0 = Success("hello")
      * scala> idenfitier.parse("if")
      * val res1 = Failure(..)
      * }}}
      *
      * @param pred the predicate that is tested against the parser result
      * @return a parser that returns the result of this parser if it fails the predicate
      * @group filter
      */
    def filterNot(pred: A => Boolean): Parsley[A] = this.filter(!pred(_))
    /** This combinator applies a partial function `pf` to the result of this parser if its result
      * is defined for `pf`, failing if it is not.
      *
      * First, parse this parser. If it succeeds, test whether its result `x` is in the
      * domain of the partial function `pf`. If it is defined for `pf`, return `pf(x)`.
      * If it is undefined, or this parser failed, then this combinator fails. Equivalent
      * to a `filter` followed by a `map`.
      *
      * @example {{{
      * scala> val int: Parsley[Int] = ..
      * scala> val safeDiv: Parsley[Int] = (int <~> char(' ') *> int).collect {
      *   case (x, y) if y != 0 => x / y
      * }
      * scala> safeDiv.parse("7 0")
      * val res0 = Failure(..) // y cannot be 0!
      * scala> safeDiv.parse("10 2")
      * val res1 = Success(5)
      * }}}
      *
      * @param pf the partial function used to both filter the result of this parser and transform it.
      * @return a parser which returns the result of this parser applied to `pf`, if possible.
      * @since 2.0.0
      * @group filter
      */
    def collect[B](pf: PartialFunction[A, B]): Parsley[B] = this.filter(pf.isDefinedAt).map(pf)
    /** This combinator attempts to downcast the result of this parser into a value of type `B`, failing
      * if this was not possible.
      *
      * First, this parser is parsed. If it is successful, an attempt is made to cast the result
      * of type `A` into a value of type `B`. If this cast was legal, then the combinator succeeds
      * with this casted result. Otherwise, the combinator fails.
      *
      * This combinator is safe (bad casting will result in a parse error, not a crash), but should
      * not be used purely to satisfy the type-checker. Its intended purpose is downcast values that
      * are ''known'' to be the right type, but for whatever reason must be stored as a weaker
      * type (say, `Any`).
      *
      * @example {{{
      * scala> import parsley.Parsley.pure
      * scala> val p: Parsley[Any] = pure(7)
      * scala> p.cast[Int].parse("")
      * val res0 = Success(7)
      * scala> p.cast[String].parse("")
      * val res1 = Failure(..)
      * }}}
      *
      * @tparam B the type to attempt to cast into, for which a `ClassTag[B]` must exist.
      * @return a parser that downcasts the result of this parser to have type `B`.
      * @since 2.0.0
      * @group filter
      */
    def cast[B: ClassTag]: Parsley[B] = this.collect {
        case x: B => x
    }

    // FOLDING COMBINATORS
    /** This combinator will parse this parser '''zero''' or more times combining the results with the function `f` and base value `k` from the right.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * All of the results generated by the successful parses are then combined in a right-to-left
      * fashion using the function `f`: the right-most value provided to `f` is the value `k`.
      * If this parser does fail at any point having consumed input, this combinator will fail.
      *
      * @example {{{
      * // in reality, many is implemented more efficiently
      * def many[A](p: Parsley[A]) = {
      *     p.foldRight(List.empty[A])(_ :: _)
      * }}}
      *
      * @param k value to use when this parser no longer succeeds.
      * @param f function to apply to each value produced by this parser, starting at the right.
      * @return a parser which parses this parser many times and folds the results together with `f` and `k` right-associatively.
      * @group fold
      */
    def foldRight[B](k: B)(f: (A, B) => B): Parsley[B] = chain.prefix(this.map(f.curried), pure(k))
    /** This combinator will parse this parser '''zero''' or more times combining the results with the function `f` and base value `k` from the left.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * All of the results generated by the successful parses are then combined in a left-to-right
      * fashion using the function `f`: the accumulation is initialised with the value `k`.
      * If this parser does fail at any point having consumed input, this combinator will fail.
      *
      * @example {{{
      * def stringOfMany(pc: Parsley[Char]): Parsley[String] = {
      *     pc.foldLeft(new StringBuilder)(_ += _).map(_.toString)
      * }
      * }}}
      *
      * @param k initial accumulation value.
      * @param f function to apply to each iteration's accumulator.
      * @return a parser which parses this parser many times and folds the results together with `f` and `k` left-associatively.
      * @group fold
      */
    def foldLeft[B](k: B)(f: (B, A) => B): Parsley[B] = new Parsley(new frontend.Chainl(pure(k).internal, this.internal, pure(f).internal))
    /** This combinator will parse this parser '''one''' or more times combining the results with the function `f` and base value `k` from the right.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * All of the results generated by the successful parses are then combined in a right-to-left
      * fashion using the function `f`: the right-most value provided to `f` is the value `k`.
      * If this parser does fail at any point having consumed input, this combinator will fail.
      *
      * @example {{{
      * // in reality, some is implemented more efficiently
      * def some[A](p: Parsley[A]) = {
      *     p.foldRight1(List.empty[A])(_ :: _)
      * }}}
      *
      * @param k value to use when this parser no longer succeeds.
      * @param f function to apply to each value produced by this parser, starting at the right.
      * @return a parser which parses this parser some times and folds the results together with `f` and `k` right-associatively.
      * @since 2.1.0
      * @group fold
      */
    def foldRight1[B](k: B)(f: (A, B) => B): Parsley[B] = {
        lift.lift2(f, this, this.foldRight(k)(f))
    }
    /** This combinator will parse this parser '''one''' or more times combining the results with the function `f` and base value `k` from the left.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * All of the results generated by the successful parses are then combined in a left-to-right
      * fashion using the function `f`: the accumulation is initialised with the value `k`.
      * If this parser does fail at any point having consumed input, this combinator will fail.
      *
      * @example {{{
      * val natural: Parsley[Int] = digit.foldLeft1(0)((x, d) => x * 10 + d.toInt)
      * }}}
      *
      * @param k initial accumulation value.
      * @param f function to apply to each iteration's accumulator.
      * @return a parser which parses this parser some times and folds the results together with `f` and `k` left-associatively.
      * @since 2.1.0
      * @group fold
      */
    def foldLeft1[B](k: B)(f: (B, A) => B): Parsley[B] = {
        new Parsley(new frontend.Chainl(this.map(f(k, _)).internal, this.internal, pure(f).internal))
    }
    /** This combinator will parse this parser '''one''' or more times combining the results right-associatively with the function `op`.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * All of the results generated by the successful parses are then combined in a right-to-left
      * fashion using the function `op`. If this parser does fail at any point having consumed input,
      * this combinator will fail.
      *
      * @example {{{
      * stmt.reduceRight(Seq(_, _))
      * }}}
      *
      * @param op function to apply to each value produced by this parser, starting from the right.
      * @return a parser which parses this parser some times and folds the results together with `op` right-associatively.
      * @since 2.3.0
      * @group fold
      */
    def reduceRight[B >: A](op: (A, B) => B): Parsley[B] = some(this).map(_.reduceRight(op))
    /** This combinator will parse this parser '''zero''' or more times combining the results right-associatively with the function `op`.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * If this parser succeeded at least once, all of the results generated by the successful
      * parses are then combined in a right-to-left fashion using the function `op` and returned
      * in a `Some`. If no successful parses occurred, then `None` is returned. If this
      * parser does fail at any point having consumed input, this combinator will fail.
      *
      * @example {{{
      * arg.reduceRightOption(Sep(_, _))
      * }}}
      *
      * @param op function to apply to each value produced by this parser, starting from the right.
      * @return a parser which parses this parser many times and folds the results together with `op` right-associatively or returns None if no parses occured.
      * @since 2.3.0
      * @group fold
      */
    def reduceRightOption[B >: A](op: (A, B) => B): Parsley[Option[B]] = option(this.reduceRight(op))
    /** This combinator will parse this parser '''one''' or more times combining the results left-associatively with the function `op`.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * All of the results generated by the successful parses are then combined in a left-to-right
      * fashion using the function `op`. If this parser does fail at any point having consumed input,
      * this combinator will fail.
      *
      * @example {{{
      * stmt.reduceLeft(Seq(_, _))
      * }}}
      *
      * @param op function to apply to each value produced by this parser, starting from the left.
      * @return a parser which parses this parser some times and folds the results together with `op` left-associatively.
      * @since 2.3.0
      * @group fold
      */
    def reduceLeft[B >: A](op: (B, A) => B): Parsley[B] = infix.left1(this, pure(op))
    /** This combinator will parse this parser '''zero''' or more times combining the results left-associatively with the function `op`.
      *
      * This parser will continue to be parsed until it fails having '''not consumed''' input.
      * If this parser succeeded at least once, all of the results generated by the successful
      * parses are then combined in a left-to-right fashion using the function `op` and returned
      * in a `Some`. If no successful parses occurred, then `None` is returned. If this
      * parser does fail at any point having consumed input, this combinator will fail.
      *
      * @example {{{
      * arg.reduceLeftOption(Sep(_, _))
      * }}}
      *
      * @param op function to apply to each value produced by this parser, starting from the left.
      * @return a parser which parses this parser many times and folds the results together with `op` left-associatively or returns None if no parses occured.
      * @since 2.3.0
      * @group fold
      */
    def reduceLeftOption[B >: A](op: (B, A) => B): Parsley[Option[B]] = option(this.reduceLeft(op))

    // EXPENSIVE SEQUENCING COMBINATORS
    /** This combinator $bind
      *
      * ''In Haskell, this combinator is known as `(>>=)` (pronounced "bind")''.
      *
      * @example {{{
      * // this is an inefficient implementation, but does work
      * def filter(pred: A => Boolean): Parsley[A] = {
      *     this.flatMap { x =>
      *         if (pred(x)) pure(x)
      *         else empty
      *     }
      * }
      * }}}
      *
      * @note there is '''significant''' overhead for using `flatMap`: if possible try to avoid using it! This
      *       is because Parsley will need to generate, process, and compile each parser produced by the combinator
      *       during parse-time.
      * @param f the function that produces the next parser.
      * @return a new parser, which sequences this parser with the parser generated from its result.
      * @group monad
      */
    def flatMap[B](f: A => Parsley[B]): Parsley[B] = new Parsley(new frontend.>>=(this.internal, f.andThen(_.internal)))
    /** This combinator, pronounced "bind", $bind
      *
      * @example {{{
      * // this is an inefficient implementation, but does work
      * def filter(pred: A => Boolean): Parsley[A] = {
      *     this >>= { x =>
      *         if (pred(x)) pure(x)
      *         else empty
      *     }
      * }
      * }}}
      *
      * @note there is '''significant''' overhead for using `>>=`: if possible try to avoid using it! This
      *       is because Parsley will need to generate, process, and compile each parser produced by the combinator
      *       during parse-time.
      * @param f the function that produces the next parser.
      * @return a new parser, which sequences this parser with the parser generated from its result.
      * @group monad
      */
    def >>=[B](f: A => Parsley[B]): Parsley[B] = this.flatMap(f)
    /** This combinator collapses two layers of parsing structure into one.
      *
      * The implicit (compiler-provided) evidence proves that this parser really has type `Parsley[Parsley[B]]`.
      * First, this parser is executed, which, if successful, will produce a new parser `p`. The parser `p` is
      * then executed, and its result is returned. If either this parser or `p` fail, then the entire combinator
      * fails.
      *
      * This is a very powerful combinator (Turing-powerful, in fact): it is only necessary (and not ''all''
      * the time) to use this for context-sensitive parsing. The idea is that it can allow a parser to return
      * any other parser as its result: this allows for an implementation of a later parser to be picked
      * by an earlier one.
      *
      * ''In Haskell, this combinator is known as `join`''.
      *
      * @example imagine a parser for RegEx that first reads the description of the regular expression, then runs
      *          that against the remaining input string. It is possible to implement the parser for the regular
      *          expression itself as a `Parsley[Parsley[Unit]]`, which returns a parser that matches the regular
      *          expression. This can then be used to parse the remaining input by using `flatten` to incorporate
      *          it into the parser again:
      * {{{
      * scala> val regexDesc: Parsley[Parsley[Unit]] = ..
      * // let's suppose "#" is the delimeter between expression and input
      * scala> val regex: Parsley[Unit] = (regexDesc <* char('#')).flatten
      * scala> regex.parse("a.(c*)#abccc")
      * val res0 = Success(())
      * scala> regex.parse("a.(c*)#a")
      * val res1 = Failure(..)
      * }}}
      *
      * @param ev witnesses that the result type of this parser, `A`, is really `Parsley[B]`.
      * @return a new parser, which sequences this parser with its result parser.
      * @note there is '''significant''' overhead for using `flatten`: if possible try to avoid using it! This
      *       is because Parsley will need to generate, process, and compile each parser produced by the combinator
      *       during parse-time.
      * @group monad
      */
    def flatten[B](implicit ev: A <:< Parsley[B]): Parsley[B] = this.flatMap[B](ev)

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
object Parsley {
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
