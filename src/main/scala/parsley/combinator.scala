package parsley

import scala.annotation.{implicitNotFound, tailrec}

import parsley.Parsley.{attempt, empty, notFollowedBy, pure, select, unit}
import parsley.expr.chain

import parsley.internal.deepembedding.{frontend, singletons}

/** This module contains a huge number of pre-made combinators that are very useful for a variety of purposes.
  *
  * In particular, it contains combinators for: performing a parser iteratively, collecting all the results;
  * querying whether or not any input is left; optionally performing parsers; parsing delimited constructions;
  * handling multiple possible alternatives or parsers to sequence; handling more complex conditional execution; and more.
  * @since 2.2.0
  *
  * @groupprio iter 0
  * @groupname iter Iterative Combinators
  * @groupdesc iter
  *     These combinators all execute a given parser an unbounded number of times, until either it fails, or another
  *     parser succeeds, depending on the combinator. Depending on the combinator, all of the results produced by the
  *     repeated execution of the parser may be returned in a `List`. These are almost essential for any practical parsing
  *     task.
  *
  * @groupprio item 10
  * @groupname item Input Query Combinators
  * @groupdesc item
  *     These combinators do not consume input, but they allow for querying of the input stream - specifically checking
  *     whether or not there is more input that can be consumed or not. In particular, most parsers should be making
  *     use of `eof` to ensure that the parser consumes all the input available at the end of the parse.
  *
  * @groupprio opt 20
  * @groupname opt Optional Parsing Combinators
  * @groupdesc opt
  *     These combinators allow for the ''possible'' parsing of some parser. If the parser succeeds, that is ok
  *     so long as it '''did not consume input'''. Be aware that the result of the success may be replaced with
  *     these combinators, with the exception of [[option `option`]], which still preserves the result.
  *
  * @groupprio sep 25
  * @groupname sep Separated Values Combinators
  * @groupdesc sep
  *     These combinators are concerned with delimited parsing, where one parser is repeated but delimited by another one.
  *     In each of these cases `p` is the parser of interest and `sep` is the delimeter. These combinators mainly differ
  *     in either the number of `p`s they require, or exactly where the delimeters are allowed (only between, always
  *     trailing, or either). In all cases, they return the list of results generated by the repeated parses of `p`.
  *
  * @groupprio multi 50
  * @groupname multi Multiple Branching/Sequencing Combinators
  * @groupdesc multi
  *     These combinators allow for testing or seqeuncing a large number of parsers in one go. Be careful, however, these are
  *     variadic combinators and are necessarily (for compatibility with Scala 2) '''not lazy'''.
  *
  *     In such a case where laziness is desired without resorting to the other lazier combinators, there
  *     is a neat trick: unroll the first iteration of the combinator, and use the corresponding regular combinator
  *     to do that (i.e. `<::>`, `<|>` (with or without `attempt`), or `*>`): since these will have a lazy
  *     right-hand side, the remaining variadic arguments will be kept lazily suspended until later. Alternatively,
  *     it is possible to use the [[parsley.Parsley.LazyParsley.unary_~ prefix `~`]] combinator to make any individual
  *     arguments lazy as required, for example `choice(p, ~q, r)`.
  *
  * @groupprio cond 75
  * @groupname cond Conditional Combinators
  * @groupdesc cond
  *     These combinators allow for the conditional extraction of a result, or the execution of a parser
  *     based on another. They are morally related to [[Parsley.branch `branch`]] and [[Parsley.select `select`]] but are
  *     less fundamental.
  *
  * @groupprio misc 100
  * @groupname misc Miscellaneous
  */
object combinator {
    //TODO: Standardise
    /** `choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      * Returns the value of the succeeding parser.
      *
      * @group multi
      */
    def choice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceRightOption(_ <|> _).getOrElse(empty)

    //TODO: Standardise
    /** `attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      * Returns the value of the succeeding parser. Utilises `attempt p <|> q` vs choice's `p <|> q`.
      *
      * @group multi
      */
    def attemptChoice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceRightOption((p, q) => attempt(p) <|> q).getOrElse(empty)

    //TODO: Standardise
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, collecting the results.
      * @param ps Parsers to be sequenced
      * @return The list containing results, one from each parser, in order
      * @group multi
      * @since 4.0.0
      */
    def sequence[A](ps: Parsley[A]*): Parsley[List[A]] = ps.foldRight(pure[List[A]](Nil))(_ <::> _)

    //TODO: Standardise
    /**
      * Like `sequence` but produces a list of parsers to sequence by applying the function `f` to each
      * element in `xs`.
      * @param f The function to map on each element of `xs` to produce parsers
      * @param xs Values to generate parsers from
      * @return The list containing results formed by executing each parser generated from `xs` and `f` in sequence
      * @group multi
      * @since 4.0.0
      */
    def traverse[A, B](f: A => Parsley[B], xs: A*): Parsley[List[B]] = sequence(xs.map(f): _*)

    //TODO: Standardise
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, ignoring the results.
      * @param ps Parsers to be performed
      * @group multi
      */
    def skip(ps: Parsley[_]*): Parsley[Unit] = ps.foldRight(unit)(_ *> _)

    /** This combinator parses exactly `n` occurrences of `p`, returning these `n` results in a list.
      *
      * Parses `p` repeatedly up to `n` times. If `p` fails before `n` is reached, then this combinator
      * fails. It is not required for `p` to fail after the `n`^th^ parse. The results produced by
      * `p`, `x,,1,,` through `x,,n,,`, are returned as `List(x,,1,,, .., x,,n,,)`.
      *
      * @example {{{
      * scala> import parsley.character.item
      * scala> import parsley.combinator.exactly
      * scala> val p = exactly(3, item)
      * scala> p.parse("ab")
      * val res0 = Failure(..)
      * scala> p.parse("abc")
      * val res1 = Success(List('a', 'b', 'c'))
      * scala> p.parse("abcd")
      * val res2 = Success(List('a', 'b', 'c'))
      * }}}
      *
      * @param n the number of times to repeat `p`.
      * @param p the parser to repeat.
      * @return a parser that parses `p` exactly `n` times, returning a list of the results.
      * @group misc
      * @since 4.0.0
      */
    def exactly[A](n: Int, p: Parsley[A]): Parsley[List[A]] = traverse[Int, A](_ => p, (1 to n): _*)

    /** This combinator tries to parse `p`, wrapping its result in a `Some` if it succeeds, or returns `None` if it fails.
      *
      * Tries to parse `p`. If `p` succeeded, producing `x`, then `Some(x)` is returned. Otherwise, if `p` failed
      * '''without consuming input''', then `None` is returned instead.
      *
      * @example {{{
      * scala> import parsley.combinator.option
      * scala> import parsley.character.string
      * scala> val p = option(string("abc"))
      * scala> p.parse("")
      * val res0 = Success(None)
      * scala> p.parse("abc")
      * val res1 = Success(Some("abc"))
      * scala> p.parse("ab")
      * val res2 = Failure(..)
      * }}}
      *
      * @param p the parser to try to parse.
      * @return a parser that tries to parse `p`, but can still succeed with `None` if that was not possible.
      * @group opt
      */
    def option[A](p: Parsley[A]): Parsley[Option[A]] = p.map(Some(_)).getOrElse(None)

    /** This combinator will parse `p` if possible, otherwise will do nothing.
      *
      * Tries to parse `p`. If `p` succeeds, or fails '''without consuming input''' then this combinator is successful. Otherwise, if `p` failed
      * having consumed input, this combinator fails.
      *
      * @example {{{
      * scala> import parsley.combinator.optional
      * scala> import parsley.character.string
      * scala> val p = optional(string("abc"))
      * scala> p.parse("")
      * val res0 = Success(())
      * scala> p.parse("abc")
      * val res1 = Success(())
      * scala> p.parse("ab")
      * val res2 = Failure(..)
      * }}}
      *
      * @param p the parser to try to parse.
      * @return a parser that tries to parse `p`.
      * @note equivalent to `optionalAs(p, ())`.
      * @group opt
      */
    def optional(p: Parsley[_]): Parsley[Unit] = optionalAs(p, ())

    /** This combinator will parse `p` if possible, otherwise will do nothing.
      *
      * Tries to parse `p`. If `p` succeeds, or fails '''without consuming input''' then this combinator is successful and returns `x`. Otherwise,
      * if `p` failed having consumed input, this combinator fails.
      *
      * @example {{{
      * scala> import parsley.combinator.optionalAs
      * scala> import parsley.character.string
      * scala> val p = optionalAs(string("abc"), 7)
      * scala> p.parse("")
      * val res0 = Success(7)
      * scala> p.parse("abc")
      * val res1 = Success(7)
      * scala> p.parse("ab")
      * val res2 = Failure(..)
      * }}}
      *
      * @param p the parser to try to parse.
      * @param x the value to return regardless of how `p` performs.
      * @return a parser that tries to parse `p`, returning `x` regardless of success or failure.
      * @group opt
      */
    def optionalAs[A](p: Parsley[_], x: A): Parsley[A] = {
        (p #> x).getOrElse(x)
    }

    //TODO: Standardise
    /** `decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.
      *
      * @group cond
      */
    def decide[A](p: Parsley[Option[A]]): Parsley[A] = p.collect {
        case Some(x) => x
    }

    //TODO: Standardise
    /** `decide(p, q)` removes the option from inside parser `p`, if it returned `None` then `q` is executed.
      *
      * @group cond
      */
    def decide[A](p: Parsley[Option[A]], q: =>Parsley[A]): Parsley[A] = select(p.map(_.toRight(())), q.map(x => (_: Unit) => x))

    /** This combinator parses `open`, followed by `p`, and then `close`.
      *
      * First parse `open`, ignore its result, then parse, `p`, producing `x`. Finally, parse `close`, ignoring its result.
      * If `open`, `p`, and `close` all succeeded, then return `x`. If any of them failed, this combinator fails.
      *
      * @example {{{
      * def braces[A](p: Parsley[A]) = between(char('{'), char('}'), p)
      * }}}
      *
      * @param open the first parser to parse.
      * @param close the last parser to parse.
      * @param p the parser to parse between the other two.
      * @return a parser that reads `open`, then `p`, then `close` and returns the result of `p`.
      * @group misc
      */
    def between[A](open: Parsley[_], close: =>Parsley[_], p: =>Parsley[A]): Parsley[A] = open *> p <* close

    /** This combinator repeatedly parses a given parser '''zero''' or more times, collecting the results into a list.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will return all of the results, `x,,1,,` through `x,,n,,` (with `n >= 0`), in a list: `List(x,,1,,, .., x,,n,,)`.
      * If `p` was never successful, the empty list is returned.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.many
      * scala> val p = many(string("ab"))
      * scala> p.parse("")
      * val res0 = Success(Nil)
      * scala> p.parse("ab")
      * val res1 = Success(List("ab"))
      * scala> p.parse("abababab")
      * val res2 = Success(List("ab", "ab", "ab", "ab"))
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return a parser that parses `p` until it fails, returning the list of all the successful results.
      * @since 2.2.0
      * @group iter
      */
    def many[A](p: Parsley[A]): Parsley[List[A]] = new Parsley(new frontend.Many(p.internal))

    /** This combinator repeatedly parses a given parser '''one''' or more times, collecting the results into a list.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will return all of the results, `x,,1,,` through `x,,n,,` (with `n >= 1`), in a list: `List(x,,1,,, .., x,,n,,)`.
      * If `p` was not successful at least one time, this combinator fails.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.some
      * scala> val p = some(string("ab"))
      * scala> p.parse("")
      * val res0 = Failure(..)
      * scala> p.parse("ab")
      * val res1 = Success(List("ab"))
      * scala> p.parse("abababab")
      * val res2 = Success(List("ab", "ab", "ab", "ab"))
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return a parser that parses `p` until it fails, returning the list of all the successful results.
      * @group iter
      */
    def some[A](p: Parsley[A]): Parsley[List[A]] = manyN(1, p)

    /** This combinator repeatedly parses a given parser '''`n`''' or more times, collecting the results into a list.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will return all of the results, `x,,1,,` through `x,,m,,` (with `m >= n`), in a list: `List(x,,1,,, .., x,,m,,)`.
      * If `p` was not successful at least `n` times, this combinator fails.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.manyN
      * scala> val p = manyN(2, string("ab"))
      * scala> p.parse("")
      * val res0 = Failure(..)
      * scala> p.parse("ab")
      * val res1 = Failure(..)
      * scala> p.parse("abababab")
      * val res2 = Success(List("ab", "ab", "ab", "ab"))
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param n the minimum number of `p`s required.
      * @param p the parser to execute multiple times.
      * @return a parser that parses `p` until it fails, returning the list of all the successful results.
      * @note `many(p) == many(0, p)` and `some(p) == many(1, p)`.
      * @group iter
      */
    def manyN[A](n: Int, p: Parsley[A]): Parsley[List[A]] = {
        @tailrec def go(n: Int, acc: Parsley[List[A]] = many(p)): Parsley[List[A]] = {
            if (n == 0) acc
            else go(n-1, p <::> acc)
        }
        go(n)
    }

    /** This combinator repeatedly parses a given parser '''zero''' or more times, ignoring the results.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will succeed.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.skipMany
      * scala> val p = skipMany(string("ab"))
      * scala> p.parse("")
      * val res0 = Success(())
      * scala> p.parse("ab")
      * val res1 = Success(())
      * scala> p.parse("abababab")
      * val res2 = Success(())
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return a parser that parses `p` until it fails, returning unit.
      * @since 2.2.0
      * @group iter
      */
    def skipMany(p: Parsley[_]): Parsley[Unit] = new Parsley(new frontend.SkipMany(p.internal))

    /** This combinator repeatedly parses a given parser '''one''' or more times, ignoring the results.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will succeed. The parser `p` must succeed at least once.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.skipSome
      * scala> val p = skipSome(string("ab"))
      * scala> p.parse("")
      * val res0 = Failure(..)
      * scala> p.parse("ab")
      * val res1 = Success(())
      * scala> p.parse("abababab")
      * val res2 = Success(())
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return a parser that parses `p` until it fails, returning unit.
      * @group iter
      */
    def skipSome(p: Parsley[_]): Parsley[Unit] = skipManyN(1, p)

    /** This combinator repeatedly parses a given parser '''`n`''' or more times, ignoring the results.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will succeed. The parser `p` must succeed at least `n` times.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.skipManyN
      * scala> val p = skipManyN(2, string("ab"))
      * scala> p.parse("")
      * val res0 = Failure(..)
      * scala> p.parse("ab")
      * val res1 = Failure(..)
      * scala> p.parse("abababab")
      * val res2 = Success(())
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return a parser that parses `p` until it fails, returning unit.
      * @group iter
      */
    def skipManyN(n: Int, p: Parsley[_]): Parsley[Unit] = {
        @tailrec def go(n: Int, acc: Parsley[Unit] = skipMany(p)): Parsley[Unit] = {
            if (n == 0) acc
            else go(n-1, p *> acc)
        }
        go(n)
    }

    //TODO: Standardise
    /** `sepBy(p, sep)` parses '''zero''' or more occurrences of `p`, separated by `sep`.
      *
      * @group sep
      */
    def sepBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepBy1(p, sep).getOrElse(Nil)

    //TODO: Standardise
    /** `sepBy1(p, sep)` parses '''one''' or more occurrences of `p`, separated by `sep`.
      *
      * @group sep
      */
    def sepBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = {
        p <::> many(sep *> p)
    }

    //TODO: Standardise
    /** `sepEndBy(p, sep)` parses '''zero''' or more occurrences of `p`, separated and optionally ended
      * by `sep`.
      *
      * @group sep
      */
    def sepEndBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepEndBy1(p, sep).getOrElse(Nil)

    //TODO: Standardise
    /** `sepEndBy1(p, sep)` parses '''one''' or more occurrences of `p`, separated and optionally ended
      * by `sep`.
      *
      * @group sep
      */
    def sepEndBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = new Parsley(new frontend.SepEndBy1(p.internal, sep.internal))

    //TODO: Standardise
    /** `endBy(p, sep)` parses '''zero''' or more occurrences of `p`, separated and ended by `sep`.
      *
      * @group sep
      */
    def endBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = many(p <* sep)

    //TODO: Standardise
    /** `endBy1(p, sep)` parses '''one''' or more occurrences of `p`, separated and ended by `sep`.
      *
      * @group sep
      */
    def endBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = some(p <* sep)

    /** This parser only succeeds at the end of the input.
      *
      * Equivalent to `notFollowedBy(item)`.
      *
      * @example {{{
      * scala> import parsley.combinator.eof
      * scala> eof.parse("a")
      * val res0 = Failure(..)
      * scala> eof.parse("")
      * val res1 = Success(())
      * }}}
      *
      * @group item
      */
    val eof: Parsley[Unit] = new Parsley(singletons.Eof)

    /** This parser only succeeds if there is still more input.
      *
      * Equivalent to `lookAhead(item).void`.
      *
      * @example {{{
      * scala> import parsley.combinator.more
      * scala> more.parse("")
      * val res0 = Failure(..)
      * scala> more.parse("a")
      * val res1 = Success(())
      * }}}
      *
      * @group item
      */
    val more: Parsley[Unit] = notFollowedBy(eof)

    /** This combinator repeatedly parses a given parser '''zero''' or more times, until the `end` parser succeeds, collecting the results into a list.
      *
      * First tries to parse `end`, if it fails '''without consuming input''', then parses `p`, which must succeed. This repeats until `end` succeeds.
      * When `end` does succeed, this combinator will return all of the results generated by `p`, `x,,1,,` through `x,,n,,` (with `n >= 0`), in a
      * list: `List(x,,1,,, .., x,,n,,)`. If `end` could be parsed immediately, the empty list is returned.
      *
      * @example This can be useful for scanning comments: {{{
      * scala> import parsley.character.{string, item, endOfLine}
      * scala> import parsley.combinator.many
      * scala> val comment = string("//") *> manyUntil(item, endOfLine)
      * scala> p.parse("//hello world")
      * val res0 = Failure(..)
      * scala> p.parse("//hello world\n")
      * val res1 = Success(List('h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'))
      * scala> p.parse("//\n")
      * val res2 = Success(Nil)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @param end the parser that stops the parsing of `p`.
      * @return a parser that parses `p` until `end` succeeds, returning the list of all the successful results.
      * @group iter
      */
    def manyUntil[A](p: Parsley[A], end: Parsley[_]): Parsley[List[A]] = {
        new Parsley(new frontend.ManyUntil((end #> ManyUntil.Stop <|> p).internal))
    }

    private [parsley] object ManyUntil {
        object Stop
    }

    /** This combinator repeatedly parses a given parser '''one''' or more times, until the `end` parser succeeds, collecting the results into a list.
      *
      * First ensures that trying to parse `end` fails, then tries to parse `p`. If it succeed then it will repeatedly: try to parse `end`, if it fails
      * '''without consuming input''', then parses `p`, which must succeed. When `end` does succeed, this combinator will return all of the results
      * generated by `p`, `x,,1,,` through `x,,n,,` (with `n >= 1`), in a list: `List(x,,1,,, .., x,,n,,)`. The parser `p` must succeed at least once
      * before `end` succeeds.
      *
      * @example This can be useful for scanning comments: {{{
      * scala> import parsley.character.{string, item, endOfLine}
      * scala> import parsley.combinator.many
      * scala> val comment = string("//") *> someUntil(item, endOfLine)
      * scala> p.parse("//hello world")
      * val res0 = Failure(..)
      * scala> p.parse("//hello world\n")
      * val res1 = Success(List('h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'))
      * scala> p.parse("//\n")
      * val res2 = Failure(..)
      * scala> p.parse("//a\n")
      * val res3 = Success(List('a'))
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @param end the parser that stops the parsing of `p`.
      * @return a parser that parses `p` until `end` succeeds, returning the list of all the successful results.
      * @group iter
      */
    def someUntil[A](p: Parsley[A], end: Parsley[_]): Parsley[List[A]] = {
        notFollowedBy(end) *> (p <::> manyUntil(p, end))
    }

    //TODO: Standardise
    /**
      * This is an if statement lifted to the parser level.
      *
      * @param b The parser that yields the condition value
      * @param p
      * @param q
      * @group cond
      * @since 4.0.0
      */
    def ifP[A](b: Parsley[Boolean], p: =>Parsley[A], q: =>Parsley[A]): Parsley[A] = {
        new Parsley(new frontend.If(b.internal, p.internal, q.internal))
    }

    //TODO: Standardise
    /** `when(p, q)` will first perform `p`, and if the result is `true` will then execute `q` or else return unit.
      * @param p The first parser to parse
      * @param q If `p` returns `true` then this parser is executed
      * @group cond
      */
    def when(p: Parsley[Boolean], q: =>Parsley[Unit]): Parsley[Unit] = ifP(p, q, unit)

    //TODO: Standardise
    /** `whileP(p)` will continue to run `p` until it returns `false`. This is often useful in conjunction with stateful
      * parsers.
      * @param p The parser to continuously execute
      * @group cond
      */
    def whileP(p: Parsley[Boolean]): Parsley[Unit] = {
        lazy val whilePP: Parsley[Unit] = when(p, whilePP)
        whilePP
    }
}
