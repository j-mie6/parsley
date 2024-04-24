/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.annotation.tailrec
import scala.collection.{Factory, mutable}

import parsley.Parsley.{atomic, empty, fresh, many, notFollowedBy, transPure => pure, secretSome, select, some}
import parsley.state.{RefMaker, StateCombinators, forP}
import parsley.syntax.zipped.zippedSyntax2

import parsley.internal.deepembedding.frontend

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
  *     These combinators allow for testing or sequencing a large number of parsers in one go. Be careful, however, these are
  *     variadic combinators and are necessarily (for compatibility with Scala 2) '''not lazy'''.
  *
  *     In such a case where laziness is desired without resorting to the other lazier combinators, there
  *     is a neat trick: unroll the first iteration of the combinator, and use the corresponding regular combinator
  *     to do that (i.e. `<::>` or `*>`): since these will have a lazy
  *     right-hand side, the remaining variadic arguments will be kept lazily suspended until later. Alternatively,
  *     it is possible to use the [[parsley.Parsley.LazyParsley.unary_~ prefix `~`]] combinator to make any individual
  *     arguments lazy as required, for example `skip(p, ~q, r)`.
  *
  * @groupprio range 65
  * @groupname range Range Combinators
  * @groupdesc range
  *     These combinators allow for the parsing of a specific parser either a specific number of times, or between a certain
  *     amount of times.
  *
  * @groupprio condComp 75
  * @groupname condComp Conditional Combinators
  * @groupdesc condComp
  *     These combinators allow for the conditional extraction of a result, or the execution of a parser
  *     based on another. They are morally related to [[Parsley.branch `branch`]] and [[Parsley.select `select`]] but are
  *     less fundamental.
  *
  * @define strict be aware that all of the arguments to this combinator are in '''strict''' positions.
  */
object combinator extends combinator
private [parsley] trait combinator {
    /** This combinator tries to parse each of the parsers `ps` in order, until one of them succeeds.
      *
      * Finds the first parser in `ps` which succeeds, returning its result. If none of the parsers
      * succeed, then this combinator fails. If a parser fails having consumed input, this combinator
      * fails '''immediately'''.
      *
      * @example {{{
      * scala> import parsley.combinator.choice
      * scala> import parsley.character.string
      * scala> val p = choice(string("abc"), string("ab"), string("bc"), string("d"))
      * scala> p.parse("abc")
      * val res0 = Success("abc")
      * scala> p.parse("ab")
      * val res1 = Failure(..)
      * scala> p.parse("bc")
      * val res2 = Success("bc")
      * scala> p.parse("x")
      * val res3 = Failure(..)
      * }}}
      *
      * @param ps the parsers to try, in order.
      * @return a parser that tries to parse one of `ps`.
      * @group multi
      * @see [[parsley.Parsley.<|> `<|>`]]
      */
    final def choice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceRightOption(_ <|> _).getOrElse(empty)

    // This combinator is still used in internal testing, but is a trap for new users
    // it will not be exposed in the API again.
    private [parsley] final def atomicChoice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceRightOption((p, q) => atomic(p) <|> q).getOrElse(empty)

    /** This combinator will parse each of `ps` in order, collecting the results.
      *
      * Given the parsers `ps`, consisting of `p,,1,,` through `p,,n,,`, parses
      * each in order. If they all succeed, producing the results `x,,1,,` through `x,,n,,`,
      * then `List(x,,1,,, .., x,,n,,)` is returned. If any of the parsers fail, then
      * the whole combinator fails.
      *
      * @example {{{
      * scala> import parsley.combinator.sequence
      * scala> import parsley.character.{char, item}
      * scala> val p = sequence(char('a'), item, char('c'))
      * scala> p.parse("abc")
      * val res0 = Success(List('a', 'b', 'c'))
      * scala> p.parse("ab")
      * val res1 = Failure(..)
      * }}}
      *
      * @param p0 the first parser to be sequenced
      * @param ps parsers to be sequenced.
      * @return a parser that parses each of `ps`, returning the results in a list
      * @group multi
      * @since 4.0.0
      * @see [[parsley.Parsley.<::> `<::>`]]
      * @note $strict
      */
    final def sequence[A](p0: Parsley[A], ps: Parsley[A]*): Parsley[List[A]] = sequence[A, List[A]](List, p0, ps: _*)
    private [parsley] final def sequence[A, C](factory: Factory[A, C], p0: Parsley[A], ps: Parsley[A]*): Parsley[C] = {
        @tailrec def go(acc: Parsley[mutable.Builder[A, C]], ps: List[Parsley[A]]): Parsley[C] = ps match {
            case Nil => acc.map(_.result())
            case p::ps => go((acc, p).zipped(_ += _), ps)
        }
        go(p0.map(factory.newBuilder += _), ps.toList)
    }

    /** This combinator will parse each of the parsers generated by applying `f` to `xs`, in order, collecting the results.
      *
      * Given the values `xs`, consisting of `x,,1,,` through `x,,n,,`, first creates
      * the parses `f(x,,1,,)` through `f(x,,n,,)` and then called `sequence` on them.
      *
      * @example {{{
      * // this is an OK implementation for `string`, which is common in Haskell.
      * def string(str: String) = {
      *     traverse(str:  _*)(char).span
      * }
      * }}}
      *
      * @param f the function used to generate parsers for each values
      * @param x0 the first value to turn into a parser and sequence.
      * @param xs the values to turn into parsers and sequence.
      * @return a parser that sequences the parsers generated from applying `f` to each of `xs`.
      * @group multi
      * @since 4.0.0
      * @see [[sequence `sequence`]]
      * @note $strict
      */
    final def traverse[A, B](x0: A, xs: A*)(f: A => Parsley[B]): Parsley[List[B]] = traverseGen(x0, xs: _*)(f, List)
    private [parsley] final def traverseGen[A, B, C](x0: A, xs: A*)(f: A => Parsley[B], factory: Factory[B, C]): Parsley[C] = {
        sequence(factory, f(x0), xs.map(f): _*)
    }

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
    final def option[A](p: Parsley[A]): Parsley[Option[A]] = p.map(Some(_)) </> None

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
    final def optional(p: Parsley[_]): Parsley[Unit] = optionalAs(p, ())

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
    final def optionalAs[A](p: Parsley[_], x: A): Parsley[A] = p.as(x) </> x

    /** This combinator can eliminate an `Option` from the result of the parser `p`.
      *
      * First parse `p`, if it succeeds returning `Some(x)`, then return `x`. However,
      * if `p` fails, or returned `None`, then this combinator fails.
      *
      * @example {{{
      * decide(option(p)) = p
      * }}}
      *
      * @param p the parser to parse and extract the result from.
      * @return a parser that tries to extract the result from `p`.
      * @group condComp
      */
    final def decide[A](p: Parsley[Option[A]]): Parsley[A] = p.collect {
        case Some(x) => x
    }

    /** This combinator parses `q` depending only if `p` returns a `None`.
      *
      * First parses `p`. If `p` returned `Some(x)`, then `x` is returned.
      * Otherwise, if `p` returned `None` then `q` is parsed, producing `y`,
      * and `y` is returned. If `p` or `q` fails, the combinator fails.
      *
      * @example {{{
      * decide(option(p), q) = p <|> q
      * }}}
      *
      * @param p the first parser, which returns an `Option` to eliminate.
      * @param q a parser to execute when `p` returns `None`, to provide a value of type `A`.
      * @return a parser that either just parses `p` or both `p` and `q` in order to return an `A`.
      * @group condComp
      */
    final def decide[A](p: Parsley[Option[A]], q: =>Parsley[A]): Parsley[A] = select(p.map(_.toRight(())), q.map(x => (_: Unit) => x))

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
    final def manyN[A](n: Int, p: Parsley[A]): Parsley[List[A]] = manyN(n, p, List)
    private [parsley] final def manyN[A, C](n: Int, p: Parsley[A], factory: Factory[A, C]): Parsley[C] = {
        require(n >= 0, "cannot pass negative integer to `manyN`")
        fresh(factory.newBuilder).persist { acc =>
            forP[Int](pure(0), pure(_ < n), pure(_ + 1)) {
                (acc, p).zipped(_ += _).impure // we don't want this optimised out, it's a mutable operation in a resultless context
            } ~> secretSome(acc, p)
        }
    }

    /** This combinator repeatedly parses a given parser '''zero''' or more times, returning how many times it succeeded.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will succeed. The number of times `p` succeeded is returned as the result.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.countMany
      * scala> val p = countMany(string("ab"))
      * scala> p.parse("")
      * val res0 = Success(0)
      * scala> p.parse("ab")
      * val res1 = Success(1)
      * scala> p.parse("abababab")
      * val res2 = Success(4)
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return the number of times `p` successfully parses
      * @group iter
      * @since 4.5.0
      */
    final def countMany(p: Parsley[_]): Parsley[Int] = p.foldLeft(0)((n, _) => n + 1)

    /** This combinator repeatedly parses a given parser '''one''' or more times, returning how many times it succeeded.
      *
      * Parses a given parser, `p`, repeatedly until it fails. If `p` failed having consumed input,
      * this combinator fails. Otherwise when `p` fails '''without consuming input''', this combinator
      * will succeed. The parser `p` must succeed at least once. The number of times `p` succeeded is returned as the result.
      *
      * @example {{{
      * scala> import parsley.character.string
      * scala> import parsley.combinator.countSome
      * scala> val p = countSome(string("ab"))
      * scala> p.parse("")
      * val res0 = Failure(..)
      * scala> p.parse("ab")
      * val res1 = Success(1)
      * scala> p.parse("abababab")
      * val res2 = Success(4)
      * scala> p.parse("aba")
      * val res3 = Failure(..)
      * }}}
      *
      * @param p the parser to execute multiple times.
      * @return the number of times `p` successfully parses
      * @group iter
      * @since 4.5.0
      */
    final def countSome(p: Parsley[_]): Parsley[Int] = p.foldLeft1(0)((n, _) => n + 1)

    /** This combinator parses '''zero''' or more occurrences of `p`, separated by `sep`.
      *
      * Behaves just like `sepBy1`, except does not require an initial `p`, returning the empty list instead.
      *
      * @example {{{
      * scala> ...
      * scala> val args = sepBy(int, string(", "))
      * scala> args.parse("7, 3, 2")
      * val res0 = Success(List(7, 3, 2))
      * scala> args.parse("")
      * val res1 = Success(Nil)
      * scala> args.parse("1")
      * val res2 = Success(List(1))
      * scala> args.parse("1, 2, ")
      * val res3 = Failure(..) // no trailing comma allowed
      * }}}
      *
      * @param p the parser whose results are collected into a list.
      * @param sep the delimiter that must be parsed between every `p`.
      * @return a parser that parses `p` delimited by `sep`, returning the list of `p`'s results.
      * @group sep
      */
    final def sepBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepBy1(p, sep) </> Nil
    private [parsley] final def sepBy[A, C](p: Parsley[A], sep: =>Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        sepBy1(p, sep, factory) <|> fresh(factory.newBuilder.result())
    }

    /** This combinator parses '''one''' or more occurrences of `p`, separated by `sep`.
      *
      * First parses a `p`. Then parses `sep` followed by `p` until there are no more `sep`s.
      * The results of the `p`'s, `x,,1,,` through `x,,n,,`, are returned as `List(x,,1,,, .., x,,n,,)`.
      * If `p` or `sep` fails having consumed input, the whole parser fails. Requires at least
      * one `p` to have been parsed.
      *
      * @example {{{
      * scala> ...
      * scala> val args = sepBy1(int, string(", "))
      * scala> args.parse("7, 3, 2")
      * val res0 = Success(List(7, 3, 2))
      * scala> args.parse("")
      * val res1 = Failure(..)
      * scala> args.parse("1")
      * val res2 = Success(List(1))
      * scala> args.parse("1, 2, ")
      * val res3 = Failure(..) // no trailing comma allowed
      * }}}
      *
      * @param p the parser whose results are collected into a list.
      * @param sep the delimiter that must be parsed between every `p`.
      * @return a parser that parses `p` delimited by `sep`, returning the list of `p`'s results.
      * @group sep
      */
    final def sepBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = p <::> many(sep *> p)
    private [parsley] final def sepBy1[A, C](p: Parsley[A], sep: =>Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        secretSome(p, sep *> p, factory)
    }

    /** This combinator parses '''zero''' or more occurrences of `p`, separated and optionally ended by `sep`.
      *
      * Behaves just like `sepEndBy1`, except does not require an initial `p`, returning the empty list instead.
      *
      * @example {{{
      * scala> ...
      * scala> val args = sepEndBy(int, string(";\n"))
      * scala> args.parse("7;\n3;\n2")
      * val res0 = Success(List(7, 3, 2))
      * scala> args.parse("")
      * val res1 = Success(Nil)
      * scala> args.parse("1")
      * val res2 = Success(List(1))
      * scala> args.parse("1;\n2;\n")
      * val res3 = Success(List(1, 2))
      * }}}
      *
      * @param p the parser whose results are collected into a list.
      * @param sep the delimiter that must be parsed between every `p`.
      * @return a parser that parses `p` delimited by `sep`, returning the list of `p`'s results.
      * @group sep
      */
    final def sepEndBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepEndBy(p, sep, List)
    private [parsley] final def sepEndBy[A, C](p: Parsley[A], sep: =>Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        sepEndBy1(p, sep, factory) <|> fresh(factory.newBuilder.result())
    }

    /** This combinator parses '''one''' or more occurrences of `p`, separated and optionally ended by `sep`.
      *
      * First parses a `p`. Then parses `sep` followed by `p` until there are no more: if a final `sep` exists, this is parsed.
      * The results of the `p`'s, `x,,1,,` through `x,,n,,`, are returned as `List(x,,1,,, .., x,,n,,)`.
      * If `p` or `sep` fails having consumed input, the whole parser fails. Requires at least
      * one `p` to have been parsed.
      *
      * @example {{{
      * scala> ...
      * scala> val args = sepEndBy1(int, string(";\n"))
      * scala> args.parse("7;\n3;\n2")
      * val res0 = Success(List(7, 3, 2))
      * scala> args.parse("")
      * val res1 = Failure(..)
      * scala> args.parse("1")
      * val res2 = Success(List(1))
      * scala> args.parse("1;\n2;\n")
      * val res3 = Success(List(1, 2))
      * }}}
      *
      * @param p the parser whose results are collected into a list.
      * @param sep the delimiter that must be parsed between every `p`.
      * @return a parser that parses `p` delimited by `sep`, returning the list of `p`'s results.
      * @group sep
      */
    final def sepEndBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepEndBy1(p, sep, List)
    private [parsley] final def sepEndBy1[A, C](p: Parsley[A], sep: =>Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        new Parsley(new frontend.SepEndBy1(p.internal, sep.internal, factory))
    }

    /** This combinator parses '''zero''' or more occurrences of `p`, separated and ended by `sep`.
      *
      * Behaves just like `endBy1`, except does not require an initial `p` and `sep`, returning the empty list instead.
      *
      * @example {{{
      * scala> ...
      * scala> val args = endBy(int, string(";\n"))
      * scala> args.parse("7;\n3;\n2")
      * val res0 = Failure(..)
      * scala> args.parse("")
      * val res1 = Success(Nil)
      * scala> args.parse("1;\n")
      * val res2 = Success(List(1))
      * scala> args.parse("1;\n2;\n")
      * val res3 = Success(List(1, 2))
      * }}}
      *
      * @param p the parser whose results are collected into a list.
      * @param sep the delimiter that must be parsed between every `p`.
      * @return a parser that parses `p` delimited by `sep`, returning the list of `p`'s results.
      * @group sep
      */
    final def endBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = endBy(p, sep, List)
    private [parsley] final def endBy[A, C](p: Parsley[A], sep: =>Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        many(p <* sep, factory)
    }

    /** This combinator parses '''one''' or more occurrences of `p`, separated and ended by `sep`.
      *
      * Parses `p` followed by `sep` one or more times.
      * The results of the `p`'s, `x,,1,,` through `x,,n,,`, are returned as `List(x,,1,,, .., x,,n,,)`.
      * If `p` or `sep` fails having consumed input, the whole parser fails.
      *
      * @example {{{
      * scala> ...
      * scala> val args = endBy1(int, string(";\n"))
      * scala> args.parse("7;\n3;\n2")
      * val res0 = Failure(..)
      * scala> args.parse("")
      * val res1 = Failure(..)
      * scala> args.parse("1;\n")
      * val res2 = Success(List(1))
      * scala> args.parse("1;\n2;\n")
      * val res3 = Success(List(1, 2))
      * }}}
      *
      * @param p the parser whose results are collected into a list.
      * @param sep the delimiter that must be parsed between every `p`.
      * @return a parser that parses `p` delimited by `sep`, returning the list of `p`'s results.
      * @group sep
      */
    final def endBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = some(p <* sep)
    private [parsley] final def endBy1[A, C](p: Parsley[A], sep: =>Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        some(p <* sep, factory)
    }

    /** This combinator repeatedly parses a given parser '''zero''' or more times, until the `end` parser succeeds, collecting the results into a list.
      *
      * First tries to parse `end`, if it fails '''without consuming input''', then parses `p`, which must succeed. This repeats until `end` succeeds.
      * When `end` does succeed, this combinator will return all of the results generated by `p`, `x,,1,,` through `x,,n,,` (with `n >= 0`), in a
      * list: `List(x,,1,,, .., x,,n,,)`. If `end` could be parsed immediately, the empty list is returned.
      *
      * @example This can be useful for scanning comments: {{{
      * scala> import parsley.character.{string, item, endOfLine}
      * scala> import parsley.combinator.manyTill
      * scala> val comment = string("//") *> manyTill(item, endOfLine)
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
      * @since 4.5.0
      */
    final def manyTill[A](p: Parsley[A], end: Parsley[_]): Parsley[List[A]] = manyTill(p, end, List)
    private [parsley] final def manyTill[A, C](p: Parsley[A], end: Parsley[_], factory: Factory[A, C]): Parsley[C] = {
        new Parsley(new frontend.ManyUntil((end.as(ManyUntil.Stop) <|> p: Parsley[Any]).internal, factory))
    }

    // TODO: find a way to make this redundant
    private [parsley] final def skipManyUntil(p: Parsley[_], end: Parsley[_]): Parsley[Unit] = {
        new Parsley(new frontend.SkipManyUntil((end.as(ManyUntil.Stop) <|> p.void: Parsley[Any]).internal))
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
      * scala> import parsley.combinator.someUntil
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
      * @since 4.5.0
      */
    final def someTill[A](p: Parsley[A], end: Parsley[_]): Parsley[List[A]] = {
        // similar to some, I think a someTill(init, p, end) could generalise both manyTill and someTill
        notFollowedBy(end) *> (p <::> manyTill(p, end))
    }

    /** This combinator parses one of `thenP` or `elseP` depending on the result of parsing `condP`.
      *
      * This is a lifted `if`-statement. First, parse `condP`: if it is successful and returns
      * `true`, then parse `thenP`; else, if it returned `false`, parse `elseP`; or, if `condP` failed
      * then fail. If either of `thenP` or `elseP` fail, then this combinator also fails.
      *
      * Most useful in conjunction with ''Registers'', as this allows for decisions to be made
      * based on state.
      *
      * @example {{{
      * ifS(pure(true), p, _) == p
      * ifS(pure(false), _, p) == p
      * }}}
      *
      * @param condP the parser that yields the condition value.
      * @param thenP the parser to execute if the condition is `true`.
      * @param elseP the parser to execute if the condition is `false.
      * @return a parser that conditionally parses `thenP` or `elseP` after `condP`.
      * @group condComp
      * @since 4.5.0
      */
    final def ifS[A](condP: Parsley[Boolean], thenP: =>Parsley[A], elseP: =>Parsley[A]): Parsley[A] = {
        new Parsley(new frontend.If(condP.internal, thenP.internal, elseP.internal))
    }

    /** This combinator conditionally parses `thenP` depending on the result of parsing `condP`.
      *
      * This is a lifted `if`-statement. First, parse `condP`: if it is successful and returns
      * `true`, then parse `thenP`; else, if it returned `false` do nothing; or, if `condP` failed
      * then fail. If `thenP` fails, then this combinator also fails.
      *
      * Most useful in conjunction with ''Registers'', as this allows for decisions to be made
      * based on state.
      *
      * @example {{{
      * whenS(pure(true), p) == p
      * whenS(pure(false), _) == unit
      * }}}
      *
      * @param condP the parser that yields the condition value.
      * @param thenP the parser to execute if the condition is `true`.
      * @return a parser that conditionally parses `thenP` after `condP`.
      * @group condComp
      */
    final def whenS(condP: Parsley[Boolean])(thenP: =>Parsley[Unit]): Parsley[Unit] = ifS(condP, thenP, pure(()))

    /** This combinator verfies that the given parser returns `true`, or else fails.
      *
      * First, parse `p`; if it succeeds then, so long at returns `true`, this `guard(p)` succeeds. Otherwise,
      * if `p` either fails, or returns `false`, `guard(p)` will fail.
      *
      * @example {{{
      * guard(pure(true)) == unit
      * guard(pure(false)) == empty
      * when(p.map(!_), empty) == guardS(p)
      * }}}
      *
      * @param p the parser that yields the condition value.
      * @group condComp
      */
    final def guardS(p: Parsley[Boolean]): Parsley[Unit] = ifS(p, pure(()), empty)

    /** This combinator repeatedly parses `p` so long as it returns `true`.
      *
      * This is a lifted `while`-loop. First, parse `p`: if it is successful and
      * returns `true`, then repeat; else if it returned `false` stop; or, if it
      * failed then this combinator fails.
      *
      * Most useful in conjunction with ''Registers'', as this allows for decisions to be made
      * based on state. In particular, this can be used to define the `forP` combinator.
      *
      * @example {{{
      * def forP[A](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: =>Parsley[_]): Parsley[Unit] = {
      *     val reg = Reg.make[A]
      *     lazy val _cond = reg.gets(cond)
      *     lazy val _step = reg.modify(step)
      *     reg.put(init) *> whenS(_cond, whileS(body *> _step *> _cond))
      * }
      * }}}
      *
      * @param p the parser to repeatedly parse.
      * @return a parser that continues to parse `p` until it returns `false`.
      * @group condComp
      */
    final def whileS(p: Parsley[Boolean]): Parsley[Unit] = {
        lazy val whileP: Parsley[Unit] = whenS(p)(whileP)
        whileP
    }

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
      * @group range
      * @since 4.0.0
      */
    final def exactly[A](n: Int, p: Parsley[A]): Parsley[List[A]] = exactly(n, p, List)
    private [parsley] final def exactly[A, C](n: Int, p: Parsley[A], factory: Factory[A, C]): Parsley[C] = {
        require(n > 0, "n must be greater than 0 for exactly")
        traverseGen(0, (1 until n): _*)(_ => p, factory)
    }

    /** This combinator parses between `min` and `max` occurrences of `p`, returning these `n` results in a list.
      *
      * Parses `p` repeatedly a minimum of `min` times and up to `max` times both inclusive. If `p` fails before
      * `min` is reached, then this combinator fails. It is not required for `p` to fail after the `max`^th^ parse. The results produced by
      * `p`, `x,,min,,` through `x,,max,,`, are returned as `List(x,,min,,, .., x,,max,,)`.
      *
      * @example {{{
      * scala> import parsley.character.item
      * scala> import parsley.combinator.range
      * scala> val p = range(min=3, max=5)(item)
      * scala> p.parse("ab")
      * val res0 = Failure(..)
      * scala> p.parse("abc")
      * val res1 = Success(List('a', 'b', 'c'))
      * scala> p.parse("abcd")
      * val res2 = Success(List('a', 'b', 'c', 'd'))
      * scala> p.parse("abcde")
      * val res2 = Success(List('a', 'b', 'c', 'd', 'e'))
      * scala> p.parse("abcdef")
      * val res2 = Success(List('a', 'b', 'c', 'd', 'e'))
      * }}}
      *
      * @param min the minimum number of times to repeat `p`, inclusive.
      * @param max the maximum number of times to repeat `p`, inclusive.
      * @param p the parser to repeat.
      * @return the results of the successful parses of `p`.
      * @group range
      * @since 4.4.0
      */
    final def range[A](min: Int, max: Int)(p: Parsley[A]): Parsley[List[A]] = fresh(mutable.ListBuffer.empty[A]).persist { xs =>
        count(min, max)((xs, p).zipped(_ += _).impure) ~>
        xs.map(_.toList)
    }

    /** This combinator parses between `min` and `max` occurrences of `p`, returning the number of successes.
      *
      * Parses `p` repeatedly a minimum of `min` times and up to `max` times both inclusive. If `p` fails before
      * `min` is reached, then this combinator fails. It is not required for `p` to fail after the `max`^th^ parse.
      * The results are discarded and the number of successful parses of `p`, `n`, is returned instead, such that
      * `min <= n <= max`.
      *
      * @example {{{
      * scala> import parsley.character.item
      * scala> import parsley.combinator.count
      * scala> val p = count(min=3, max=5)(item)
      * scala> p.parse("ab")
      * val res0 = Failure(..)
      * scala> p.parse("abc")
      * val res1 = Success(3)
      * scala> p.parse("abcd")
      * val res2 = Success(4)
      * scala> p.parse("abcde")
      * val res2 = Success(5)
      * scala> p.parse("abcdef")
      * val res2 = Success(5)
      * }}}
      *
      * @param min the minimum number of times to repeat `p`, inclusive.
      * @param max the maximum number of times to repeat `p`, inclusive.
      * @param p the parser to repeat.
      * @return the number of times `p` parsed successfully.
      * @group range
      * @since 4.4.0
      */
    final def count(min: Int, max: Int)(p: Parsley[_]): Parsley[Int] = min.makeRef { i =>
        exactly(min, p) ~>
        Parsley.many(guardS(i.gets(_ < max)) ~> p ~> i.update(_ + 1)) ~>
        i.get
    }
}
