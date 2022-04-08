package parsley

import parsley.Parsley.{unit, empty, pure, select, notFollowedBy, attempt}
import parsley.internal.deepembedding.{singletons, frontend}
import parsley.expr.chain
import scala.annotation.{tailrec, implicitNotFound}

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
  *     these combinators, with the exception of `[[option]]`, which still preserves the result.
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
  *     right-hand side, the remaining variadic arguments will be kept lazily suspended until later.
  *
  * @groupprio cond 75
  * @groupname cond Conditional Combinators
  * @groupdesc cond
  *     These combinators allow for the conditional extraction of a result, or the execution of a parser
  *     based on another. They are morally related to `[[Parsley.branch]]` and `[[Parsley.select]]` but are
  *     less fundamental.
  *
  * @groupprio misc 100
  * @groupname misc Miscellaneous
  */
object combinator {
    /** `choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      * Returns the value of the succeeding parser.
      *
      * @group multi
      */
    def choice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceRightOption(_ <|> _).getOrElse(empty)

    /** `attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      * Returns the value of the succeeding parser. Utilises `attempt p <|> q` vs choice's `p <|> q`.
      *
      * @group multi
      */
    def attemptChoice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceRightOption((p, q) => attempt(p) <|> q).getOrElse(empty)
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, collecting the results.
      * @param ps Parsers to be sequenced
      * @return The list containing results, one from each parser, in order
      * @group multi
      */
    def sequence[A](ps: Parsley[A]*): Parsley[List[A]] = ps.foldRight(pure[List[A]](Nil))(_ <::> _)
    /**
      * Like `sequence` but produces a list of parsers to sequence by applying the function `f` to each
      * element in `xs`.
      * @param f The function to map on each element of `xs` to produce parsers
      * @param xs Values to generate parsers from
      * @return The list containing results formed by executing each parser generated from `xs` and `f` in sequence
      * @group multi
      */
    def traverse[A, B](f: A => Parsley[B], xs: A*): Parsley[List[B]] = sequence(xs.map(f): _*)
    /**
      * Evaluate each of the parsers in `ps` sequentially from left to right, ignoring the results.
      * @param ps Parsers to be performed
      * @group multi
      */
    def skip(ps: Parsley[_]*): Parsley[Unit] = ps.foldRight(unit)(_ *> _)

    /** `exactly(n, p)` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser is
      * `pure(Nil)`. Returns a list of `n` values returned by `p`.
      *
      * @group misc
      */
    def exactly[A](n: Int, p: Parsley[A]): Parsley[List[A]] = sequence((for (_ <- 1 to n) yield p): _*)

    /** `option(p)` tries to apply parser `p`. If `p` fails without consuming input, it returns
      * `None`, otherwise it returns `Some` of the value returned by `p`.
      *
      * @group opt
      */
    def option[A](p: Parsley[A]): Parsley[Option[A]] = p.map(Some(_)).getOrElse(None)

    /** `decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.
      *
      * @group cond
      */
    def decide[A](p: Parsley[Option[A]]): Parsley[A] = p.collect {
        case Some(x) => x
    }

    /** `decide(p, q)` removes the option from inside parser `p`, if it returned `None` then `q` is executed.
      *
      * @group cond
      */
    def decide[A](p: Parsley[Option[A]], q: =>Parsley[A]): Parsley[A] = select(p.map(_.toRight(())), q.map(x => (_: Unit) => x))

    /** `optional(p)` tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
      * fails after consuming input. It discards the result of `p`.
      *
      * @group opt
      */
    def optional(p: Parsley[_]): Parsley[Unit] = optionalAs(p, ())

    /** `optionalAs(p, x)` tries to apply parser `p`. It will always result in `x` regardless of
      * whether or not `p` succeeded or `p` failed without consuming input.
      *
      * @group opt
      */
    def optionalAs[A](p: Parsley[_], x: A): Parsley[A] = {
        (p #> x).getOrElse(x)
    }

    /** `between(open, close, p)` parses `open`, followed by `p` and `close`. Returns the value returned by `p`.
      *
      * @group misc
      */
    def between[A](open: Parsley[_], close: =>Parsley[_], p: =>Parsley[A]): Parsley[A] = open *> p <* close

    /** `many(p)` executes the parser `p` zero or more times. Returns a list of the returned values of `p`.
      *
      * @since 2.2.0
      * @group iter
      */
    def many[A](p: Parsley[A]): Parsley[List[A]] = new Parsley(new frontend.Many(p.internal))

    /** `some(p)` applies the parser `p` '''one''' or more times. Returns a list of the returned values of `p`.
      *
      * @group iter
      */
    def some[A](p: Parsley[A]): Parsley[List[A]] = manyN(1, p)

    /** `manyN(n, p)` applies the parser `p` *n* or more times. Returns a list of the returned values of `p`.
      *
      * @group iter
      */
    def manyN[A](n: Int, p: Parsley[A]): Parsley[List[A]] = {
        @tailrec def go(n: Int, acc: Parsley[List[A]] = many(p)): Parsley[List[A]] = {
            if (n == 0) acc
            else go(n-1, p <::> acc)
        }
        go(n)
    }

    /** `skipMany(p)` executes the parser `p` zero or more times and ignores the results.
      *
      * @since 2.2.0
      * @group iter
      */
    def skipMany[A](p: Parsley[A]): Parsley[Unit] = new Parsley(new frontend.SkipMany(p.internal))

    /** `skipSome(p)` applies the parser `p` '''one''' or more times, skipping its result.
      *
      * @group iter
      */
    def skipSome[A](p: Parsley[A]): Parsley[Unit] = skipManyN(1, p)

    /** `skipManyN(n, p)` applies the parser `p` *n* or more times, skipping its result.
      *
      * @group iter
      */
    def skipManyN[A](n: Int, p: Parsley[A]): Parsley[Unit] = {
        @tailrec def go(n: Int, acc: Parsley[Unit] = skipMany(p)): Parsley[Unit] = {
            if (n == 0) acc
            else go(n-1, p *> acc)
        }
        go(n)
    }

    /** `sepBy(p, sep)` parses '''zero''' or more occurrences of `p`, separated by `sep`.
      *
      * @group sep
      */
    def sepBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepBy1(p, sep).getOrElse(Nil)

    /** `sepBy1(p, sep)` parses '''one''' or more occurrences of `p`, separated by `sep`.
      *
      * @group sep
      */
    def sepBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = {
        p <::> many(sep *> p)
    }

    /** `sepEndBy(p, sep)` parses '''zero''' or more occurrences of `p`, separated and optionally ended
      * by `sep`.
      *
      * @group sep
      */
    def sepEndBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = sepEndBy1(p, sep).getOrElse(Nil)

    /** `sepEndBy1(p, sep)` parses '''one''' or more occurrences of `p`, separated and optionally ended
      * by `sep`.
      *
      * @group sep
      */
    def sepEndBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = new Parsley(new frontend.SepEndBy1(p.internal, sep.internal))

    /** `endBy(p, sep)` parses '''zero''' or more occurrences of `p`, separated and ended by `sep`.
      *
      * @group sep
      */
    def endBy[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = many(p <* sep)

    /** `endBy1(p, sep)` parses '''one''' or more occurrences of `p`, separated and ended by `sep`.
      *
      * @group sep
      */
    def endBy1[A](p: Parsley[A], sep: =>Parsley[_]): Parsley[List[A]] = some(p <* sep)

    /** This parser only succeeds at the end of the input. This is a primitive parser.
      *
      * @group item
      */
    val eof: Parsley[Unit] = new Parsley(singletons.Eof)

    /** This parser only succeeds if there is still more input.
      *
      * @group item
      */
    val more: Parsley[Unit] = notFollowedBy(eof)

    /** `manyUntil(p, end)` applies parser `p` zero or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`. This parser can be used to scan comments.
      *
      * @group iter
      */
    def manyUntil[A](p: Parsley[A], end: Parsley[_]): Parsley[List[A]] = {
        new Parsley(new frontend.ManyUntil((end #> ManyUntil.Stop <|> p).internal))
    }

    private [parsley] object ManyUntil {
        object Stop
    }

    /** `someUntil(p, end)` applies parser `p` one or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`.
      *
      * @group iter
      */
    def someUntil[A](p: Parsley[A], end: Parsley[_]): Parsley[List[A]] = {
        notFollowedBy(end) *> (p <::> manyUntil(p, end))
    }

    /**
      * This is an if statement lifted to the parser level.
      *
      * @param b The parser that yields the condition value
      * @param p
      * @param q
      * @group cond
      */
    def ifP[A](b: Parsley[Boolean], p: =>Parsley[A], q: =>Parsley[A]): Parsley[A] = {
        new Parsley(new frontend.If(b.internal, p.internal, q.internal))
    }

    /** `when(p, q)` will first perform `p`, and if the result is `true` will then execute `q` or else return unit.
      * @param p The first parser to parse
      * @param q If `p` returns `true` then this parser is executed
      * @group cond
      */
    def when(p: Parsley[Boolean], q: =>Parsley[Unit]): Parsley[Unit] = ifP(p, q, unit)

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