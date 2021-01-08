package parsley

import parsley.Parsley.{LazyParsley, get, gets, put, local, unit, many, skipMany, empty, select, sequence}
import parsley.internal.deepembedding
import scala.annotation.{tailrec, implicitNotFound}

object Combinator
{
    /**`choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser.*/
    def choice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceLeftOption(_<|>_).getOrElse(empty)

    /**`attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser. Utilises `<\>` vs choice's `<|>`.*/
    def attemptChoice[A](ps: Parsley[A]*): Parsley[A] = ps.reduceLeftOption(_<\>_).getOrElse(empty)

    /** `repeat(n, p)` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser is
      *  `pure(Nil)`. Returns a list of `n` values returned by `p`.*/
    def repeat[A](n: Int, p: =>Parsley[A]): Parsley[List[A]] =
    {
        lazy val _p = p
        sequence((for (_ <- 1 to n) yield _p): _*)
    }

    /**`option(p)` tries to apply parser `p`. If `p` fails without consuming input, it returns
      * `None`, otherwise it returns `Some` of the value returned by `p`.*/
    def option[A](p: =>Parsley[A]): Parsley[Option[A]] = p.map(Some(_)).getOrElse(None)

    /**`decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.*/
    def decide[A](p: =>Parsley[Option[A]]): Parsley[A] = p.collect {
        case Some(x) => x
    }

    /**`decide(p, q)` removes the option from inside parser `p`, if it returned `None` then `q` is executed.*/
    def decide[A](p: =>Parsley[Option[A]], q: =>Parsley[A]): Parsley[A] = decide(p).orElse(q)

    /**optional(p) tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
      * fails after consuming input. It discards the result of `p`.*/
    def optional(p: =>Parsley[_]): Parsley[Unit] = optionally(p, ())

    /**optionally(p, x) tries to apply parser `p`. It will always result in `x` regardless of
      * whether or not `p` succeeded or `p` failed without consuming input.*/
    def optionally[A](p: =>Parsley[_], x: =>A): Parsley[A] =
    {
        lazy val _x = x
        (p #> x).getOrElse(x)
    }

    /**`between(open, close, p)` parses `open`, followed by `p` and `close`. Returns the value returned by `p`.*/
    def between[A](open: =>Parsley[_],
                   close: =>Parsley[_],
                   p: =>Parsley[A]): Parsley[A] = open *> p <* close

    /**`some(p)` applies the parser `p` *one* or more times. Returns a list of the returned values of `p`.*/
    def some[A](p: =>Parsley[A]): Parsley[List[A]] = manyN(1, p)

    /**`manyN(n, p)` applies the parser `p` *n* or more times. Returns a list of the returned values of `p`.*/
    def manyN[A](n: Int, p: =>Parsley[A]): Parsley[List[A]] =
    {
        lazy val _p = p
        @tailrec def go(n: Int, acc: Parsley[List[A]] = many(_p)): Parsley[List[A]] =
        {
            if (n == 0) acc
            else go(n-1, _p <::> acc)
        }
        go(n)
    }

    /**`skipSome(p)` applies the parser `p` *one* or more times, skipping its result.*/
    def skipSome[A](p: => Parsley[A]): Parsley[Unit] = skipManyN(1, p)

    /**`skipManyN(n, p)` applies the parser `p` *n* or more times, skipping its result.*/
    def skipManyN[A](n: Int, p: =>Parsley[A]): Parsley[Unit] =
    {
        lazy val _p = p
        @tailrec def go(n: Int, acc: Parsley[Unit] = skipMany(_p)): Parsley[Unit] =
        {
            if (n == 0) acc
            else go(n-1, _p *> acc)
        }
        go(n)
    }

    /**`sepBy(p, sep)` parses *zero* or more occurrences of `p`, separated by `sep`. Returns a list
      * of values returned by `p`.*/
    def sepBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = sepBy1(p, sep).getOrElse(Nil)

    /**`sepBy1(p, sep)` parses *one* or more occurrences of `p`, separated by `sep`. Returns a list
      *  of values returned by `p`.*/
    def sepBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] =
    {
        lazy val _p = p
        lazy val _sep = sep
        _p <::> many(_sep *> _p)
    }

    /**`sepEndBy(p, sep)` parses *zero* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    def sepEndBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = sepEndBy1(p, sep).getOrElse(Nil)

    /**`sepEndBy1(p, sep)` parses *one* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    def sepEndBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = new Parsley(new deepembedding.SepEndBy1(p.internal, sep.internal))

    /**`endBy(p, sep)` parses *zero* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = many(p <* sep)

    /**`endBy1(p, sep)` parses *one* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = some(p <* sep)

    /**`chainr(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainr[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B], x: B)
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = chainr1(p, op).getOrElse(x)

    /**`chainl(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainl[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B], x: B)
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = chainl1(p, op).getOrElse(x)

    /**`chainr1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.*/
    def chainr1[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B])
                     (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        new Parsley(new deepembedding.Chainr(p.internal, op.internal, wrap))
    }

    /**`chainPre(op, p)` parses many prefixed applications of `op` onto a single final result of `p`*/
    def chainPre[A](op: =>Parsley[A => A], p: =>Parsley[A]): Parsley[A] = new Parsley(new deepembedding.ChainPre(p.internal, op.internal))

    /**chainl1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.*/
    def chainl1[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B])
                     (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = {
        new Parsley(new deepembedding.Chainl(new deepembedding.Subroutine(p.internal), op.internal, wrap))
    }

    /**`chainPost(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.*/
    def chainPost[A](p: =>Parsley[A], op: =>Parsley[A => A]): Parsley[A] = new Parsley(new deepembedding.ChainPost(p.internal, op.internal))

    /**This parser only succeeds at the end of the input. This is a primitive parser.*/
    val eof: Parsley[Unit] = new Parsley(new deepembedding.Eof)

    /**This parser only succeeds if there is still more input.*/
    val more: Parsley[Unit] = notFollowedBy(eof)

    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new Parsley(new deepembedding.NotFollowedBy(p.internal))

    /**`manyUntil(p, end)` applies parser `p` zero or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`. This parser can be used to scan comments.*/
    def manyUntil[A, B](p: =>Parsley[A], end: =>Parsley[B]): Parsley[List[A]] =
    {
        new Parsley(new deepembedding.ManyUntil((end #> deepembedding.ManyUntil.Stop <|> p).internal))
    }

    /**`someUntil(p, end)` applies parser `p` one or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`.*/
    def someUntil[A, B](p: =>Parsley[A], end: =>Parsley[B]): Parsley[List[A]] =
    {
        lazy val _p = p
        lazy val _end = end
        notFollowedBy(_end) *> (_p <::> manyUntil(_p, _end))
    }

    /** `when(p, q)` will first perform `p`, and if the result is `true` will then execute `q` or else return unit.
      * @param p The first parser to parse
      * @param q If `p` returns `true` then this parser is executed
      * @return ()
      */
    def when(p: =>Parsley[Boolean], q: =>Parsley[Unit]): Parsley[Unit] = p ?: (q, unit)

    /** `whileP(p)` will continue to run `p` until it returns `false`. This is often useful in conjunction with stateful
      * parsers.
      * @param p The parser to continuously execute
      * @return ()
      */
    def whileP(p: =>Parsley[Boolean]): Parsley[Unit] =
    {
        lazy val whilePP: Parsley[Unit] = when(p, whilePP)
        whilePP
    }

    /** `forP(v, init, cond, step, body)` behaves much like a traditional for loop using variable `v` as the loop
      * variable and `init`, `cond`, `step` and `body` as parsers which control the loop itself. This is useful for
      * performing certain context sensitive tasks. For instance, to read an equal number of as, bs and cs you can do:
      *
      * {{{
      * put(v1, 0) *>
      * many('a' *> modify[Int](v1, _+1)) *>
      * forP[Int](v2, get[Int](v1), pure(_ != 0), pure(_ - 1), 'b') *>
      * forP[Int](v2, get[Int](v1), pure(_ != 0), pure(_ - 1), 'c')
      * }}}
      *
      * The value of `v` is reset on exiting this parser. This is to preserve the limited register numbers.
      *
      * @param v The address the induction variable is stored in
      * @param init The initial value that register v should take
      * @param cond The condition by which the loop terminates
      * @param step The change in induction variable on each iteration
      * @param body The body of the loop performed each iteration
      * @return ()
      */
    // TODO: We can put this back for Parsley 2.1, because the new version will not have a `v` parameter
    /*def forP[A](r: Reg[A], init: =>Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A], body: =>Parsley[_]): Parsley[Unit] =
    {
        val _cond = gets(v, cond)
        val _step = put(v, gets(v, step))
        local(v, init, when(_cond, whileP(body *> _step *> _cond)))
    }*/
}