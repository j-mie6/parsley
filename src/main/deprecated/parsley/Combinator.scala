package parsley

import parsley.Parsley.{LazyParsley, get, gets, put, local, unit, many, skipMany, empty, select, sequence}
import parsley.internal.deepembedding
import parsley.expr.chain
import scala.annotation.{tailrec, implicitNotFound}

// $COVERAGE-OFF$
object Combinator
{
    /**`choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.choice` instead", "v2.2.0")
    def choice[A](ps: Parsley[A]*): Parsley[A] = combinator.choice(ps: _*)

    /**`attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser. Utilises `<\>` vs choice's `<|>`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.attemptChoice` instead", "v2.2.0")
    def attemptChoice[A](ps: Parsley[A]*): Parsley[A] = combinator.attemptChoice(ps: _*)

    /** `repeat(n, p)` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser is
      *  `pure(Nil)`. Returns a list of `n` values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.repeat` instead", "v2.2.0")
    def repeat[A](n: Int, p: =>Parsley[A]): Parsley[List[A]] =
    {
        lazy val _p = p
        sequence((for (_ <- 1 to n) yield _p): _*)
    }

    /**`option(p)` tries to apply parser `p`. If `p` fails without consuming input, it returns
      * `None`, otherwise it returns `Some` of the value returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.option` instead", "v2.2.0")
    def option[A](p: =>Parsley[A]): Parsley[Option[A]] = p.map(Some(_)).getOrElse(None)

    /**`decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.decide` instead", "v2.2.0")
    def decide[A](p: =>Parsley[Option[A]]): Parsley[A] = p.collect {
        case Some(x) => x
    }

    /**`decide(p, q)` removes the option from inside parser `p`, if it returned `None` then `q` is executed.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.decide` instead", "v2.2.0")
    def decide[A](p: =>Parsley[Option[A]], q: =>Parsley[A]): Parsley[A] = decide(p).orElse(q)

    /**optional(p) tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
      * fails after consuming input. It discards the result of `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.optional` instead", "v2.2.0")
    def optional(p: =>Parsley[_]): Parsley[Unit] = optionally(p, ())

    /**optionally(p, x) tries to apply parser `p`. It will always result in `x` regardless of
      * whether or not `p` succeeded or `p` failed without consuming input.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.optionally` instead", "v2.2.0")
    def optionally[A](p: =>Parsley[_], x: =>A): Parsley[A] =
    {
        lazy val _x = x
        (p #> x).getOrElse(x)
    }

    /**`between(open, close, p)` parses `open`, followed by `p` and `close`. Returns the value returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.between` instead", "v2.2.0")
    def between[A](open: =>Parsley[_],
                   close: =>Parsley[_],
                   p: =>Parsley[A]): Parsley[A] = open *> p <* close

    /**`some(p)` applies the parser `p` *one* or more times. Returns a list of the returned values of `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.some` instead", "v2.2.0")
    def some[A](p: =>Parsley[A]): Parsley[List[A]] = manyN(1, p)

    /**`manyN(n, p)` applies the parser `p` *n* or more times. Returns a list of the returned values of `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.manyN` instead", "v2.2.0")
    def manyN[A](n: Int, p: =>Parsley[A]): Parsley[List[A]] = {
        lazy val _p = p
        @tailrec def go(n: Int, acc: Parsley[List[A]] = many(_p)): Parsley[List[A]] = {
            if (n == 0) acc
            else go(n-1, _p <::> acc)
        }
        go(n)
    }

    /**`skipSome(p)` applies the parser `p` *one* or more times, skipping its result.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.skipSome` instead", "v2.2.0")
    def skipSome[A](p: => Parsley[A]): Parsley[Unit] = skipManyN(1, p)

    /**`skipManyN(n, p)` applies the parser `p` *n* or more times, skipping its result.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.skipManyN` instead", "v2.2.0")
    def skipManyN[A](n: Int, p: =>Parsley[A]): Parsley[Unit] = {
        lazy val _p = p
        @tailrec def go(n: Int, acc: Parsley[Unit] = skipMany(_p)): Parsley[Unit] = {
            if (n == 0) acc
            else go(n-1, _p *> acc)
        }
        go(n)
    }

    /**`sepBy(p, sep)` parses *zero* or more occurrences of `p`, separated by `sep`. Returns a list
      * of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.sepBy` instead", "v2.2.0")
    def sepBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = sepBy1(p, sep).getOrElse(Nil)

    /**`sepBy1(p, sep)` parses *one* or more occurrences of `p`, separated by `sep`. Returns a list
      *  of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.sepBy1` instead", "v2.2.0")
    def sepBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = {
        lazy val _p = p
        lazy val _sep = sep
        _p <::> many(_sep *> _p)
    }

    /**`sepEndBy(p, sep)` parses *zero* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.sepEndBy` instead", "v2.2.0")
    def sepEndBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = sepEndBy1(p, sep).getOrElse(Nil)

    /**`sepEndBy1(p, sep)` parses *one* or more occurrences of `p`, separated and optionally ended
      * by `sep`. Returns a list of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.sepEndBy1` instead", "v2.2.0")
    def sepEndBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = new Parsley(new deepembedding.SepEndBy1(p.internal, sep.internal))

    /**`endBy(p, sep)` parses *zero* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.endBy` instead", "v2.2.0")
    def endBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = many(p <* sep)

    /**`endBy1(p, sep)` parses *one* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.endBy1` instead", "v2.2.0")
    def endBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = some(p <* sep)

    // $COVERAGE-OFF$
    /**`chainr(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.expr.chain.right` instead", "v2.2.0")
    def chainr[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B], x: B)
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = chain.right(p, op, x)

    /**`chainl(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.expr.chain.left` instead", "v2.2.0")
    def chainl[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B], x: B)
                    (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = chain.left(p, op, x)

    /**`chainr1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.expr.chain.right1` instead", "v2.2.0")
    def chainr1[A, B](p: =>Parsley[A], op: =>Parsley[(A, B) => B])
                     (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = chain.right1(p, op)

    /**chainl1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.expr.chain.left1` instead", "v2.2.0")
    def chainl1[A, B](p: =>Parsley[A], op: =>Parsley[(B, A) => B])
                     (implicit @implicitNotFound("Please provide a wrapper function from ${A} to ${B}") wrap: A => B): Parsley[B] = chain.left1(p, op)

    /**`chainPre(op, p)` parses many prefixed applications of `op` onto a single final result of `p`*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.expr.chain.prefix` instead", "v2.2.0")
    def chainPre[A](op: =>Parsley[A => A], p: =>Parsley[A]): Parsley[A] = chain.prefix(op, p)

    /**`chainPost(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.expr.chain.postfix` instead", "v2.2.0")
    def chainPost[A](p: =>Parsley[A], op: =>Parsley[A => A]): Parsley[A] = chain.postfix(p, op)
    // $COVERAGE-ON$

    /**This parser only succeeds at the end of the input. This is a primitive parser.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.eof` instead", "v2.2.0")
    val eof: Parsley[Unit] = new Parsley(new deepembedding.Eof)

    /**This parser only succeeds if there is still more input.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.more` instead", "v2.2.0")
    val more: Parsley[Unit] = notFollowedBy(eof)

    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.notFollowedBy` instead", "v2.2.0")
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new Parsley(new deepembedding.NotFollowedBy(p.internal))

    /**`manyUntil(p, end)` applies parser `p` zero or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`. This parser can be used to scan comments.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.manyUntil` instead", "v2.2.0")
    def manyUntil[A, B](p: =>Parsley[A], end: =>Parsley[B]): Parsley[List[A]] =
    {
        new Parsley(new deepembedding.ManyUntil((end #> deepembedding.ManyUntil.Stop <|> p).internal))
    }

    /**`someUntil(p, end)` applies parser `p` one or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`.*/
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.someUntil` instead", "v2.2.0")
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
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.when` instead", "v2.2.0")
    def when(p: =>Parsley[Boolean], q: =>Parsley[Unit]): Parsley[Unit] = p ?: (q, unit)

    /** `whileP(p)` will continue to run `p` until it returns `false`. This is often useful in conjunction with stateful
      * parsers.
      * @param p The parser to continuously execute
      * @return ()
      */
    @deprecated("This method will be removed in Parsley 3.0, use `parsley.combinator.whileP` instead", "v2.2.0")
    def whileP(p: =>Parsley[Boolean]): Parsley[Unit] =
    {
        lazy val whilePP: Parsley[Unit] = when(p, whilePP)
        whilePP
    }
}
// $COVERAGE-ON$