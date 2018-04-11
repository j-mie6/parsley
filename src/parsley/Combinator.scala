package parsley

import parsley.Parsley._
import scala.annotation.tailrec

object Combinator
{
    /**`choice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser.*/
    def choice[A](ps: List[Parsley[A]]): Parsley[A] = ps.reduceLeftOption(_<|>_).getOrElse(empty)

    /**`attemptChoice(ps)` tries to apply the parsers in the list `ps` in order, until one of them succeeds.
      *  Returns the value of the succeeding parser. Utilises <\> vs choice's <|>.*/
    def attemptChoice[A](ps: List[Parsley[A]]): Parsley[A] = ps.reduceLeftOption(_<\>_).getOrElse(empty)

    /** `repeat(n, p)` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the parser is
      *  `pure(Nil)`. Returns a list of `n` values returned by `p`.*/
    def repeat[A](n: Int, p: =>Parsley[A]): Parsley[List[A]] =
    {
        lazy val _p = p
        sequence(for (_ <- 1 to n) yield _p)
    }

    /**`option(p)` tries to apply parser `p`. If `p` fails without consuming input, it returns
      * `None`, otherwise it returns `Some` of the value returned by `p`.*/
    def option[A](p: =>Parsley[A]): Parsley[Option[A]] = p.map(Some(_)).getOrElse(None)

    /**`decide(p)` removes the option from inside parser `p`, and if it returned `None` will fail.*/
    def decide[A](p: =>Parsley[Option[A]]): Parsley[A] = for (opt <- p; if opt.isDefined) yield opt.get

    /**`optional(p)` tries to apply parser `p`. It will parse `p` or nothing. It only fails if `p`
      * fails after consuming input. It discards the result of `p`.*/
    def optional[A](p: =>Parsley[A]): Parsley[Unit] = (p *> unit).getOrElse(())

    /**`between(open, close, p)` parses `open`, followed by `p` and `close`. Returns the value returned by `p`.*/
    def between[O, C, A](open: =>Parsley[O],
                         close: =>Parsley[C],
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
    def sepEndBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = new DeepEmbedding.SepEndBy1(p, sep)

    /**`endBy(p, sep)` parses *zero* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = many(p <* sep)

    /**`endBy1(p, sep)` parses *one* or more occurrences of `p`, separated and ended by `sep`. Returns a list
      * of values returned by `p`.*/
    def endBy1[A, B](p: =>Parsley[A], sep: =>Parsley[B]): Parsley[List[A]] = some(p <* sep)

    /**`chainr(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainr[A](p: =>Parsley[A], op: =>Parsley[(A, A) => A], x: A): Parsley[A] = chainr1(p, op).getOrElse(x)

    /**`chainl(p, op, x)` parses *zero* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions returned by `op` to the values
      * returned by `p`. If there are no occurrences of `p`, the value `x` is returned.*/
    def chainl[A](p: =>Parsley[A], op: =>Parsley[(A, A) => A], x: A): Parsley[A] = chainl1(p, op).getOrElse(x)

    /**`chainr1(p, op)` parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a right associative application of all functions return by `op` to the values
      * returned by `p`.*/
    def chainr1[A](p: =>Parsley[A], op: =>Parsley[(A, A) => A]): Parsley[A] = new DeepEmbedding.Chainr(p, op)

    /**`chainPre(p, op)` parses many prefixed applications of `op` onto a single final result of `p`*/
    def chainPre[A](p: =>Parsley[A], op: =>Parsley[A => A]): Parsley[A] = new DeepEmbedding.ChainPre(p, op)

    /**chainl1(p, op) parses *one* or more occurrences of `p`, separated by `op`. Returns a value
      * obtained by a left associative application of all functions return by `op` to the values
      * returned by `p`. This parser can for example be used to eliminate left recursion which
      * typically occurs in expression grammars.*/
    def chainl1[A](p: =>Parsley[A], op: =>Parsley[(A, A) => A]): Parsley[A] = new DeepEmbedding.Chainl(p, op)

    /**`chainPost(p, op)` parses one occurrence of `p`, followed by many postfix applications of `op`
      * that associate to the left.*/
    def chainPost[A](p: =>Parsley[A], op: =>Parsley[A => A]) = new DeepEmbedding.ChainPost(p, op)

    /**This parser only succeeds at the end of the input. This is a primitive parser.*/
    val eof: Parsley[Unit] = new DeepEmbedding.*>(new DeepEmbedding.Eof, unit)

    /**`notFollowedBy(p)` only succeeds when parser `p` fails. This parser does not consume any input.
      * This parser can be used to implement the 'longest match' rule. For example, when recognising
      * keywords, we want to make sure that a keyword is not followed by a legal identifier character,
      * in which case the keyword is actually an identifier. We can program this behaviour as follows:
      * {{{attempt(kw *> notFollowedBy(alphaNum))}}}*/
    def notFollowedBy(p: Parsley[_]): Parsley[Unit] = new DeepEmbedding.*>(new DeepEmbedding.NotFollowedBy(p), unit)

    /**`manyTill(p, end)` applies parser `p` zero or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`. This parser can be used to scan comments.*/
    def manyTill[A, B](p: =>Parsley[A], end: =>Parsley[B]): Parsley[List[A]] =
    {
        new DeepEmbedding.ManyTill(end #> DeepEmbedding.ManyTill.Stop <|> p)
    }

    /**`many1Till(p, end)` applies parser `p` one or more times until the parser `end` succeeds.
      * Returns a list of values returned by `p`.*/
    def many1Till[A, B](p: =>Parsley[A], end: =>Parsley[B]): Parsley[List[A]] =
    {
        lazy val _p = p
        lazy val _end = end
        notFollowedBy(_end) *> (_p <::> manyTill(_p, _end))
    }
}