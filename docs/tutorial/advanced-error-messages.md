# Advanced Error Messages

@:callout(info)
This page is still being updated for the wiki port, so some things may be a bit broken or look a little strange.
@:@

@:callout(warning)
_This page is out-of-date, and describes the situation in `parsley-3.x.y` and not `parsley-4.0.0`. Much of the content remains the same, but some individual things may be incorrect._
@:@

Previously, we saw the most basic approach to improving error messages: `.label` and `.explain`.
However, the other tools I listed in the post are valuable in their own right, but can be slightly
less common. I would like to demonstrate their use here, as well as some more advanced patterns.
Let's recap who the remaining players are:

* `fail` is useful, but a bit of a sledgehammer. When `fail` (or any of its derivative combinators
  like `guardAgainst`) is used, it removes the unexpected and expected information and just replaces it
  with a given message. If there are multiple `fail`s that appear in the same message, they are
  each written on a newline.
* `unexpected` is the least commonly used combinator in practice. When it is used, it will,
  like `fail`, immediately fails except it reports a custom unexpected message.
* `amend` and `entrench` are a pair of combinators that work together to correct the position of
  some error messages. These are quite rare in practice.

This post will cover the use of these combinators and a couple of useful patterns to provide very
fine grained error messages.

## Using `amend` and `entrench`
I'm going to start by introducing this dynamic duo, as they can be very useful in the right
circumstances. In fact, let's revisit the `identifier` parser we made when we did _Effective Lexing_:

```scala
object lexer {
    private val keywords = Set("while", "then", "else")

    private def lexeme[A](p: Parsley[A]): Parsley[A] = p <* skipWhitespace
    private def token[A](p: Parsley[A]): Parsley[A] = lexeme(atomic(p))

    val identifier =
        token {
            some(alphaNum).map(_.mkString).filterOut {
                case v if keywords(v) =>
                    s"keyword $v may not be used as an identifier"
            }
        }.label("identifier")
}
```

To recap, the idea behind this parser is that it first reads some alpha-numeric characters, then
converts them to a string. After this it ensures that the parsed identifier is not in `keywords`.
The `atomic` is wrapped round that entire block so that, if we did read a keyword, we are able to
backtrack out in case the keyword was indeed valid (for another branch). After we are done we can
read whitespace. This time, let's take a look at the error messages when it goes wrong:

```
lexer.identifier.parse("then")
(line 1, column 5):
  unexpected end of input
  expected identifier
  keyword then may not be used as an identifier
  >then
       ^
```

Cool! But there is something bugging me about this message. It's pointing at column 5, but since we
used `atomic` in `token`, surely no input was consumed? Let's verify:

```
(lexer.identifier <|> "then").parse("then")
Success("then")
```

Yup, it can backtrack out, so this is perhaps a little misleading. But you can argue that, since it
doesn't affect the working of the parser surely it's fine? Well, let's take a look at another error
message:

```
(lexer.identifier <|> "then").parse("while")
(line 1, column 6):
  unexpected end of input
  expected identifier
  keyword while may not be used as an identifier
  >while
        ^
```

Now, this might seem fine at first look, but the expected clause is _wrong_. We expect either an
identifier _or_ `then`, but the message doesn't reflect this. The problem is that when Parsley looks
to unify error messages, it will always favour error messages that happened later in the input.
identifier fails at column 6, but `then` fails at column 1. So, how to proceed? Well, we need a way
of either having consumed no input before we perform the filter, or we need to adjust the position
of the error message. I'm actually going to show both approaches, even though one is clearly better
than the other, purely so you can see the technique: it might come in handy for all I know.

Let's start by fixing the error message as opposed to fiddling with the way the parser works. Here
is where our new friend `amend` comes in. What `amend` does is, simply put, change the positions of
error messages that occur inside it so that they happen at the same position that the combinator
started to be executed at. Let's see the effect in action:

```scala
val identifier =
    token {
        amend {
            some(alphaNum).map(_.mkString).filterOut {
                case v if keywords(v) =>
                    s"keyword $v may not be used as an identifier"
            }
        }
    }.label("identifier")
```

Easy! Now, what happens to the error?

```
(lexer.identifier <|> "then").parse("while")
(line 1, column 1):
  unexpected "whil"
  expected "then" or identifier
  keyword while may not be used as an identifier
  >while
   ^
```

Great! So, what happens is the following: first we enter the `token`, and then the `amend`. At
this point, no input has been consumed, so `amend` will make any messages start at column 1. Then
the identifier is read and filter excludes it at column 6. Then `amend` fixes the error message.
Simple technique, but what if there are errors inside the combinator we _don't_ want to change?
Let's artificially make identifiers more complex, and say that the second letter must be a digit:

```scala
val identifier =
    token {
        //amend {
            (alphaNum <::> (digit <::> many(alphaNum))).map(_.mkString).filterOut {
                case v if keywords(v) =>
                    s"keyword $v may not be used as an identifier"
            }
        //}
    }.label("identifier")
```

I've removed the `amend`, so we can see what happens:

```
(lexer.identifier <|> "then").parse("abc")
(line 1, column 2):
  unexpected "b"
  expected digit
  >abc
    ^
```

Right, this is good, the second letter isn't a digit, so the parser failed, and that's
the reason. But what about when the `amend` is there:

```
(lexer.identifier <|> "then").parse("abc")
(line 1, column 1):
  unexpected "abc"
  expected "then" or identifier
  >abc
   ^
```

This isn't right at all! The error has backed out with
no indication of what happened. This is where `entrench` comes in. This combinator is found inside
an `amend` and blocks its effect. In our case, we want to protect the errors from within the first
part of the parser, and amend those from the `filterOut` only:

```scala
val identifier =
    token {
        amend {
            entrench(alphaNum <::> (digit <::> many(alphaNum))).map(_.mkString).filterOut {
                case v if keywords(v) =>
                    s"keyword $v may not be used as an identifier"
            }
        }
    }.label("identifier")
```

Now that the first part has been entrenched, the amendment to the message will be prevented:

```
(lexer.identifier <|> "then").parse("abc")
(line 1, column 2):
  unexpected "b"
  expected digit
  >abc
    ^
```

Now, obviously, we don't have any keywords with the second character a digit, so this is a little
esoteric, but there are much larger examples of the `filterOut` (or indeed `guardAgainst`) technique
where this might come in handy. For our case, however, we can make do with just `amend`.

### Using `lookAhead` instead
As promised, here is another way we could fix our problem (and it's less efficient!). Like I said
earlier, the crux of the issue is that we consumed input on the successful parse _before_ we perform
the filter, which means the error message is pointing after that input. The `lookAhead` combinator,
however, doesn't consume input on success (but it might on fail!). So we can split the work into
two parts: first is validation and then the second commital:

```scala
val identifier =
    lexeme {
        lookAhead(some(alphaNum).map(_.mkString)).filterOut {
            case v if keywords(v) =>
                s"keyword $v may not be used as an identifier"
        } <* some(alphaNum)
    }.label("identifier")
```

Now, I've missed out an `atomic` (and so used `lexeme` instead of `token`) here because I know that
if an identifier fails to read anything
(barring keywords), it won't consume input in the process. In this parser, we first look ahead at
the next piece of input and try and read an identifier, if we are successful, we filter it to check
for keywords (but have consumed no input). If that fails then we have already backed out of the
input reading. If the identifier is not a keyword then, unfortunately, we have to read the whole
thing again to actually get the input consumed. The silver lining is that we at least get to make
use of the work we already did to turn it into a string by using `<*`. This will have the same
effect as using `amend` on the errors but it is a bit more expensive and duplicates code. That being
said, in this form we can get a finer control over how the labelling might work, since we don't have
a label outside an attempt, we can label inside the parser as well. In fact, our esoteric example
behaves itself a little better in this form:

```scala
val identifier =
    lexeme {
        lookAhead((alphaNum <::> (digit <::> many(alphaNum))).map(_.mkString)).filterOut {
            case v if keywords(v) =>
                s"keyword $v may not be used as an identifier"
        } <* some(alphaNum)
    }.label("identifier")
```

This time, even with the `.label("identifier")`, we still get information about our digit in errors.
We also don't need to change the commital parser, since it knows there must have been a digit to
get to that point anyway. Again though, this is not the best practice to do: if `amend` and
`entrench` get the job done, I would advise using them!

## Using `unexpected` and `fail` to refine messages
The `unexpected` combinator is a simple combinator, but one that is difficult to apply in practice.
At it's core, it fails, but adjusts the unexpected message within the error. Let's take a look:

```
unexpected("something").label("something else").parse("a")
(line 1, column 1):
  unexpected something
  expected something else
  >a
   ^
```

The `fail` combinator, however, destroys the _entire_ message. Instead you can list out a bunch of
lines that should make up the body of the message (these merge if two paths both result in a "specialised"
error). Error messages from `fail` take precedence over any other error messages. Here it is:

```
fail("something", "something else").label("nothing happens").explain("nothing at all").parse("a")
(line 1, column 1):
  something
  something else
  >a
   ^
```

On their own, it's quite hard to see how they can be useful! The key is to recognise that they can be
used in conjunction with other combinators. Before we do that though, I want to illustrate that
there is a form of `unexpected` that exists as a method:

```
anyChar.unexpected(c => s"character $c").parse("a")
(line 1, column 2):
  unexpected character a
  >a
    ^
```

And `!` exists for `fail`:

```
(anyChar ! (c => s"character $c is illegal for some reason")).parse("a")
(line 1, column 2):
  character a is illegal for some reason
  >a
    ^
```

This variant can use the result of the parser it is attached to in order to generate the unexpected
message.

### Using `amend` or `lookAhead`
The easiest way to start using `unexpected` or `fail` is by using the two tools we've seen so far. The
pattern is to use it to produce predicative errors. This is where you can anticipate common syntax
issues with a language or format and produce a specialised error to help with them. The idea is to use
them when other alternatives have failed: this gives the parser the chance to collect up the expected
items that would help to resolve the error.

For this section I'm going to create a new parser for a small interpreter. It supports conditionals,
assignment, arithmetic, and boolean expressions; variables must be integers. Our parser is actually
going to build an interpreter for the program it parses (just for variety), which is defined as follows:

```scala
object eval {
    import scala.collection.mutable
    import scala.annotation.tailrec
    type Context = mutable.Map[String, Int]
    type Eval[A] = Context => Either[String, A]

    def number(x: Int): Eval[Int] = _ => Right(x)
    def bool(b: Boolean): Eval[Boolean] = _ => Right(b)

    def negate(x: Eval[Int]): Eval[Int] = ctx => x(ctx).map(0 - _)
    def add(x: Eval[Int], y: Eval[Int]): Eval[Int] = ctx =>
        for (n <- x(ctx); m <- y(ctx)) yield n + m
    def sub(x: Eval[Int], y: Eval[Int]): Eval[Int] = ctx =>
        for (n <- x(ctx); m <- y(ctx)) yield n - m
    def mul(x: Eval[Int], y: Eval[Int]): Eval[Int] = ctx =>
        for (n <- x(ctx); m <- y(ctx)) yield n * m

    def less(x: Eval[Int])(y: Eval[Int]): Eval[Boolean] = ctx =>
        for (n <- x(ctx); m <- y(ctx)) yield n < m
    def equal(x: Eval[Int])(y: Eval[Int]): Eval[Boolean] = ctx =>
        for (n <- x(ctx); m <- y(ctx)) yield n == m

    def and(x: Eval[Boolean], y: Eval[Boolean]): Eval[Boolean] =
        cond(x, y, bool(false))
    def or(x: Eval[Boolean], y: Eval[Boolean]): Eval[Boolean] =
        cond(x, bool(true), y)
    def not(x: Eval[Boolean]): Eval[Boolean] = ctx =>
        x(ctx).map(!_)

    def ask(v: String): Eval[Int] = ctx =>
        ctx.get(v).toRight(s"variable $v out of scope")
    def store(v: String, x: Eval[Int]): Eval[Unit] = ctx =>
        for (n <- x(ctx)) yield ctx += (v -> n)

    def cond[A](b: Eval[Boolean], t: Eval[A], e: Eval[A]): Eval[A] = ctx =>
        b(ctx).flatMap(b => if (b) t(ctx) else e(ctx))

    @tailrec def sequence(actions: List[Eval[Unit]], ctx: Context): Either[String, Unit] = actions match {
        case Nil => Right(())
        case action::actions => action(ctx) match {
            case Right(_) => sequence(actions, ctx)
            case msg => msg
        }
    }
}
```

Nothing too fancy here, but we can write an expression like
`store("x", add(number(5), ask(v)))(mutable.Map("v" -> 3))` and our map would contain `"x" -> 8`
afterwards. We'll be using the same `lexer` as we've been accustomed to recently (with some extra
keywords and operators), so let's see what the parser is like:

```scala
object interpreter {
    import lexer.implicits.implicitToken
    import lexer.{number, identifier, fully}
    import eval._
    import parsley.implicits.lift.{Lift2, Lift3}
    import parsley.Result

    def apply(input: String)(ctx: Map[String, Int]): Either[String, Map[String, Int]] = {
      import scala.collection.mutable
      val ctx_ = mutable.Map(ctx.toSeq: _*)
      for {
        prog <- parser.parse(input).toEither
        _ <- prog(ctx_)
      } yield ctx_.toMap
    }

    private lazy val atom: Parsley[Eval[Int]] = "(" *> expr <* ")" <|> number.map(eval.number) <|> identifier.map(ask)
    private lazy val expr = precedence[Eval[Int]](atom)(
        Ops(Prefix)("negate" #> negate),
        Ops(InfixL)("*" #> mul),
        Ops(InfixL)("+" #> add, "-" #> sub))

    private lazy val blit: Parsley[Eval[Boolean]] = "true" #> bool(true) <|> "false" #> bool(false)
    private lazy val comp: Parsley[Eval[Boolean]] = (expr <**> ("<" #> (less _) <|> "==" #> (equal _))) <*> expr
    private lazy val btom: Parsley[Eval[Boolean]] = atomic("(" *> pred) <* ")" <|> blit <|> comp
    private lazy val pred = precedence[Eval[Boolean]](btom)(
        Ops(Prefix)("not" #> not),
        Ops(InfixR)("&&" #> and),
        Ops(InfixR)("||" #> or))

    private def braces[A](p: =>Parsley[A]) = "{" *> p <* "}"

    private lazy val asgnStmt: Parsley[Eval[Unit]] = (store _).lift(identifier, "=" *> expr)
    private lazy val ifStmt: Parsley[Eval[Unit]] = (cond[Unit] _).lift("if" *> pred, braces(stmts), "else" *> braces(stmts))
    private lazy val stmt: Parsley[Eval[Unit]] = asgnStmt <|> ifStmt
    private lazy val stmts: Parsley[Eval[Unit]] = sepEndBy(stmt, ";").map((sequence _).curried)

    val parser = fully(stmts)
}
```

I'm not going to say too much about this, since all of the ideas have been covered in previous pages
(and the Haskell example!). Let's run an example through it and get used to the syntax:

```scala
interpreter(
    """x = 7;
      |if x < v && 5 < v {
      |  y = 1
      |}
      |else {
      |  y = 0
      |};
      |x = 0
    """.stripMargin)(Map("v" -> 8))
Right(Map(v -> 8, x -> 0, y -> 1))
```

The `|`s here are being used by Scala's `stripMargin` method to remove the leading whitespace on our
multi-line strings. We can see that the syntax of this language makes use of semi-colons for
delimiting, and `if` statements require a semi-colon after the `else`. In addition, no parentheses
are required for the `if`, but braces are mandated. This syntax is a little unorthodox, which is
great for us because it'll give us a lot of opportunities to test out our new tools! I've neglected
to add any `.label`s or `.explain`s here, but obviously there are plenty of opportunities.

Let's start by seeing what happens if we accidentally write a `;` before an `else`:
```
interpreter(
    """if true {};
      |else {}
    """.stripMargin)(Map.empty)
(line 1, column 11):
  unexpected ";"
  expected else
  >if true {};
             ^
  >else {}
```

This is what we'd expect, since at this point we'd expect an `else`. We could detail this issue for
the user with an `.explain`, and explain that semi-colons are not something that work in this
position:

```scala
private lazy val ifStmt: Parsley[Eval[Unit]] =
    (cond[Unit] _).lift(
        "if" *> pred, braces(stmts),
        "else".explain("semi-colons cannot be written after ifs") *> braces(stmts))
```

What effect will this have?

```
(line 1, column 11):
  unexpected ";"
  expected else
  semi-colons cannot be written after ifs
  >if true {};
             ^
  >else {}
```

Ok, this is better! But what if I wrote something else there instead?

```
(line 1, column 11):
  unexpected "a"
  expected else
  semi-colons cannot be written after ifs
  >if true {}a
             ^
  >else {}
```

Ah, right, not so good anymore. We could go back and make the `explain` a bit more general, of course,
but that means we've lost out on the helpful prompt to the user about our language's syntax. So, what
can we do here? This is where `unexpected` comes in, along with `amend`:

```scala
private val _semiCheck =
    amend(';'.hide *> unexpected("semi-colon").explain("semi-colons cannot be written between `if` and `else`"))

private lazy val ifStmt: Parsley[Eval[Unit]] =
    (cond[Unit] _).lift(
        "if" *> pred, braces(stmts),
        ("else" <|> _semiCheck)
        *> braces(stmts))
```

Here, if we can't read an `else`, we immediately try to parse `_semiCheck`, which read a semi-colon
(and is careful to hide it from the errors, otherwise we might see `expected else or ";"`). If it succeeds, then we fail with an `unexpected` and
use `explain` to add our reason back in. Without `amend`, the error message would point _after_ the
semi-colon and not _at_ the semi-colon. A parser like `_semiCheck` is known as an "error widget":
prefix them with `_` to make them more immediately identifiable.

With this we have:

```
(line 1, column 11):
  unexpected "a"
  expected else
  >if true {}a
             ^
  >else {}

(line 1, column 11):
  unexpected semi-colon
  expected else
  semi-colons cannot be written between `if` and `else`
  >if true {};
             ^
  >else {}
```

This is exactly what we wanted! Note that, if it were important that we could backtrack out of the
semi-colon, we could use the following instead:

```scala
private val _semiCheck =
    lookAhead(';'.hide) *> unexpected("semi-colon").explain("...")
```

In fact, `lookAhead(p) *> failCombinator` is the same as `atomic(amend(p *> failCombinator)` when
`p` doesn't fail having consumed input (which `';'` on its own can't).

So, where else can we apply this technique? Let's see what happens if we miss out a closing brace

```
interpreter(
    """if true {}
      |else {""".stripMargin)(Map.empty)
(line 2, column 7):
  unexpected end of input
  expected "}", identifier, or if
  >if true {}
  >else {
         ^
```

We could, again, give the user a helping hand here, and point out that they have an unclosed
_something_ that they need to close. Again, we could start by using the `.explain` combinator
directly, on the `}`. Let's see what effect this will have:

```scala
private def braces[A](p: =>Parsley[A]) = "{" *> p <* "}".explain("unclosed `if` or `else`")
```

This will give us the more helpful error:

```
(line 2, column 7):
  unexpected end of input
  expected "}", identifier, or if
  unclosed `if` or `else`
  >if true {}
  >else {
         ^
```

This time, adding extra input won't cause a problem, so is this fine? Well, what about this input:

```
interpreter(
    """if true {}
      |else {
      | x = 7a
      |}""".stripMargin)(Map.empty)

(line 3, column 7):
  unexpected "a"
  expected ";", "}", *, +, -, or digit
  unclosed `if` or `else`
  >else {
  > x = 7a
         ^
  >}
```

Argh! The `else` _is_ closed this time, but since `}` is a valid continuation character we've
triggered our `explain` message. Again, we can fix this by using our new `amend + unexpected` pattern:

```scala
private val _eofCheck = amend(eof.hide *> unexpected("end of input").explain("unclosed `if` or `else`"))
private def braces[A](p: =>Parsley[A]) =
    "{" *> p <* ("}" <|> _eofCheck)
```

This time we've latched onto whether or not there is any input left at all. This will work fine!

```
(line 3, column 7):
  unexpected "a"
  expected ";", "}", *, +, -, or digit
  >else {
  > x = 7a
         ^
  >}

(line 2, column 7):
  unexpected end of input
  expected "}", identifier, or if
  unclosed `if` or `else`
  >if true {}
  >else {
         ^
```

Perfect 🙂. What now? Well, another area where the user might trip up is thinking that you can
assign booleans to variable! Let's see what the errors are:

```
(line 1, column 5):
  unexpected keyword true
  expected "(", digit, identifier, or negate
  >x = true
       ^

(line 1, column 8):
  unexpected "<"
  expected ";", *, +, -, or end of input
  >x = 10 < 9
          ^

(line 1, column 5):
  unexpected keyword not
  expected "(", digit, identifier, or negate
  >x = not true
       ^
```

Now, there is a cheap way of dealing with this and an expensive one. Let's start cheap and see what
needs to be done and how effective it is. The first thing we can recognise is that we can special
case `not`, `true`, and `false` using the same strategy as before. We can either choose to attach
this to `expr` or to the `assign` itself (or indeed both!), the choice of which will change how
we will structure the message. Let's try both and abstract it into a new combinator:

```scala
private def _noBoolCheck(reason: String): Parsley[Nothing] = {
    ("true" <|> "false" <|> "not").hide *>
        unexpected("boolean expression")
        .label("arithmetic expression")
        .explain(reason)
}
```

Now, we can add this to both places (I've omited an `amend` from the combinator itself for reasons we'll see) as follows:

```scala
private lazy val expr = precedence[Eval[Int]](atom)(
    Ops(Prefix)("negate" #> negate),
    Ops(InfixL)("*" #> mul),
    Ops(InfixL)("+" #> add, "-" #> sub)) <|> amend(_noBoolCheck("booleans cannot be integers"))
```

Let's try this as a first attempt, this doesn't perfectly address our assignment problem yet, but
it will be helpful to see what the effects of it are:

```
(line 1, column 5):
  unexpected keyword true
  expected "(", arithmetic expression, digit, identifier, or negate
  booleans cannot be integers
  >x = true
       ^
```

Hmm, it looks like the error from the check and the `expr` itself were merged: of course they would
be, since they happened at the same offset. This means the labels are merged, and the message appears
but the unexpected message isn't what we were after. Well, the problem is that when two unexpected
messages are merged, it happens with the following scheme:

* "raw" messages take the longest
* "custom" messages take priority over "raw", and the first is taken
* "end of input" messages take priority over "custom" and "raw"

So, since both errors produce a "custom" unexpected error, the `expr` proper took the priority: not
quite what we wanted. Perhaps a natural reaction to learning this is to reverse their ordering...

```scala
private lazy val expr = atomic(amend(_noBoolCheck("booleans cannot be integers"))) <|> precedence[Eval[Int]](atom)(
    Ops(Prefix)("negate" #> negate),
    Ops(InfixL)("*" #> mul),
    Ops(InfixL)("+" #> add, "-" #> sub))
```

This would yield:

```
(line 1, column 5):
  unexpected boolean expression
  expected "(", arithmetic expression, digit, identifier, or negate
  booleans cannot be integers
  >x = true
       ^
```

But, annoyingly, this creates backtracking! Also, the labels are still merged: this isn't so much
a problem, since we could add the label to the expr too and eliminate it. But really, the backtracking
is the annoying bit; the idea behind this pattern is to identify user mistake only when we've
exhausted the other options, keeping performance good. Now, it's a good time to apply our previous
trick:

```scala
private lazy val expr = amend {
    entrench(precedence[Eval[Int]](atom)(
        Ops(Prefix)("negate" #> negate),
        Ops(InfixL)("*" #> mul),
        Ops(InfixL)("+" #> add, "-" #> sub))) <|> _noBoolCheck("booleans cannot be integers")
}
```

And this gives:

```
(line 1, column 5):
  unexpected boolean expression
  expected arithmetic expression
  booleans cannot be integers
  >x = true
       ^
```

Perfect! Let's reinforce our understanding of why this works like it does:

1. First we issue an instruction to amend any final error message that arises from `expr`
2. We try reading an expression, being ensure to protect it against the amendment
3. Suppose we fail to read an expression: if no input was consumed, it's likely that we've hit one
   of the boolean keywords. The error occured at column 5.
4. With no input consumed we can try our bool check, and we _will_ consume input doing this: up to
   column 9
5. The two errors are merged, with the second error at column 9 taking precedence.
6. The `amend` sets the column back to 5.

This is a really nice trick to give you control over which errors should make the final cut. It's
well worth really understanding why this worked out as it did. In the future, we'll have a new
debug combinator to help illustrate this (and I'll change this page to suit).

Right, so now to tackle the other place where the boolean check can occur. This will require us to
mark our first `_noBoolCheck` with an `atomic` to allow us to perform a second (again, we want to
delay it as long as possible). We could, of course, move the `_noBoolCheck` somewhere else so that
exactly one dominates each use site, but when this is a "last resort" mechanism, it doesn't make
much of a difference.

```scala
private lazy val asgnStmt: Parsley[Eval[Unit]] =
    (store _).lift(identifier,
                   "=" *> (expr <|> amend(_noBoolCheck("booleans cannot be assigned to variables"))))
```

With this in place (and the aforementioned `atomic`) we get an error like:

```
(line 1, column 5):
  unexpected boolean expression
  expected arithmetic expression
  booleans cannot be assigned to variables
  booleans cannot be integers
  >x = true
       ^
```

This looks fine as it is, but if we wanted to prevent the noise of the second reason, we could use
the `amend` trick once again (remember that `entrench` will hold fast against any number of `amend`s
so don't worry about the `expr` error itself):

```scala
private lazy val asgnStmt: Parsley[Eval[Unit]] =
    (store _).lift(identifier,
                   "=" *> amend(expr <|> _noBoolCheck("booleans cannot be assigned to variables")))
```

Giving us the alternative:

```
(line 1, column 5):
  unexpected boolean expression
  expected arithmetic expression
  booleans cannot be assigned to variables
  >x = true
       ^
```

Again, we got this following the steps I outlined previously. So, we've cracked the _leading_ edge
of the booleans, but we are still no closer to managing to deal with `<`, `==`. These
are significantly trickier to handle with our current approach, because they occur _after_ some input
has already been read. We would need to insert them as alternatives at every point where they could be
"valid" predictions. This is far from ideal. Note that we don't really need to worry about `&&` and
`||`, since we are going to have to have already found one of `<`, `==`, `not`, `true`, or `false`
before we reach it _anyway_.

#### Using `notFollowedBy`
Fortunately for us, our `lookAhead/amend` with `<|>` has a cousin: `notFollowedBy` with `<*`. We've
even seen a variant of this when we handled `keyword` not ending in a letter. Using this, we can
capture the problem much closer to the expression (or indeed the assignment). This comes at a cost,
however: `notFollowedBy` has no custom `unexpected` message, and it will not progress further in the
parser to collect labels so, if we want them, we have to figure them out ourselves! Let's abstract this
into another handy combinator:

```scala
private def labels(ls: String*) = choice(ls.map(empty.label(_)): _*)

private def _noCompCheck(reason: String): Parsley[Unit] = {
    notFollowedBy(oneOf('<', '=')).explain(reason) <|> unexpected("boolean expression") <|> labels("+", "*", "-", "end of input", "\";\"")
}
```

This is fairly interesting in and of itself: by combining the `notFollowedBy`, which does the payload
of the work with `unexpected` and a bunch of `empty.label`, we can recover the same shape of error
message that we would have had before. Of course, we had to go and find these alternatives ourselves
from the version of the parser without the check. We could side step this by using the `arithmetic
expression` label if we wished, but we only want to apply that if we don't consume any input within
the error message! So, let's see what the effect of the error will be:

```scala
private lazy val asgnStmt: Parsley[Eval[Unit]] =
        (store _).lift(identifier,
                       "=" *> amend((expr <* _noCompCheck("booleans cannot be assigned to variables")) <|>
                                     _noBoolCheck("booleans cannot be assigned to variables")))
```
```
(line 1, column 5):
  unexpected boolean expression
  expected ";", *, +, -, or end of input
  booleans cannot be assigned to variables
  >x = 10 < 9
       ^
```

Well, it's worked properly, however notice that the labels are all incorrect as we have indeed been
reset by the amend. Let's see what happens, however, if we add an arithmetic label to the comparison
check:

```
(line 1, column 5):
  unexpected boolean expression
  expected *, +, -, or arithmetic expression
  booleans cannot be assigned to variables
  >x = 10 < 9
       ^
```

Oh no! Unfortunately here, the error failed having consumed no input, so the "hints" from expression
are still valid. This is annoying for sure. There are two ways we can handle this, the first would
be to use `notFollowedBy(...) <|> fail(...)` to create a custom message that would certainly override
the hints from the expression, or we can add an entrench and see what effect it has:

```scala
private lazy val asgnStmt: Parsley[Eval[Unit]] =
        (store _).lift(identifier,
                       "=" *> amend((expr <* entrench(_noCompCheck("booleans cannot be assigned to variables"))) <|>
                                     _noBoolCheck("booleans cannot be assigned to variables")))
```
```
(line 1, column 8):
  unexpected boolean expression
  expected ";", *, +, -, or end of input
  booleans cannot be assigned to variables
  >x = 10 < 9
          ^
```

The other place we can add it would be ideally inside the brackets of `atom`, since if we put them
at the end of `expr` itself, it would likely supercede the message we put in `asgnStmt` because it
has been entrenched:

```scala
private lazy val atom: Parsley[Eval[Int]] =
    "(" *> expr <* entrench(_noCompCheck("booleans cannot be integers")) <* ")" <|>
    number.map(eval.number) <|>
    identifier.map(ask)
```

In all, this gives us the errors:

```
(line 1, column 9):
  unexpected boolean expression
  expected ";", *, +, -, or end of input
  booleans cannot be integers
  >x = (10 < 9) + 7
           ^
(line 1, column 8):
  unexpected boolean expression
  expected ";", *, +, -, or end of input
  booleans cannot be assigned to variables
  >x = 10 < 9
          ^

(line 1, column 6):
  unexpected boolean expression
  expected arithmetic expression
  booleans cannot be integers
  >x = (true) && false
        ^

(line 1, column 5):
  unexpected boolean expression
  expected arithmetic expression
  booleans cannot be assigned to variables
  >x = true
       ^
```

These all look pretty good! The only little niggle here is that the cursor is at column 6 in the
bracket example. I mentioned earlier there is a more expensive way of addressing this: essentially
we would try and parse a complete `pred` _first_ and then use `fail` to create the message. This
would override any errors that happened at the same offset from `expr` itself, removing any noise.
This is more expensive as it must parse an entire predicate potentially backtracking if it goes
wrong. It doesn't remove the need for `notFollowedBy` to stop arithmetic expressions from terminating
the parse "eagerly", but it is slightly more robust with handling edge cases like the brackets. But
this is so minor that I'm not going to go into it, you can explore it for yourself if you want.

## Using `fail` and `guardAgainst`
So far, we've seen the use of `filterOut`, `unexpected`, `empty`, `explain`, and `fail`. As
`filterOut` is to `empty + explain`, `guardAgainst` is to `fail`. The distinction between the two
is really just the _sort_ of the message or failure we are trying to produce. Something that is
syntactic and recoverable really should be using `filterOut` and `unexpected`, but something that
is more a property of the language, or extraneous verification should be using `fail` or
`guardAgainst`. We might argue that the boolean/int distinction we made in the previous sections
could have very easily just been modelled using "specialised" errors, since it's not so much a syntactic
problem as a semantic one. Really, play around with both sorts and see which one you feel fits the
tone of the error more.

### Context-Awareness
It's interesting to recognise that `filterOut` is a more context-aware version of `filter`. That is
to say that `filterOut` gets access to the parsed result in order to produce the message, whereas
`filter` does not. `guardAgainst` is also context-aware. In this last part of the page, I want to
explore how these combinators are defined, and show something we haven't yet touched upon:
context-aware combinators.

Generally, there are two sorts of context-aware combinators: the selective, and the monadic
combinators. Roughly, a selective provides branching behaviour based on the result of a parser:
this allows it to make combinators like `filter`, conditionals, `when`, `whileP` etc. Selectives are,
at their core, given by the operation `branch`:

```scala
def branch[A, B, C](b: =>Parsley[Either[A, B]], l: =>Parsley[A => C], r: =>Parsley[B => C]): Parsley[C]
def select[A, B](p: =>Parsley[Either[A, B]], q: =>Parsley[A => B]): Parsley[B] = {
    branch(p, q, pure(identity[B]))
}
```

What `branch` does is will execute _one_ of `l` and `r`, but not both, depending on what result `b`
produces. More traditionally, `select` is the core operation, it only executes `q` if `p` returned
a `Left`. Using `branch` we can start to implement _some_ context-aware operations: here's a version
of filter

```scala
def filter[A](f: A => Boolean, p: =>Parsley[A]) = {
    select(p.map(x => if (f(x)) Left(()) else Right(x)), empty)
}
```

This works by returning a `Left(())` when the predicate fails and therefore executing `empty`.
The reason I've mentioned the selectives first is that they are _significantly_ cheaper in Parsley
than the monadic operation `flatMap`. That being said, `flatMap` is strictly more powerful than
`branch` could ever hope to be and can implement it:

```scala
def (p: =>Parsley[A]) flatMap[B](f: A => Parsley[B]): Parsley[B]
def branch[A, B, C](b: =>Parsley[Either[A, B]], l: =>Parsley[A => C], r: =>Parsley[B => C]): Parsley[C] = {
    b.flatMap {
        case Left(x) => l <*> pure(x)
        case Right(y) => r <*> pure(y)
    }
}
```

But easier still it can implement `filter`:

```scala
def filter[A](f: A => Boolean, p: =>Parsley[A]) = p.flatMap(x => {
    if (f(x)) pure(x)
    else empty
})
```

So, why am I mentioning it? Well, `guardAgainst` and `filterOut` are both _implementable_ with
`flatMap` but not `branch`. I say implementable in italics because, in reality, they are primitives
to avoid the cost of a `flatMap`. But here they _would_ be:

```scala
def filterOut[A](f: PartialFunction[A, String], p: =>Parsley[A]) = p.flatMap {
    case x if f.isDefinedAt(x) => empty.explain(f(x))
    case x => pure(x)
}

def guardAgainst[A](f: PartialFunction[A, String], p: =>Parsley[A]) = p.flatMap {
    case x if f.isDefinedAt(x) => fail(f(x))
    case x => pure(x)
}
```

The reason why this is of interest to us is that it gives you the tools you need to implement a
variant of a filter which uses `unexpected` instead of `empty`, since this isn't currently exposed
by Parsley. Another good reason to know about `flatMap` is that it gives us a taste of how to
introduce much more powerful error messages based on contextual information. When we come to do
context-sensitive parsers in the final part of this series, however, we will be avoiding `flatMap` in
favour of using _registers_ and selectives.
