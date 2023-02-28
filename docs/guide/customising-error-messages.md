# Customising Error Messages

Previously, in
[Effective Whitespace Parsing](https://github.com/j-mie6/Parsley/wiki/Effective-Whitespace-Parsing)
we saw how we could extend our parsers to handle whitespace. In this wiki post we'll finally address
error messages. Thoughout all the other entries in this series I have neglected to talk about error
messages at all, but they are a very important part of a parser. Recently, Parsley 2.6.1 overhauled
the entire way error messages work and what they look like, and Parsley 3.0.0 added the `ErrorBuilder`
mechanism, so I will be targeting 3.0.0 and above for this post.

# Adjusting Error Content
I'm going to start with the parser from last time, but before we introduced the `Lexer` class. The
reason for this is that the `Lexer` functionality has error messages baked into it, which means this
post would be even shorter! It's not perfect, however, but it does make some good error messages for
your basic lexemes. There is nothing stopping you from using the techniques here to change those
messages if you wish though. Simply put: the original grammar has more room for exploration for us.

```scala
import parsley.Parsley, Parsley.attempt

object lexer {
    import parsley.character.{digit, whitespace, string, item, endOfLine}
    import parsley.combinator.{manyUntil, skipMany, eof}

    private def symbol(str: String): Parsley[String] = attempt(string(str))
    private implicit def implicitSymbol(tok: String): Parsley[String] = symbol(tok)

    private val lineComment = "//" *> manyUntil(item, endOfLine)
    private val multiComment = "/*" *> manyUntil(item, "*/")
    private val comment = lineComment <|> multiComment
    private val skipWhitespace = skipMany(whitespace <|> comment)

    private def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* skipWhitespace
    private def token[A](p: =>Parsley[A]): Parsley[A] = lexeme(attempt(p))
    def fully[A](p: =>Parsley[A]): Parsley[A] = skipWhitespace *> p <* eof

    val number = token(digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit))

    object implicits {
        implicit def implicitToken(s: String): Parsley[String] = lexeme(symbol(s))
    }
}

object expressions {
    import parsley.expr.{precedence, Ops, InfixL}

    import lexer.implicits.implicitToken
    import lexer.{number, fully}

    private lazy val atom: Parsley[Int] = "(" *> expr <* ")" <|> number
    private lazy val expr = precedence[Int](atom)(
        Ops(InfixL)("*" #> (_ * _)),
        Ops(InfixL)("+" #> (_ + _), "-" #> (_ - _)))

    val parser = fully(expr)
}
```

So, before, we saw how this ran on succesful cases. Let's now start to see how it works on _bad_
input.

```
expressions.parser.parse("5d")
(line 1, column 2):
  unexpected "d"
  expected "*", "+", "-", "/*", "//", digit, end of input, or whitespace
  >5d
    ^
```

Let's start by breaking this error down first and understanding what the components of it are and
why this information has appeared. The first line of the error reports the line and column number
of the message (in Parsley hard tabs are treated as aligning to the nearest 4th column). If you are
using `parseFromFile` then this will also display the filename. The last two lines always show the
location at which the error occured. This is going to be _the point_ at which the error that
eventually ended up being reported occured, _not_ necessarily where the parser ended up. This can
be improved in the future. Next you can see the _unexpected_ and _expected_ clauses. The unexpected
"d" here is telling us roughly what we already knew. The expected clause on the other hand tells us
all the things we could have used to fix our problem. There is definitely a lot of noise here though.

First let's just make sure we understand where each of these alternatives came from. Firstly, it's
clear that since the last thing we read was a 5, a good way of carrying on would be reading another
digit to make the number bigger. We could also read a space or start a comment as a way of making
more progress too. Of course, another way we could make progress would have been using one of the
operators and in the process continued our expression. Finally we could simply remove the d and it
would run perfectly fine. Notice how `(` and `)` are not suggested as alternatives despite appearing
in the parser: `5(` or `5)` makes no sense either. As another small example, let's see what happens
to the error if we add a space between the 5 and the d.

```
expressions.parser.parse("5 d")
(line 1, column 3):
  unexpected "d"
  expected "*", "+", "-", "/*", "//", end of input, or whitespace
  >5 d
     ^
```

Neat, so this time round `digit` is no longer a valid alternative: clearly the number has come to
an end because we wrote a space. But the other possibilities from before are still valid. So, how
can we start making improvements? There are seven combinators available to us for this purpose:

* `.label` or `?` is the most common combinator you'll be using. It influences the way an _expected_
  behaves for the parser it annotates. Importantly, if the parser it is annotating failed _and_
  consumed input in the process, then the label will not be applied. We'll see an example of why
  this is useful later.
* `fail` is useful, but a bit of a sledgehammer. When `fail` (or any of its derivative combinators
  like `guardAgainst`) is used, it removes the unexpected and expected information and just replaces it
  with a given message. If there are multiple `fail`s that appear in the same message, they are
  each written on a newline.
* `unexpected` is the least commonly used combinator in practice. When it is used, it will,
  like `fail`, immediately fails except it reports a custom unexpected message. Currently, only
  one unexpected message can be present in the error at once, so this is not very useful unless you
  really know what you are doing.
* `.hide` is a method that removes the output of a parser from the error, it is essentially a nice
  name for `.label("")`
* `.explain` is a method that can provide _reasons_ for a parser's failure. If the parser can recover
  and move onto other alternatives, the reasons may be lost. But they can still be quite nice when
  used in the correct place!
* `amend` and `entrench` are a pair of combinators that work together to correct the position of
  some error messages. These are quite rare in practice.

All of these can be found in the `parsley.errors.combinator` module.

## Using `label`
From this section, we are only going to be using `label` and `hide`, as they are by far the most useful
and effective of the five methods. That being said, `explain` can be _very_ useful, but we'll find
there are no compelling use-cases for it in this example. Let's start off by giving a label to
`comment` and see what happens:

```scala
import parsley.errors.combinator._
...
private val comment = (lineComment <|> multiComment).label("comment")
...
```

Now let's run our parser from before:

```
expressions.parser.parse("5d")
(line 1, column 2):
  unexpected "d"
  expected "*", "+", "-", comment, digit, end of input, or whitespace
  >5d
    ^
```

Nice! So, if you compare the two, you'll notice that `"/*"` and `"//"` both disappeared from the
message, but `comment` was added. You can tell when `label` is being used because there are not
quotes surrounding the items. Knowing this, you can probably guess that `digit`, `eof`, and
`whitespace` all have error labels of their own.

### Using `hide` to trim away junk
This is a good start, but normally we might say
that whitespace suggestions in an error message are normally just noise: of course we expect to be
able to write whitespace in places, it's not _usually_ the solution to someone's problem. This makes
it a good candidate for the `hide` combinator:

```scala
import parsley.errors.combinator._
...
private val skipWhitespace = skipMany(whitespace <|> comment).hide
...
```

Now let's check again:

```
expressions.parser.parse("5d")
(line 1, column 2):
  unexpected "d"
  expected "*", "+", "-", digit, or end of input
  >5d
    ^
```

Great! The `hide` combinator has removed the information from the error message, and now it's
looking a lot cleaner. But what if we started writing a comment, what would happen then?

```
expressions.parser.parse("5/*")
(line 1, column 4):
  unexpected end of input
  expected "*/" or any character
  >5/*
      ^
```

So, as I mentioned earlier, `hide` is just a `label`, and `label` will not relabel something if it
fails and consumes input. That means, by opening our comment but not _finishing_ it, we can see
some different suggestions. In this case, end of input is not allowed, and any character will work
to extend the comment, but clearly `*/` is a way to properly end it. Let's add a label to that,
however, to make it a bit friendlier:

```scala
...
private val lineComment = "//" *> manyUntil(item, endOfLine.label("end of comment"))
private val multiComment = "/*" *> manyUntil(item, "*/".label("end of comment"))
...
```

Now we get a more informative error message of:

```
expressions.parser.parse("5/*")
(line 1, column 4):
  unexpected end of input
  expected any character or end of comment
  >5/*
      ^
```

Great! Now let's turn our attention back to expressions and not whitespace.

### Labelling our numbers
Let's take a look at a very simple bad input and see how we can improve on it:

```
expressions.parser.parse("d")
(line 1, column 1):
  unexpected "d"
  expected "(" or digit
  >d
   ^
```

So this time, we can see two possible ways of resolving this error are opening brackets, or a
`digit`. Now `digit` is really a poor name here, what we really mean is `integer` or `number`:

```scala
...
val number = token(digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)).label("number")
...
```

Now we get, the following, nicer error message:

```
expressions.parser.parse("d")
(line 1, column 1):
  unexpected "d"
  expected "(" or number
  >d
   ^

expressions.parser.parse("5x")
(line 1, column 2):
  unexpected "x"
  expected "*", "+", "-", digit, or end of input
  >5x
    ^
```

But notice in the second error message, again we have been given `digit` and not `number` as our
alternative. This is good, once we've started reading a number by reading `5` it would be
inappropriate to suggest a number as a good next step. But `digit` here is not particularly descriptive
and we can do better still:

```scala
...
val number =
    token(
        digit.label("end of number").foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
    ).label("number")
...
```

This gives us, again, a much nicer message:

```
expressions.parser.parse("5x")
(line 1, column 2):
  unexpected "x"
  expected "*", "+", "-", end of input, or end of number
  >5x
    ^
```

### Merging multiple labels
With an example grammar as small as this, I think we are almost done here! The last thing we could
improve is the repetition of `"*"`, `"+"`, and `"-"`. Really, we know that there is nothing special
about any of them individually, so we could more concisely replace this them with `arithmetic operator`,
or since we only have arithmetic operators here `operator` will do. we don't need to do anything
special here, when multiple labels are encountered with the same name, they will only appear once!

```scala
...
private lazy val expr = precedence[Int](atom)(
  Ops(InfixL)("*".label("operator") #> (_ * _)),
  Ops(InfixL)("+".label("operator") #> (_ + _), "-".label("operator") #> (_ - _)))
...
```

Now we arrive at our final form:

```
expressions.parser.parse("5x")
(line 1, column 2):
  unexpected "x"
  expected end of input, end of number, or operator
  >5x
    ^

expressions.parser.parse( 67 + )
(line 1, column 7):
  unexpected end of input
  expected "(" or number
  > 67 +
         ^
```

Great! Now obviously you could take this even further and make `"("` become `opening parenthesis`
or something, but I don't really feel that adds much.

## Wrapping up the Expression Example
Hopefully, you get a sense of how much of an
art form and subjective writing good error messages is, but Parsley provides decent error messages
out of the box (now based on `megaparsec`'s error messages from Haskell). It doesn't have to be hard
though, so just play around and see what feels right. I would say, however, there is an interesting
phenomenon in the programming languages and compilers community:
**compiler writers write error messages that are tailored for compiler writers**. It's an
interesting problem when you think about it: the person who writes error messages is a compiler
expert, and so they often rely on the concepts they understand. That means they are more prone to
including the names of stuff in the grammar to describe syntax problems, and so on. While this is
great for experts and compiler writers, it seemingly forgets people who are new to programming or
this "grammar" in particular. That can make error messages needlessly intimidating for the average
Joe. The take home from this is to try and avoid labelling `expr` with `.label("expression")`,
because that just ends up making something that is no longer useful or informative:

```
expressions.parser.parse("")
(line 1, column 1):
  unexpected end of input
  expected expression
  >
   ^
```

What use is that to anybody? The same idea applies to statements, and various other abstract
grammatical notions. Something like
`"expected if statement, while loop, for loop, variable declaration, or assignment"` is so much more
meaningful than `"expected statement"`. I would ask that you keep that in mind 🙂. To conclude our
work with this parser, here is the full code of the finished product. Obviously, with the
`Lexer`, some of this work is already done, but you can still apply the lessons learnt here to the
wider parser!

```scala
import parsley.Parsley, Parsley.attempt
import parsley.errors.combinator._

object lexer {
    import parsley.character.{digit, whitespace, string, item, endOfLine}
    import parsley.combinator.{manyUntil, skipMany, eof}

    private def symbol(str: String): Parsley[String] = attempt(string(str))
    private implicit def implicitSymbol(s: String): Parsley[String] = symbol(s)

    private val lineComment = "//" *> manyUntil(item, endOfLine.label("end of comment"))
    private val multiComment = "/*" *> manyUntil(item, "*/".label("end of comment"))
    private val comment = (lineComment <|> multiComment).label("comment")
    private val skipWhitespace = skipMany(whitespace <|> comment).hide

    private def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* skipWhitespace
    private def token[A](p: =>Parsley[A]): Parsley[A] = lexeme(attempt(p))
    def fully[A](p: =>Parsley[A]): Parsley[A] = skipWhitespace *> p <* eof

    val number =
        token(
            digit.label("end of number").foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
        ).label("number")

    object implicits {
        implicit def implicitToken(s: String): Parsley[String] = lexeme(symbol(s))
    }
}

object expressions {
    import parsley.expr.{precedence, Ops, InfixL}

    import lexer.implicits.implicitToken
    import lexer.{number, fully}

    lazy val atom: Parsley[Int] = "(" *> expr <* ")" <|> number
    lazy val expr = precedence[Int](atom)(
        Ops(InfixL)("*".label("operator") #> (_ * _)),
        Ops(InfixL)("+".label("operator") #> (_ + _), "-".label("operator") #> (_ - _)))

    val p = fully(expr)
}
```

## Using `explain`
So far, we've seen how `label` can be used to clean up error messages and make them much more
presentable and informative. Another way of achieving this is by using the `explain` combinator.
Unlike `label` this is much more freeform and when used properly can be _incredibly_ effective.
Essentially, with `explain` you are leveraging your own knowledge about the context you are in
to provide a much more tailored and hand-crafted message to the user. It can be used to both provide
an additional hint in an otherwise poor message or to enrich the error with suggestions for how the
error might be fixed.

Using it is just as easy as using `label` and you can't really go wrong with it: other than being a
bit... too descriptive. Again, the `Lexer` class already makes use of this technique to improve its
own error messages, but let's suppose we wanted to write some of its functionality ourselves. Let's
cook up a string literal parser, supporting some (limited) escape sequences.

```scala
import parsley.Parsley
import parsley.implicits.character.charLift
import parsley.combinator.{many, between, choice}
import parsley.character._
import parsley.errors.combinator._

val escapeChar =
    choice('n' #> '\n', 't' #> '\t', '\"' #> '\"', '\\' #> '\\')
val stringLetter = noneOf('\"', '\\').label("string character") <|> ('\\' ~> escapeChar).label("escape character")

val stringLiteral =
    between('\"',
            '\"'.label("end of string"),
            many(stringLetter))
        .map(_.mkString)
        .label("string")
```

Let's start with something like this. If we run a couple of examples, we can see where it performs
well and where it performs less well:

```
stringLiteral.parse("")
(line 1, column 1):
  unexpected end of input
  expected string
  >
   ^

stringLiteral.parse("\"")
(line 1, column 2):
  unexpected end of input
  expected end of string, escape character, or string character
  >"
    ^

stringLiteral.parse("\"\\a")
(line 1, column 3):
  unexpected "a"
  expected """, "\", "n", or "t"
  >"\a
     ^
```

So, for the first two cases, the error message performs quite well. But the last message is a bit
noisy. One possible approach to improve this could be to label each alternative to give them a
slightly clearer name, which would result in something like:

```
(line 1, column 3):
  unexpected "a"
  expected \", \\, \n, or \t
  >"\a
     ^
```

This is _better_, but a bit misleading, we don't expect a `\`! Now, you could instead opt to remove
the backslashes, but then that doesn't give much information about why these things are expected.
Another option would be to label all alternatives with some common name:

```scala
val escapeChar =
    choice('n' #> '\n', 't' #> '\t', '\"' #> '\"', '\\' #> '\\')
        .label("end of escape sequence")
```

Which would yield

```
(line 1, column 3):
  unexpected "a"
  expected end of escape sequence
  >"\a
     ^
```

This is a bit more helpful, in that it does provide a good name to what we expected. But at the same
time it doesn't help the user to understand how to fix their problem: "what is an escape sequence".
This is similar to the "statement" problem I described above. In this case, (and indeed in the
"statement" case), we can add an `explain` to help the user understand what we mean:

```scala
val escapeChar =
    choice('n' #> '\n', 't' #> '\t', '\"' #> '\"', '\\' #> '\\')
        .label("end of escape sequence")
        .explain("valid escape sequences include \\n, \\t, \\\", or \\\\")
```

The `explain` combinator annotates failure messages with an additional reason. These can stack, and
are displayed each on their own line in the error message. With this in place, let's see what the
new error message is:

```
(line 1, column 3):
  unexpected "a"
  expected end of escape sequence
  valid escape sequences include \n, \t, \", or \\
  >"\a
     ^
```

This time, we keep the name of the expected token clear and concise, but we _also_ help the user to
understand what this actually means. The error isn't misleading in the sense that we aren't
suggesting that a `\n` would fix the parse error _after_ the `\` we already wrote, but have have
said that we expect the end of the escape as well as demonstrated what that would look like. This
is great!

There isn't much more to say about the `explain` combinator than that really. Hopefully this already
gives you a sense of how useful it can be. Like I mentioned before, the poor error problem that
compiler writers often suffer from can be nicely solved using `explain`. For instance, a message like
"... expected statement ... valid statements include 'if statements', 'loops', or 'assignments'" is
subjectively better than both of the alternatives (namely "expected statement" or the one that lists
out every single alternative). This has the benefits of both worlds: for an experienced user, the error
message gets straight to the point, and for the newcomer, the error message provides a bit more
information that can help them learn the terminology.

# Adjusting Error Formatting

As we've seen in this post, the error messages produced by parsley are fairly readable. They are
broken into two kinds: "vanilla" errors built up of "expected", "unexpected", and "reason" clauses;
and "specialised" errors built up solely of "message" clauses. So far, we have only seen examples of
the "vanilla" errors, and we will see the "specialised" errors in the next post. These have been
so far formatted using Parsley's default mechanism, which creates an error as a string. This is ok
for basic use, but in projects where there is some pre-existing error format, then maintaining consistency
across error messages is much harder without _parsing_ the resulting `String` errors to extract
their content: this is, frankly, ridiculous to expect! Moreover, suppose you wanted to unit test
your parser in both successful and failing cases, then performing raw string comparision is really
brittle, especially if Parsley adjusts the format slightly!

Luckily, Parsley 3.0.0 introduced an abstraction layer between the error messages that the parsers
work with and the final resulting error message. This means that actually, the error message format
is not only configurable, but doesn't _have_ to be a `String`! The final part of this post is dedicated
to understanding how to work with this mechanism, using Parsley's own unit test formatter as an example.

Firstly, I want to give examples of both types of format, and annotate the names given to each part
of them:

```
┌───────────────────────────────────────────────────────────────────────┐
│   Vanilla Error                                                       │
│                          ┌────────────────┐◄──────── position         │
│                  source  │                │                           │
│                     │    │   line      col│                           │
│                     ▼    │     │         ││                           │
│                  ┌─────┐ │     ▼         ▼│   end of input            │
│               In foo.txt (line 1, column 5):       │                  │
│                 ┌─────────────────────┐            │                  │
│unexpected ─────►│                     │            │  ┌───── expected │
│                 │          ┌──────────┐ ◄──────────┘  │               │
│                 unexpected end of input               ▼               │
│                 ┌──────────────────────────────────────┐              │
│                 expected "(", "negate", digit, or letter              │
│                          │    └──────┘  └───┘     └────┘ ◄────── named│
│                          │       ▲        └──────────┘ │              │
│                          │       │                     │              │
│                          │      raw                    │              │
│                          └─────────────────┬───────────┘              │
│                 '-' is a binary operator   │                          │
│                 └──────────────────────┘   │                          │
│                ┌──────┐        ▲           │                          │
│                │>3+4- │        │           expected items             │
│                │     ^│        │                                      │
│                └──────┘        └───────────────── reason              │
│                   ▲                                                   │
│                   │                                                   │
│                   line info                                           │
└───────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────┐
│   Specialised Error                                                   │
│                          ┌────────────────┐◄──────── position         │
│                  source  │                │                           │
│                     │    │   line       col                           │
│                     ▼    │     │         │                            │
│                  ┌─────┐ │     ▼         ▼                            │
│               In foo.txt (line 1, column 5):                          │
│                                                                       │
│           ┌───► something went wrong                                  │
│           │                                                           │
│ message ──┼───► it looks like a binary operator has no argument       │
│           │                                                           │
│           └───► '-' is a binary operator                              │
│                ┌──────┐                                               │
│                │>3+4- │                                               │
│                │     ^│                                               │
│                └──────┘                                               │
│                   ▲                                                   │
│                   │                                                   │
│                   line info                                           │
└───────────────────────────────────────────────────────────────────────┘
```

As you can see, the content for a specialised error is (ironically) plainer than a vanilla message.
This means that the errors are much more customisable from the parser side, but it is less rich
in parser generated information than the vanilla is. Hopefully you can see that both error messages
still have a _very_ similar shape other than the error info lines themselves. In both cases, and not
shown by the diagrams, the main contents of the error -- either unexpected, expected, reasons, and
line info; or messages and line info -- are called "error info lines".

For vanilla errors, notice that the unexpected and expected lines make references to raw, named, and
end of input: these are collectively known as _items_. The `.label` combinator produces named items,
the `eof` combinator produces the end of input item, and unlabelled combinators produce raw items.

Together, all these components are referenced (by these names!) by the `ErrorBuilder` trait. The way
it works is that a concrete `ErrorBuilder` has to be provided to the `.parse` method of a parser,
and when the parser has finished failing, the builder is used to construct the final error message,
converting the internal representation that Parsley uses into the desired output specified by the
builder: you can think of it like a conversation. The internals of Parsley take a portion of the
information it has, and talks to the builder how to format it into another intermediate form; it then
will feed this new information into another method of the builder after possibly more collection.
To allow all of this plumbing to be fed together and maintain maximum flexiblity to the user, the
builder makes use of "associated types". Let's take a look at the definition of `ErrorBuilder` without
all the sub-formatters to understand what I mean:

```scala
trait ErrorBuilder[Err] {
    // This is the top level function which takes all the sub-parts and combines them into the final `Err`
    def format(pos: Position, source: Source, lines: ErrorInfoLines): Err

    type Position
    type Source
    type ErrorInfoLines
    type ExpectedItems
    type Messages
    type UnexpectedLine
    type ExpectedLine
    type Message
    type LineInfo
    type Item
    type Raw <: Item
    type Named <: Item
    type EndOfInput <: Item

    ...
}
```

Wow, that's a lot of types! Essentially, each concrete implementation of this trait must specify
what each of those types are. This means that the representation of the error is as flexible as
possible. In the `format` method, you can see that the types `Position`, `Source`, and
`ErrorInfoLines` are all referenced. Indeed, you can also see these marked on _both_ diagrams: in
other words, `format` is responsible for the general shape of _both_ types of error message.

To understand how these might come about, let's take a step "into" the formatter to find the sources
of values for `Position`, `Source`, and `ErrorInfoLines`:

```scala
trait ErrorBuilder[Err] {
    ...

    def pos(line: Int, col: Int): Position
    def source(sourceName: Option[String]): Source

    def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines
    def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines

    ...
}
```

Hopefully, you can start to see how this might be structured:

* To get a `Position` value for the final error message, the line and column information is fed
  _straight_ from the parser into the `pos` method, which can then hand back the "formatted" position.
* To get the `Source` name, the filename (if it exists!) is fed into the `source` method, which can
  then be fed into `format` by the internals of Parsley.
* To collect up all the `ErrorInfoLines` there are two possible approaches depending on whether the
  error is vanilla or specialised. In both cases, the relevant information is passed in and can be
  "formatted" into whatever `ErrorInfoLines` is: for instance, the default in Parsley has
  `type ErrorInfoLines = Seq[String]`. Neither of these two methods take raw information from the
  parser, they have clearly been fed through another part of the formatter, given their types.

I won't continue traversing deeper and deeper into the system, because it's just going to be the
same idea over and over again. But I will note all the "terminal" methods that do take information
directly from the parser:

```scala
trait ErrorBuilder[Err] {
    ...

    def pos(line: Int, col: Int): Position
    def source(sourceName: Option[String]): Source

    def reason(reason: String): Message
    def message(msg: String): Message
    def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo
    val numLinesBefore: Int
    val numLinesAfter: Int

    def raw(item: String): Raw
    def named(item: String): Named
    val endOfInput: EndOfInput

    def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token
}
```

The two attributes `numLinesBefore` and `numLinesAfter` are used by the Parsley internals to decide
how many raw lines of input both before and after the problematic line to provide to `lineInfo`. In
a pinch, overriding these values from `DefaultErrorBuilder` is a quick way of changing how specific
your errors are to other lines in the input. The `unexpectedToken` method is
special, but I'll leave a discussion of this to another page. All of the other methods in the `ErrorBuilder` will make
use of the refined results from the methods above.

I hope that, by this point, you have a reasonable idea of how this system all ties together. But, if
you don't, or you want an example, let's take a look at how Parsley's own unit tests format error
messages to be easier to pattern match on and test against.

## Example: Unit Tests
When testing parsers, it is sometimes worth checking that the error messages contain the _correct_
content. Usually, the order doesn't matter, but the presence or absence of certain items is important.
This makes `String` the _worst_ possible choice for testing, as order matters and its hard to test
for inclusion without string spliting and other nasty work. Instead, Parsley opts to build its own
`TestError` type that strips away the junk and only keeps the important information, without any
ordering. Let's take a look at it:

```scala
case class TestError(pos: (Int, Int), lines: TestErrorLines)

sealed trait TestErrorLines
case class VanillaError(unexpected: Option[TestErrorItem], expecteds: Set[TestErrorItem], reasons: Set[String]) extends TestErrorLines
case class SpecialisedError(msgs: Set[String]) extends TestErrorLines

sealed trait TestErrorItem
case class TestRaw(item: String) extends TestErrorItem
case class TestNamed(item: String) extends TestErrorItem
case object TestEndOfInput extends TestErrorItem
```

This type represents the vanilla and specialised errors with separate case classes, and stores the
position as a pair of ints. All of the error items in the messages are stored in a set to avoid
ordering issues, and the line info that provides the surrounding input context is not present, as
it isn't critical to the tests. This is _very_ stripped down, essentially, but gets the job done.

So, how is the builder implemented? Well, we are going to make a subclass that extends `ErrorBuilder[TestError]`
and proceed from there. We'll start with the stub implementation and fill each in from there:

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): TestError = ???

    type Position
    override def pos(line: Int, col: Int): Position = ???

    type Source
    override def source(sourceName: Option[String]): Source = ???

    type ErrorInfoLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = ???
    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = ???

    type ExpectedItems
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = ???

    type Messages
    override def combineMessages(alts: Seq[Message]): Messages = ???

    type UnexpectedLine
    override def unexpected(item: Option[Item]): UnexpectedLine = ???
    type ExpectedLine
    override def expected(alts: ExpectedItems): ExpectedLine = ???

    type Message
    override def reason(reason: String): Message = ???
    override def message(msg: String): Message = ???

    type LineInfo
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo = ???

    type Item
    type Raw <: Item
    type Named <: Item
    type EndOfInput <: Item
    override def raw(item: String): Raw = ???
    override def named(item: String): Named = ???
    override val endOfInput: EndOfInput = ???

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int = 0
}
```

Quickly note the addition of `with tokenextractors.MatchParserDemand` in the
declaration line: this is used to provide the implementation of `unexpectedToken` for us, for more information see "Advanced Error Messages".
The first task is to work out what each of the types should be. Let's start by identifying
the components of the error message we _don't_ need and set their type to `Unit`. This provides a
trivial implementation of the relevant methods: they just return `()`. The error format we defined
didn't require any line info, or the source file:

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    ...

    type Source = Unit
    override def source(sourceName: Option[String]): Source = ()

    ...

    type LineInfo = Unit
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo = ()

    ...
}
```

Easy. Now, lets work top down to fill in the rest! The top-most method is `format`, which we know
needs to return a `TestError`. In this case we are given a `Position`, a `Unit`, and some `ErrorInfoLines`.
Well, the constructor for `TestError` requires `(Int, Int)` and `TestErrorLines`, so this helps us
fill in some more types, and the definition of `format` can just invoke the constructor:

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): TestError = TestError(pos, lines)

    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = ???

    ...

    type ErrorInfoLines = TestErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = ???
    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = ???

    ...
}
```

Right, well, `pos` is easy to define, we need to return `(Int, Int)`, so we just tuple them up.
The `TestErrorLines` trait has two sub-classes: `VanillaError` and `SpecialisedError`. This means
that these two constructors will be used in the `vanillaError` and `specialisedError` implementations.
This allows us to resolve a few more types: we know `LineInfo = Unit` already, so lets compare how
the _other_ arguments of these methods line up to the constructors:

```scala
def        vanillaError(unexpected: UnexpectedLine,        expected:  ExpectedLine,       reasons: Messages):           TestErrorLines
case class VanillaError(unexpected: Option[TestErrorItem], expecteds: Set[TestErrorItem], reasons: Set[String]) extends TestErrorLines

def        specialisedError(msgs: Messages   ):        TestErrorLines
case class SpecialisedError(msgs: Set[String]) extends TestErrorLines
```

This tells us the following:

```scala
type UnexpectedLine = Option[TestErrorItem]
type ExpectedLine = Set[TestErrorItem]
type Messages = Set[String]
```

Great, let's fill those types in as well as the, again straightforward, implementation:

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    ...

    override def pos(line: Int, col: Int): Position = (line, col)

    ...

    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
        VanillaError(unexpected, expected, reasons)
    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs)

    ...

    type Messages = Set[String]
    override def combineMessages(alts: Seq[Message]): Messages = ???

    type UnexpectedLine = Option[TestErrorItem]
    override def unexpected(item: Option[Item]): UnexpectedLine = ???
    type ExpectedLine = Set[TestErrorItem]
    override def expected(alts: ExpectedItems): ExpectedLine = ???

    ...
}
```

These three new methods don't have much choice about how to plumb them together. It looks as if
`Message = String`, so `combineMessages` just needs to convert the sequence of messages into a set.
The `unexpected` method is converting an `Option[Item]` to an `Option[TestErrorItem]`, so it seems
like `Item = TestErrorItem` and this is the identity function. In a similar vein `ExpectedItems = Set[TestErrorItem]`
and this can be the identity function too.

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    ...

    type ExpectedItems = Set[TestErrorItem]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = ???

    ...

    override def unexpected(item: Option[Item]): UnexpectedLine = item
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    ...

    type Message = String
    override def reason(reason: String): Message = ???
    override def message(msg: String): Message = ???

    ...

    type Item = TestErrorItem
    type Raw <: Item
    type Named <: Item
    type EndOfInput <: Item
    override def raw(item: String): Raw = ???
    override def named(item: String): Named = ???
    override val endOfInput: EndOfInput = ???

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int = 0
}
```

This is the final stretch now: we know that, actually `Set[Item] =:= ExpectedItems` already, since
`ExpectedItems` is a `Set[TestErrorItem]` and `Item = TestErrorItem`, so `combineExpectedItems` is,
once again, the identity function. This is the case for `reason` and `message` too! Finally, we know
that `Item = TestErrorItem` and it has three subtypes called `Raw`, `Named`, and `EndOfInput`.
Happily, `TestErrorItem` has three subtypes called `TestRaw`, `TestNamed`, and `TestEndOfInput`, so
these will match up nicely! The three relevant methods can just invoke the constructors:

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    ...

    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    ...

    type Raw = TestRaw
    type Named = TestNamed
    type EndOfInput = TestEndOfInput.type
    override def raw(item: String): Raw = TestRaw(item)
    override def named(item: String): Named = TestNamed(item)
    override val endOfInput: EndOfInput = TestEndOfInput

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int = 0
}
```

Phew! Let's take a look at the finished result:

```scala
class TestErrorBuilder extends ErrorBuilder[TestError] with tokenextractors.MatchParserDemand {
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): TestError = TestError(pos, lines)

    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Unit
    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = TestErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
        VanillaError(unexpected, expected, reasons)
    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs)

    type ExpectedItems = Set[TestErrorItem]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    type Messages = Set[String]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    type UnexpectedLine = Option[TestErrorItem]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
    type ExpectedLine = Set[TestErrorItem]
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = Unit
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo = ()

    type Item = TestErrorItem
    type Raw = TestRaw
    type Named = TestNamed
    type EndOfInput = TestEndOfInput.type
    override def raw(item: String): Raw = TestRaw(item)
    override def named(item: String): Named = TestNamed(item)
    override val endOfInput: EndOfInput = TestEndOfInput

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int = 0
}
```

This could be cleaned up a bit by, for instance, setting `type Messages = Set[Message]` etc. Regardless,
this is a fully working `ErrorBuilder[TestError]`! How do we use it?

Well, we can either define an implicit value like `implicit val eb: ErrorBuilder[TestError] = new TestErrorBuilder`
and then using `parser.parse` will automatically use that value if its in scope, or we
can provide one explicitly by writing something like `parser.parse(input)(new TestErrorBuilder)`.
If there is no `ErrorBuilder` implicitly in scope, Scala is smart enough to go and fetch the one Parsley
defines, which maps to a `DefaultErrorBuilder <: ErrorBuilder[String] with tokenextractors.TillNextWhitespace`, which is what we've been
relying on so far in this wiki.

So what do errors look like if we are using this new `ErrorBuilder`?

```scala
(line 1, column 5):
  unexpected end of input
  expected "(", "negate", digit, or letter
  '-' is a binary operator
  >3+4-
       ^

=====>

builder.format (
    builder.pos(1, 5),
    builder.source(None),
    builder.vanillaError (
        builder.unexpected(Some(builder.endOfInput)),
        builder.expected (
            builder.combineExpectedItems(Set (
                builder.raw("("),
                builder.raw("negate"),
                builder.named("digit"),
                builder.named("letter")
            ))
        ),
        builder.combineMessages(List(
            builder.reason("'-' is a binary operator")
        )),
        builder.lineInfo("3+4-", Nil, Nil, 4)
    )
)

=====>

TestError((1, 5),
    VanillaError(
        Some(TestEndOfInput),
        Set(TestRaw("("),
            TestRaw("negate"),
            TestNamed("digit"),
            TestNamed("letter")),
        Set("'-' is a binary operator")))
```

Cool! Hopefully seeing these examples side-by-side helps settle any last issues you have.
