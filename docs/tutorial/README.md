{%
laika.versioned = true
helium.site.pageNavigation.enabled = false
laika.site.metadata.description = "Roadmap of the parser combinator tutorial."
%}

# Parser Combinator Tutorial

Parsley is a _parser combinator library_.  In contrast to a parser generator library, like
ANTLR, this allows the users to build their parsers as part of the host language: in this case
Scala. This brings a huge amount of power to the fingertips of the programmer, but, admittedly,
embarking on the journey to learning how they work is very intimidating! Personally, I think
the pay-off is still great, and, really, it's quite hard to see why until after you've tried
it. The most important part is not being afraid to play with things.

This series of wiki posts aims to help guide you on your journey to learning parser
combinators. Unfortunately, while the fundamentals carry nicely over to other libraries, such
as `parsec`, `megaparsec` and so on in Haskell (and to a lesser extent the other Scala
libraries, which admittedly have a slightly different feel to them), learning a parser
combinator library is still a specialism, and many of the helpful abstractions and patterns
that Parsley provides may not be available or even possible in some other contexts.

Something I've learnt about this wiki is that people can take it fairly literally at each step
instead of viewing each page as a larger whole. The consequence is that some of the neat
techniques that are presented later in the series may have come too late, and users may have
already implemented something the "long way". My advice is to **keep reading** until the end
before embarking on any serious work using the library. Of course, you don't have to, but I've
distilled _years_ of knowledge into this wiki, and it would be a shame to miss out on it. To
try and make this process easier, I've added a road-map of sorts below, to help you understand
the whole story before you start it.

## The Roadmap
### Basics of Combinators
Our journey starts at the beginning: the very beginning. In many respects, this first post
probably goes into too much detail. However, I think it's very important to see the lower level
primitives to be able to understand the more powerful and convenient abstractions. In
particular, the nuances of backtracking can have a big effect on the error messages of even
correct parsers, so understanding _how_ and _when_ to backtrack is an important skill. That
being said, the way that parsers are written in this post are **not** representative of how
parsers really **should** be written! Instead it demonstrates some recurring _themes_ and some
of the most important ideas.

### Building Expression Parsers (_introduces_ `expr.precedence`)
By the end of the first post, the basic combinators, recursion and the main principles of
combinators have been demonstrated. However, the final example, which sees us write a parser
for boolean expressions, leaves much to be desired. For the uninitiated reader, they'll see
nothing wrong with it, but as the second page shows, there are much easier and more powerful
tools at our disposal for working with expressions. Again, this page starts by showing off the
fundamental building blocks of the expression parser before showing the more commonly used
`precedence` combinator. The reason I chose to take this route is similar to before, it's good
to be able to have a sense of how the more powerful tools were built up, and there are often
opportunities to use these combinators where a `precedence` (which takes the center stage) is
just a bit more clunky.

Be warned that this page doesn't re-introduce any of the material of the previous page, so if a
combinator is used without being explained, you can always check back to the first post (or the
cheatsheet). Also you should be aware that the latter half of the post gets a bit more
technical than most people will a) need to know and b) care about. The reason these sections
were left in was to help the many people who don't like to blindly accept concepts presented to
them without having a, however basic, understanding of how it works. If you're not one of those
people, or you're otherwise not interested, then you can feel free to move on to the next page
at the section _Path-Dependent Typing and `Ops`/`Gops`_.

### Effective Whitespace Parsing
By the third post, we take a (welcome) break from learning new combinators and concepts, and
instead discuss good parser _design_, and the best ways to deal with pesky whitespace in the
input: from the first two posts, we'll already have seen all the tools we need to write correct
parsers, just not the _best_ ways to do so.

Whitespace, simply put, is annoying because it distracts from the rest of the grammar.
Normally, a lexer is used to deal with whitespace, and the grammar more accurately describes
the relationships between tokens. The basic idea behind this page is to demonstrate how we can
start to use Scala's features to develop handy abstractions for ourselves that make whitespace
disappear from the main parser. Again, there are better tools for dealing with these issues
than hand-rolling them ourselves, but in order to use such tools effectively and really
understand their implication, it's a very good idea to understand the fundamentals: the
**fourth** page more effectively deals with the issues highlighted in this page _and_ uses some
of the techniques introduced, in the process refining them.

### Effective Lexing (_introduces_ `Lexer`)
The fourth post builds on the ideas of its predecessor, first outlining the general principles
behind how we write and structure the lexical parsers for our grammar, and then how to
seemlessly integrate them into the parser proper. The ideas here are very similar to those
already laid out previously.

Unlike the third post, here the mighty `Lexer` class is introduced. While it's not always
needed to write parsers, its usefulness, even for just handling whitespace, can't be
understated. It's not always the right tool for the job though, so definitely don't disregard
all the lessons presented before it!

### The _Parser Bridges_ Pattern (_introduces position tracking with_ `line`_,_ `col`_, and_ `pos`)
This page takes a huge leap forward in terms of how parsers are designed and integrated with
the Abstract Syntax Trees they so often produce. An important (and often overlooked) aspect of
parsing with combinators is how position information in the parser is preserved in the
generated structure. In my experience, I've found this is often done as an afterthought, when
the programmer realises that the information is important: for instance, when performing
Semantic Analysis in a compiler... And, usually, its introduction makes a complete mess of an otherwise nice looking parser.

The joy of the _Parser Bridges_ pattern, which this page introduces, is that it separates the building
of the AST during parsing from whether or not that AST node needs position information, or
indeed the mechanics of putting together the components in the right way. This separation
creates a pleasant cleanliness in the main body of the grammar, which by this point now retains
the simple elegance we might expect to see with a plain old BNF representation. If you've been
unconvinced so far that parser combinators look very similar to the grammar they represent,
this may change your perspective.

### _Interlude 1_: Building a Parser for Haskell
By this point, you'll have covered a lot of information:

* Basics of what combinators are and what they are built from
* Cleanly handing expressions with varying precedence and associativities
* How to correctly deal with whitespace
* How to cleanly factor out lexing logic
* How to abstract away the construction of a resulting AST from the grammar

With this wealth of knowledge, you'll have the power to go and write all but the trickiest
of parsers. To demonstrate that, the first of three interludes will work through the structure
and design of a (simplified) Haskell parser with the tools we've accrued so far. Even though
this will help consolidate everything you've been shown by putting it all into practice, there
is still a big chunk of the story missing: error messages.

### Customising Error Messages (_introduces_ `label`, `explain`, _and_ `ErrorBuilder`)
With the mechanics of writing parsers that can _succeed_ out of the way, it's about time to
learn about how to improve error messages that _failing_ parsers produce. By default, the error
messages, whilst not _bad_, aren't nearly as they could be. The `Lexer` class does help with
this for lexemes at least, but that doesn't mean all the work is done: especially in the main
grammar, or for times when `Lexer` was a no-go. Writing good error messages is an art-form, and
so this page takes a more subjective look at the process. For most people, this is just about as
far as you'd need to go.

The second part of this post explains how to use the `ErrorBuilder` mechanism in Parsley to
customise the format of error messages, or even change what type they have. This can be particularly
useful for unit-testing or for standardising parser error messages as part of a larger compiler.

### Advanced Error Messages (_introduces_ `unexpected` _and_ `fail`)
The combinators introduced in the previous page are already pretty good! But there are still
some neat patterns we can use to kick it up a notch or two. In particular, this page introduces
patterns that can be used to _anticipate_ common syntax errors and produce much more
descriptive errors upon encountering them.

### _Interlude 2_: Adding Errors to the Haskell Parser
The second interlude takes the new-found lessons from the previous two pages to augment the
Haskell parser with error messages, illustrating the considerations and patterns in practice.
The reason that Interlude 1 comes before error messages is that, whilst they aren't
particularly obstructive, the error message combinators provide a little extra noise that makes
the core part of the parser a little bit harder to admire, especially for someone who is only
getting to grips with the concepts for the very first time!

This, for almost all use-cases, the end of the story. By this point you'll have all the tools
you need to parse context-free grammars, which make up the vast majority of practical languages
and data formats _and_ generate good error messages for them. If however, you are keen to learn
about context-sensitive grammars, or you are thoroughly engrossed in the story up to this
point, there is one final stretch.

### Indentation Sensitive Parsing
For most languages, the grammar is constructed in such a way that it remains context-free. This
is, primarily, because context-sensitive grammars are a brutal combination of hard to express
and hard to parse efficiently. Indentation-sensitive parsing _can_ be considered an example of
a context-sensitive grammar, though, in practice, some compilers like to shunt the work out to
the lexer to make the grammar context-free again (this is the case with Python).

Using parser combinators though, context-sensitive grammars can be encoded comparatively
naturally! In most other combinator libraries, the `flatMap` (or `>>=`) combinator is used to
deal with context-sensitivity. However, in the Parsley family, the power that `flatMap`
provides comes at a heavy cost to performance. Instead, we reach for stateful parsers called
"references", evoking images of register machines vs stack machines: as we know, register
machines are turing powerful, and can most certainly do the job, no matter the parsing task.

This page provides a more concrete and gentle introduction to using references specifically
demonstrating how to use them to implement combinators for managing indentation-sensitive
workloads in a clean and effective way.

### _Interlude 3_: Supporting the Haskell Offside Rule
As the final act of this series, the last "interlude" (by this point just a finale) takes the
combinators built up in the previous page to add the off-side rule to the Haskell parser: this is
the essence of Haskell's indentation-sensitive syntax.
