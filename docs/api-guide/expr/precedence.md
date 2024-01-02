{%
laika.title = "`precedence`"
parsley.tabname = "Precedence Combinators"
laika.site.metadata.description = "This page describes how to build precedence tables."
%}

# Precedence Combinators
The chain combinators in `parsley.expr.{chain, infix}` are useful
for handling isolated instances of expression-like behaviours in
a grammar. However, they can be cumbersome to use when there are
multiple interacting layers of expression operators. In these
circumstances, a precedence table is the more appropriate
choice. In `parsley`, the precedence machinery is very general, which can make the documentation intimidating. This page aims to break down how this works, with examples of different uses.
The relevant combinator for turning a precedence table into a
parser is the `precedence` combinator.

## Formulating Precedence
In general, a precedence table is formed up of a collection of
base "atoms", and then one or more levels of operators. The
precise shape these components take depends on how complex the
types of the results are.

The simplest shape of `precedence`
directly takes a variadic list of atoms and then a variadic list
of levels as arguments: the levels lexically closest to the atoms
will be the tightest-binding operators. This shape is discussed
in [Homogeneous Precedence], below.

More generally, however, the `precedence` combinator takes a
*heterogeneous list* of levels of operators, where the base
case of this structure contains the atoms. This can be used
to handle more generic shapes of results, and is discussed
in [Heterogeneous Precedence].

However, both shapes make use of the `Ops` type, which requires
a `Fixity` to be provided to it. First, it is good to understand
the basics of how that works in a simplified presentation: for
now, trust that the types work out. There are five different
`Fixity` values: `InfixL`, `InfixR`, `Prefix`, `Postfix`, and
`InfixN`. These each denote either binary or unary operators,
and where recursion may appear for each of them. `InfixN` is a
non-recursive operator, like `<` in some languages. At its
simplest, `Ops` first takes a `Fixity` and then takes a variadic
number of operators that should have that fixity and all have
the same precedence. This will be explored in more detail in
[`Ops` and `Fixity`].

Throughout this section, the following imports and parsers
are assumed:

```scala mdoc:silent
import parsley.Parsley
import parsley.character.{letter, digit, stringOfSome}
import parsley.implicits.character.stringLift

val int: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
val ident: Parsley[String] = stringOfSome(letter)
```

### Homogeneous Precedence
When the result type of the precedence table is the same throughout the table -- right through to the atoms, this is
a *homogeneous* precedence table. This can be captured by the
`Ops` smart-constructor, and make use of the simple flat
structure. As an example:

```scala mdoc:to-string:nest
import parsley.expr.{precedence, Ops, InfixL}

sealed trait Expr
case class Add(x: Expr, y: Expr) extends Expr
case class Sub(x: Expr, y: Expr) extends Expr
case class Mul(x: Expr, y: Expr) extends Expr
case class Num(n: Int) extends Expr
case class Var(v: String) extends Expr

val expr: Parsley[Expr] =
    precedence(ident.map(Var), int.map(Num))(
        Ops(InfixL)("*" as Mul),
        Ops(InfixL)("+" as Add, "-" as Sub)
    )

expr.parse("x+5*y")
```

In the above example, notice that `Add` and `Sub` are within the
same `Ops`, and therefore are the same precedence; `Mul` is closer to the atoms lexically than `Add` and `Sub` are, so it binds tighter. All levels have been marked as left associative with `InfixL`. When using `Prefix` or `Postfix`, the type of the
parsers in the `Ops` will have type `Parsley[A => A]` instead
of the above `Parsley[(A, A) => A]`.

### `Ops` and `Fixity`
The above example of `precedence` did not elaborate on how the types work to allow the operators to vary in type when the fixity
does. Here is the type of the `Ops` constructor from above:

```scala
object Ops {
    def apply[A](fixity: Fixity)(ops: Parsley[fixity.Op[A, A]]*): Ops[A, A]
}
```

There are a few things to note here. First is that the type
returned is `Ops[A, A]`: this means that the input type to this
precedence layer matches the output type (this is because it's
homogeneous). Second is that the type that the operators must
return is `fixity.Op[A, A]`: this is known as a *path-dependent type*. Each object of type `Fixity` will describe the shape of
operators that match that fixity, the two type parameters are
the input and output of the operator (again, for homogeneous precedence, these will coincide). This is formulated as follows:

```scala
sealed trait Fixity {
    type Op[A, B]
}
case object InfixL  { type Op[A, B] = (B, A) => B }
case object InfixR  { type Op[A, B] = (A, B) => B }
case object InfixN  { type Op[A, B] = (A, A) => B }
case object Prefix  { type Op[A, B] = B => B }
case object Postfix { type Op[A, B] = B => B }
```

These types align with those seen in `parsley.expr.infix`, as
well as `Prefix` and `Postfix` matching the homogeneous shape in
`parsley.expr.chain.{prefix, postfix}`. As such, when a specific
fixity is provided to `Ops`, it also fixes the type of the
operators in the next set of parentheses.

When considering heterogeneous combinators, the types `A` and
`B` can indeed vary, and the more general `SOps` and `GOps` will
be explored in the next section.

### Heterogeneous Precedence
When the results of the precedence table is not the same throughout, it is necessary to generalise the machinery to
make use of either `SOps` or `GOps`:

```scala
object SOps {
    def apply[B, A <: B](fixity: Fixity)(ops: Parsley[fixity.Op[A, B]]*): Ops[A, B]
}

object GOps {
    def apply[A, B](fixity: Fixity)(ops: Parsley[fixity.Op[A, B]]*)
                   (implicit wrap: A => B): Ops[A, B]
}
```

The `SOps` object allows the input and output to the layer to vary so long as they are in a sub-type relation: this is the most common form of heterogeneous hierachy that leverages Scala's strengths. Otherwise, `GOps` handles any arbitrary relationship between the types, so long as there is a known implicit `A => B` conversion.

Since the types between layers differ, a variadic argument list cannot be used to collect them together. Instead, the `Ops` are
stitched together into a `Prec[A]` structure:

```scala
sealed trait Prec[A] {
    def +:[B](ops: Ops[A, B]): Prec[B]
    def :+[B](ops: Ops[A, B]): Prec[B]
}
case class Atoms[A](atoms: Parsley[A]*) extends Prec[A]
```

The atoms are placed into `Atoms`, which is the base case of the
list. The `+:` and `:+` operators attach an additional layer
onto the table, adapting the existing tables input type into a
new output type via the given operators. Again, levels closer
to the `Atoms` will bind tighter. As an example:

```scala mdoc:to-string:nest
import parsley.expr.{precedence, SOps, Atoms, InfixL}

sealed trait Expr
case class Add(x: Expr, y: Term) extends Expr
case class Sub(x: Expr, y: Term) extends Expr
sealed trait Term extends Expr
case class Mul(x: Term, y: Atom) extends Term
sealed trait Atom extends Term
case class Num(n: Int) extends Atom
case class Var(v: String) extends Atom

val expr: Parsley[Expr] =
    precedence {
        Atoms(ident.map(Var), int.map(Num)) :+
        SOps(InfixL)("*" as Mul) :+
        SOps(InfixL)("+"as Add, "-" as Sub)
    }

expr.parse("x+5*y")
```

Here, the types within the syntax tree are very specific about
the fact that all three operators are left associative. To be
able to use this type with precedence `SOps` is required, since
the outer layer takes in `Term`s and produces `Expr`s, the
middle layer takes `Atom`s and produces `Term`s, and the
inner-most atoms produce `Atom`s. Notice that, if the layers
are incorrectly stitched together, the table does not typecheck:

```scala mdoc:fail:nest
import parsley.expr.InfixR

val expr: Parsley[Expr] =
    precedence {
        Atoms(ident.map(Var), int.map(Num)) :+
        SOps(InfixR)("*" as Mul) :+
        SOps(InfixL)("+" as Add, "-" as Sub)
    }
```
```scala mdoc:fail:nest
val expr: Parsley[Expr] =
    precedence {
        Atoms(ident.map(Var), int.map(Num)) :+
        SOps(InfixL)("+" as Add, "-" as Sub) :+
        SOps(InfixL)("*" as Mul)
    }
```
