{%
laika.title = "`chain`"
%}

# Chain Combinators
The `parsley.expr.chain` module contains a variety of combinators for abstracting, most commonly, the application of
operators to values in expressions. This allows `parsley` to
handle *left recursion* idiomatically. To distinguish between
these chains and the functionality found in [`parsley.expr.infix`][Infix Chain Combinators], it is recommended to always import this module qualified as `import parsley.expr.chain`.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.expr.chain`](@:api(parsley.expr.chain$)).*
@:@

## Binary Chains
The first kind of chains found in `chain` are the binary chains,
which handle infix application of a binary operator to values
in either a left- or right-associative way. These are called `chain.left1` or `chain.right1`. The `1` here means that there
must be at least one value present, though there may be no
operators. As an example:

> <h3>`p op p op p op p op p`</h3>

The above can be parsed using `chain.left1(p, op)` to have the
effect of parsing like:

> <h3>`(((p op p) op p) op p) op p`</h3>

It can also be parded using `chain.right1(p, op)` to have the
effect of parsing like:

> <h3>`p op (p op (p op (p op p)))`</h3>

Both of these combinators share the same type, where the parser
`p: Parsley[A]`, and the parser `op: Parsley[(A, A) => A]`. This
means that the two combinators can be freely swapped between
in an implementation. This is useful when the grammar being
encoded for is fully-associative and the associativity within
the parser is an implementation detail. However, if more type-safety is desired, the `infix.left1` and `infix.right1`
combinators may be more appropriate.

## Unary Chains
