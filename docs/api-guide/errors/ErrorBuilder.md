{%
laika.title = "`ErrorBuilder`"
%}

## Constructing Custom Errors
By default, `parsley` returns errors that consist of `String`-based
content. However, it is possible to build error messages into a
datatype or format that is user-defined. This is done with the
`ErrorBuilder` typeclass.

The `ErrorBuilder` is pulled in implicitly by the `parse` method of
the `Parsley` type:

```scala
class Parsley[A] {
    def parse[Err: ErrorBuilder](input: String): Result[Err, A]
}
```

This is equivalent to having an implicit parameter of type
`ErrorBuilder[Err]`. As the `ErrorBuilder` companion object has an
implicit value of type `ErrorBuilder[String]` only, the type
`String` is chosen as the default instantiation of `Err` by Scala.
Providing another `ErrorBuilder` implicit object in a tighter
scope (or adding an explicit type ascription with another implicit
object available), you are able to hook in your own type instead.

This page describes how the `ErrorBuilder` is structured, and gives
an example of how to construct a lossy type suitable for unit
testing generated error messages.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.errors.ErrorBuilder`](@:api(parsley.errors.ErrorBuilder)).*
@:@

## Error Message Structure

## The `ErrorBuilder` Typeclass

## Constructing Test Errors
