package parsley.token

import scala.language.implicitConversions

import parsley.Parsley
import parsley.errors.combinator.markAsToken

private [token] object LexemeImpl {
    implicit def fromSpace(spaces: Parsley[_]): Lexeme = new Lexeme {
        def apply[A](p: Parsley[A]): Parsley[A] = markAsToken(p) <* spaces
    }
    val empty = new Lexeme {
        def apply[A](p: Parsley[A]): Parsley[A] = markAsToken(p)
    }
}
