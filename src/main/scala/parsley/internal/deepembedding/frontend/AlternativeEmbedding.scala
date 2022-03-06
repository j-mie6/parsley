package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}
import parsley.internal.errors.{ErrorItem, Raw, Desc}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class <|>[A](p: LazyParsley[A], q: =>LazyParsley[A]) extends Binary[A, A, A](p, q) {
    override def pretty(l: String, r: String) = s"($l <|> $r)"
    override def make(p: StrictParsley[A], q: StrictParsley[A]) = new backend.<|>(p, q)
}