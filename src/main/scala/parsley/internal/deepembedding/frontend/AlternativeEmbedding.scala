package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}
import parsley.internal.errors.{ErrorItem, Raw, Desc}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.backend

private [parsley] final class <|>[A](p: LazyParsley[A], q: =>LazyParsley[A]) extends Binary[A, A, A](p, q, (l, r) => s"($l <|> $r)", new backend.<|>(_, _))