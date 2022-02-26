package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.errors.{ErrorItem, Raw, Desc}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [parsley] final class <|>[A](p: Parsley[A], q: =>Parsley[A]) extends Binary[A, A, A](p, q, (l, r) => s"($l <|> $r)", new backend.<|>(_, _))

private [parsley] object Empty extends Singleton[Nothing]("empty", backend.Empty)