package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import backend.StrictParsley

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A](s"pure($x)", new backend.Pure(x))

private [parsley] final class <*>[A, B](pf: Parsley[A => B], px: =>Parsley[A])
    extends Binary[A => B, A, B](pf, px, (l, r) => s"($l <*> $r)", new backend.<*>(_, _))

private [parsley] final class >>=[A, B](p: Parsley[A], private [>>=] val f: A => Parsley[B])
    extends Unary[A, B](p, l => s"($l >>= ?)", new backend.>>=(_, f))

private [deepembedding] sealed abstract class Seq[A, B, Res](left: Parsley[A], right: =>Parsley[B], pretty: String, make: (StrictParsley[A], StrictParsley[B]) => StrictParsley[Res])
    extends Binary[A, B, Res](left, right, (l, r) => s"($l $pretty $r)", make)
private [parsley] final class *>[A](_p: Parsley[_], _q: =>Parsley[A]) extends Seq[Any, A, A](_p, _q, "*>", new backend.*>(_, _))
private [parsley] final class <*[A](_p: Parsley[A], _q: =>Parsley[_]) extends Seq[A, Any, A](_p, _q, "<*", new backend.<*(_, _))