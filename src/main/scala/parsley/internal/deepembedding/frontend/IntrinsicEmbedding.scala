package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.registers.Reg

import scala.language.higherKinds

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, p: LazyParsley[A], q: =>LazyParsley[B]) extends Binary[A, B, C](p, q) {
    override def pretty(l: String, r: String) = s"lift2(f, $l, $r)"
    override def make(p: StrictParsley[A], q: StrictParsley[B]) = new backend.Lift2(f, p, q)
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, p: LazyParsley[A], q: =>LazyParsley[B], r: =>LazyParsley[C])
    extends Ternary[A, B, C, D](p, q, r) {
    override def pretty(f: String, s: String, t: String) = s"lift3(f, $f, $s, $t)"
    override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]) = new backend.Lift3(f, p, q, r)
}
private [parsley] final class Local[S, A](val reg: Reg[S], p: LazyParsley[S], q: =>LazyParsley[A]) extends Binary[S, A, A](p, q) with UsesRegister {
    override def pretty(l: String, r: String) = s"local($reg, $l, $r)"
    override def make(p: StrictParsley[S], q: StrictParsley[A]) = new backend.Local(reg, p, q)
}