package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.registers.Reg

import scala.language.higherKinds

import parsley.internal.deepembedding.backend

private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, p: LazyParsley[A], q: =>LazyParsley[B]) extends Binary[A, B, C](p, q, new backend.Lift2(f, _, _)) {
    override def pretty(l: String, r: String) = s"lift2(f, $l, $r)"
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, p: LazyParsley[A], q: =>LazyParsley[B], r: =>LazyParsley[C]) extends Ternary[A, B, C, D](p, q, r, new backend.Lift3(f, _, _, _)) {
    override def pretty(f: String, s: String, t: String) = s"lift3(f, $f, $s, $t)"
}
private [parsley] final class Local[S, A](val reg: Reg[S], p: LazyParsley[S], q: =>LazyParsley[A]) extends Binary[S, A, A](p, q, new backend.Local(reg, _, _)) with UsesRegister {
    override def pretty(l: String, r: String) = s"local($reg, $l, $r)"
}