package parsley.internal.deepembedding.frontend

import scala.language.higherKinds

import parsley.registers.Reg

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, p: LazyParsley[A], q: =>LazyParsley[B]) extends Binary[A, B, C](p, q) {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"lift2(f, $l, $r)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = new backend.Lift2(f, p, q)
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, p: LazyParsley[A], q: =>LazyParsley[B], r: =>LazyParsley[C])
    extends Ternary[A, B, C, D](p, q, r) {
    // $COVERAGE-OFF$
    override def pretty(f: String, s: String, t: String): String = s"lift3(f, $f, $s, $t)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = new backend.Lift3(f, p, q, r)
}
private [parsley] final class Local[S, A](val reg: Reg[S], p: LazyParsley[S], q: =>LazyParsley[A]) extends Binary[S, A, A](p, q) with UsesRegister {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"local($reg, $l, $r)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[S], q: StrictParsley[A]): StrictParsley[A] = new backend.Local(reg, p, q)
}
