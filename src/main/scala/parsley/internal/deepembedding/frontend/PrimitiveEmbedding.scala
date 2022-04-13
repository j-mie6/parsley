package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.debug.{Breakpoint, EntryBreak, ExitBreak, FullBreak}
import parsley.registers.Reg

import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.machine.instructions

private [parsley] final class Attempt[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p) {
    // $COVERAGE-OFF$
    override val name: String = "attempt"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Attempt(p)
}
private [parsley] final class Look[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p) {
    // $COVERAGE-OFF$
    override val name: String = "lookAhead"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Look(p)
}
private [parsley] final class NotFollowedBy[A](p: LazyParsley[A]) extends ScopedUnary[A, Unit](p) {
    // $COVERAGE-OFF$
    override val name: String = "notFollowedBy"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[Unit] = new backend.NotFollowedBy(p)
}
private [parsley] final class Put[S](val reg: Reg[S], _p: LazyParsley[S]) extends Unary[S, Unit](_p) with UsesRegister {
    // $COVERAGE-OFF$
    def pretty(c: String): String = s"put($reg, $c)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[S]): StrictParsley[Unit] = new backend.Put(reg, p)
}
// $COVERAGE-OFF$
private [parsley] final class Debug[A](p: LazyParsley[A], name: String, ascii: Boolean, break: Breakpoint) extends Unary[A, A](p) {
    // $COVERAGE-OFF$
    def pretty(p: String): String = p
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Debug(p, name, ascii, break)
}
// $COVERAGE-ON$
