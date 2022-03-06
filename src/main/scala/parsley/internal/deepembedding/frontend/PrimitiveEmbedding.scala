package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import parsley.debug.{Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Attempt[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p) {
    override val name = "attempt"
    override def make(p: StrictParsley[A]) = new backend.Attempt(p)
}
private [parsley] final class Look[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p) {
    override val name = "lookAhead"
    override def make(p: StrictParsley[A]) = new backend.Look(p)
}
private [parsley] final class NotFollowedBy[A](p: LazyParsley[A]) extends ScopedUnary[A, Unit](p) {
    override val name = "notFollowedBy"
    override def make(p: StrictParsley[A]) = new backend.NotFollowedBy(p)
}
private [parsley] final class Put[S](val reg: Reg[S], _p: LazyParsley[S]) extends Unary[S, Unit](_p) with UsesRegister {
    def pretty(c: String) = s"put($reg, $c)"
    override def make(p: StrictParsley[S]) = new backend.Put(reg, p)
}
// $COVERAGE-OFF$
private [parsley] final class Debug[A](p: LazyParsley[A], name: String, ascii: Boolean, break: Breakpoint) extends Unary[A, A](p) {
    def pretty(p: String) = p
    override def make(p: StrictParsley[A]) = new backend.Debug(p, name, ascii, break)
}
// $COVERAGE-ON$