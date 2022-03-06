package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import parsley.debug.{Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Attempt[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p, "attempt", new backend.Attempt(_))
private [parsley] final class Look[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p, "lookAhead", new backend.Look(_))
private [parsley] final class NotFollowedBy[A](p: LazyParsley[A]) extends ScopedUnary[A, Unit](p, "notFollowedBy", new backend.NotFollowedBy(_))
private [parsley] final class Put[S](val reg: Reg[S], _p: LazyParsley[S])
    extends Unary[S, Unit](_p, c => s"put($reg, $c)", new backend.Put(reg, _)) with UsesRegister
// $COVERAGE-OFF$
private [parsley] final class Debug[A](p: LazyParsley[A], name: String, ascii: Boolean, break: Breakpoint)
    extends Unary[A, A](p, identity[String], new backend.Debug(_, name, ascii, break))
// $COVERAGE-ON$