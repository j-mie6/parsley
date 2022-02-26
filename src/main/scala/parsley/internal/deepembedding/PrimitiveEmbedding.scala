package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import parsley.debug.{Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import backend.CodeGenState
import backend.StrictParsley, StrictParsley.InstrBuffer

private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: Option[String])
    extends Singleton[Char]("satisfy(f)", new backend.Satisfy(f, expected))
private [parsley] final class Attempt[A](p: Parsley[A]) extends ScopedUnary[A, A](p, "attempt", new backend.Attempt(_))
private [parsley] final class Look[A](p: Parsley[A]) extends ScopedUnary[A, A](p, "lookAhead", new backend.Look(_))
private [parsley] final class NotFollowedBy[A](p: Parsley[A]) extends ScopedUnary[A, Unit](p, "notFollowedBy", new backend.NotFollowedBy(_))

private [deepembedding] final class Rec[A](private [deepembedding] val p: Parsley[A], val strict: backend.Rec[A]) extends Singleton(s"rec($p)", strict)

private [parsley] object Line extends Singleton[Int]("line", backend.Line)
private [parsley] object Col extends Singleton[Int]("col", backend.Col)
private [parsley] final class Get[S](val reg: Reg[S]) extends Singleton[S](s"get($reg)", new backend.Get(reg)) with UsesRegister
private [parsley] final class Put[S](val reg: Reg[S], _p: Parsley[S])
    extends Unary[S, Unit](_p, c => s"put($reg, $c)", new backend.Put(reg, _)) with UsesRegister
// $COVERAGE-OFF$
private [parsley] final class Debug[A](p: Parsley[A], name: String, ascii: Boolean, break: Breakpoint)
    extends Unary[A, A](p, identity[String], new backend.Debug(_, name, ascii, break))
// $COVERAGE-ON$