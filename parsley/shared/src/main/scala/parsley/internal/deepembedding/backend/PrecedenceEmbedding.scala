/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.singletons.Pure
import parsley.internal.collection.mutable.SinglyLinkedList
import parsley.internal.machine.instructions
import parsley.internal.machine.instructions.{ShuntToken, Atom, Operator}
import parsley.expr.{Fixity, Prefix}
import parsley.internal.deepembedding.singletons.Fail
import parsley.internal.errors.FlexibleCaret
import parsley.expr.InfixL
import parsley.expr.InfixR
import parsley.expr.Postfix
import parsley.expr.InfixN

private [deepembedding] final class Precedence[A](prefixAtomChoice: StrictParsley[ShuntToken], postfixInfixChoice: StrictParsley[ShuntToken], wraps: Array[Array[Any => Any]]) extends StrictParsley[A] {
    override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = {
        val prefixAtomLabel = state.freshLabel()
        val postfixInfixLabel = state.freshLabel()
        val shuntLabel = state.freshLabel()
        instrs += new instructions.Fresh(instructions.ShuntingYardState.empty)
        instrs += new instructions.PushHandlerAndState(shuntLabel)
        instrs += new instructions.Label(prefixAtomLabel)
        suspend(prefixAtomChoice.codeGen[M, R](producesResults = true)) >> {
            instrs += new instructions.Jump(shuntLabel)
            instrs += new instructions.Label(postfixInfixLabel)
            suspend(postfixInfixChoice.codeGen[M, R](producesResults = true)) |> {
                instrs += new instructions.Label(shuntLabel)
                instrs += new instructions.Shunt(prefixAtomLabel, postfixInfixLabel, wraps)
                if (!producesResults) instrs += instructions.Pop
            }
        }
    }

    override private[deepembedding] def inlinable: Boolean = false

    // $COVERAGE-OFF$
    override private[deepembedding] def pretty: String = "precedence" // TODO: implement pretty printing
    // $COVERAGE-ON$
}

// TODO: refactor/remove need for StrictPrec
private [deepembedding] object Precedence {
    def apply[A](table: StrictPrec): Precedence[A] = {
        val (prefixAtomOptions, postfixInfixOptions) = buildChoiceOptions(table)
        val prefixAtomChoice = buildChoiceNode(prefixAtomOptions)
        val postfixInfixChoice = buildChoiceNode(postfixInfixOptions)
        new Precedence(prefixAtomChoice, postfixInfixChoice, buildPrecomputedWraps(table.wraps))
    }

    private def buildChoiceNode[A](options: List[StrictParsley[A]]): StrictParsley[A] = options.map(_.optimise) match {
        case Nil => new Fail(new FlexibleCaret(0))
        case p :: Nil => p
        case p1 :: p2 :: Nil => <|>(p1, p2)
        case p1 :: p2 :: p3 :: tail => Choice.unsafe(p1, p2, SinglyLinkedList(p3, tail: _*))
    }

    private def unwrapChoices(ps: List[StrictParsley[Any]]): List[StrictParsley[Any]] = ps.flatMap {
        case Choice(alt1, alt2, alts) => alt1 :: alt2 :: alts.toList
        case p => p :: Nil
    }

    private def buildOpChoice(op: StrictOp): StrictParsley[Operator] = {
        val opFn = op.fixity match {
            case InfixL => (x: Any) => new instructions.InfixLOp(x.asInstanceOf[(Any, Any) => Any], op.prec)
            case InfixR => (x: Any) => new instructions.InfixROp(x.asInstanceOf[(Any, Any) => Any], op.prec)
            case Prefix => (x: Any) => new instructions.PrefixOp(x.asInstanceOf[Any => Any], op.prec)
            case Postfix => (x: Any) => new instructions.PostfixOp(x.asInstanceOf[Any => Any], op.prec)
            case InfixN => (x: Any) => new instructions.InfixNOp(x.asInstanceOf[(Any, Any) => Any], op.prec)
        }
        <*>(new Pure(opFn), op.op).optimise
    }

    private def buildChoiceOptions(table: StrictPrec): (List[StrictParsley[ShuntToken]], List[StrictParsley[ShuntToken]]) = (
        unwrapChoices(table.atoms).map(a => <*>(new Pure(r => new Atom(r, table.wraps.length)), a).optimise) ::: table.ops.collect { case op if op.fixity == Prefix => buildOpChoice(op) },
        table.ops.collect { case op if op.fixity != Prefix => buildOpChoice(op) }
    )

    private def buildPrecomputedWraps(wraps: Array[Any => Any]): Array[Array[Any => Any]] = {
        val d = wraps.length + 1
        val output = Array.ofDim[Any => Any](d, d)

        for (i <- 0 until d) output(i)(i) = identity
        for (from <- 0 until d; to <- from - 1 to 0 by -1) {
            output(from)(to) = output(from)(to + 1) match {
                case _: <:<[_, _] => wraps(to) // FIXME: untested
                case prev => wraps(to) match {
                    case _: <:<[_, _] => prev
                    case next => prev andThen next
                }
            }
        }
        output
    }
}

private [deepembedding] final class StrictPrec(val atoms: List[StrictParsley[Any]], val ops: List[StrictOp], val wraps: Array[Any => Any])
private [deepembedding] final class StrictOp(val fixity: Fixity, val op: StrictParsley[Any], val prec: Int)
