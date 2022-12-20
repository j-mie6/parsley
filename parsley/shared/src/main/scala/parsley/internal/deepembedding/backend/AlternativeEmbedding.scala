/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.XAssert._

import parsley.internal.collection.mutable.SinglyLinkedList, SinglyLinkedList.LinkedListIterator
import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.errors.{ExpectDesc, ExpectItem, ExpectRaw}
import parsley.internal.machine.instructions

// scalastyle:off underscore.import
import Choice._
import StrictParsley.InstrBuffer
// scalastyle:on underscore.import

private [deepembedding] final class Choice[A](private [backend] val alt1: StrictParsley[A],
                                              private [backend] var alt2: StrictParsley[A],
                                              private [backend] var alts: SinglyLinkedList[StrictParsley[A]]) extends StrictParsley[A] {
    def inlinable: Boolean = false

    override def optimise: StrictParsley[A] = this match {
        // Assume that this is eliminated first, so not other alts
        case (u: Pure[_]) <|> _ => u
        case Empty <|> q => q
        case p <|> Empty => p
        case Choice(ret@Choice(_, _, lalts: SinglyLinkedList[StrictParsley[A]]),
                    Choice(ralt1, ralt2, ralts: SinglyLinkedList[StrictParsley[A]]),
                    alts) =>
            assume(!alts.exists(_.isInstanceOf[Choice[_]]), "alts can never contain a choice")
            assume(!lalts.exists(_.isInstanceOf[Choice[_]]), "ralts can never contain a choice")
            assume(!ralts.exists(_.isInstanceOf[Choice[_]]), "lalts can never contain a choice")
            lalts.addOne(ralt1)
            lalts.addOne(ralt2)
            lalts.stealAll(ralts)
            lalts.stealAll(alts)
            ret
        case Choice(ret@Choice(_, _, alts: SinglyLinkedList[StrictParsley[A]]), p, alts_) =>
            assume(!alts.exists(_.isInstanceOf[Choice[_]]), "alts can never contain a choice")
            assume(!alts_.exists(_.isInstanceOf[Choice[_]]), "alts_ can never contain a choice")
            alts.addOne(p)
            alts.stealAll(alts_)
            ret
        case Choice(_, Choice(alt1, alt2, alts: SinglyLinkedList[StrictParsley[A]]), alts_) =>
            assume(!alts.exists(_.isInstanceOf[Choice[_]]), "alts can never contain a choice")
            assume(!alts_.exists(_.isInstanceOf[Choice[_]]), "alts_ can never contain a choice")
            this.alt2 = alt1
            this.alts = alts
            alts.prependOne(alt2)
            alts.stealAll(alts_)
            this
        case _ => this
    }

    override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        this.tablify match {
            // If the tablified list is single element (or the next is None), that implies that this should be generated as normal!
            case (_ :: Nil) | (_ :: (_, None) :: Nil) => codeGenChain(alt1, alt2, alts.iterator)
            case tablified => codeGenJumpTable(tablified)
        }
    }

    private def tablify: List[(StrictParsley[_], Option[(Char, Option[ExpectItem], Int, Boolean)])] = {
        tablify((alt1::alt2::alts).iterator, mutable.ListBuffer.empty, mutable.Set.empty, None)
    }

    @tailrec private def tablify(
            it: LinkedListIterator[StrictParsley[A]],
            acc: mutable.ListBuffer[(StrictParsley[_], Option[(Char, Option[ExpectItem], Int, Boolean)])],
            seen: mutable.Set[Char],
            lastSeen: Option[Char]
        ): List[(StrictParsley[_], Option[(Char, Option[ExpectItem], Int, Boolean)])] = it.next() match {
        case u if it.hasNext =>
            val leadingInfo = tablable(u, backtracks = false)
            leadingInfo match {
                // if we've not seen it before that's ok
                case Some((c, _, _, _)) if !seen.contains(c) => tablify(it, acc += ((u, leadingInfo)), seen += c, Some(c))
                // if we've seen it, then only a repeat of the last character is allowed
                case Some((c, _, _, _)) if lastSeen.contains(c) => tablify(it, acc += ((u, leadingInfo)), seen += c, lastSeen)
                // if it's seen and not the last character we have to stop
                case _ => (acc += ((new Choice(u, it.next(), it.remaining), None))).toList
            }
        case p => (acc += ((p, tablable(p, backtracks = false)))).toList
    }

    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- alt1.pretty
            s2 <- alt2.pretty
            ss <- ContOps.sequence(alts.map(_.pretty[Cont, R]).toList)
        } yield (s1::s2::ss).mkString("choice(", ", ", ")")
    // $COVERAGE-ON$
}

private [backend] object Choice {
    private def unapply[A](self: Choice[A]): Some[(StrictParsley[A], StrictParsley[A], SinglyLinkedList[StrictParsley[A]])] =
        Some((self.alt1, self.alt2, self.alts))

    private def scopedState[A, Cont[_, +_]: ContOps, R](p: StrictParsley[A])(generateHandler: =>Cont[R, Unit])
                                                       (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        val skip = state.freshLabel()
        instrs += new instructions.PushHandlerAndState(handler, saveHints = true, hideHints = false)
        suspend(p.codeGen[Cont, R]) >> {
            instrs += new instructions.JumpAndPopState(skip)
            instrs += new instructions.Label(handler)
            generateHandler |> {
                instrs += new instructions.Label(skip)
            }
        }
    }

    private def scopedCheck[A, Cont[_, +_]: ContOps, R](p: StrictParsley[A])(generateHandler: =>Cont[R, Unit])
                                                       (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        val skip = state.freshLabel()
        instrs += new instructions.PushHandlerAndCheck(handler, saveHints = true)
        suspend(p.codeGen[Cont, R]) >> {
            instrs += new instructions.JumpAndPopCheck(skip)
            instrs += new instructions.Label(handler)
            generateHandler |> {
                instrs += new instructions.Label(skip)
            }
        }
    }

    private def codeGenChain[A, Cont[_, +_]: ContOps, R](alt1: StrictParsley[A], alt2: StrictParsley[A], alts: Iterator[StrictParsley[A]])
                                                        (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        if (alts.hasNext) {
            val alt3 = alts.next()
            codeGenAlt(alt1, suspend(codeGenChain[A, Cont, R](alt2, alt3, alts)))
        }
        else alt2 match {
            case Pure(x) => alt1 match {
                case Attempt(u) => scopedState(u) {
                    instrs += new instructions.AlwaysRecoverWith[A](x)
                    result(())
                }
                case u => scopedCheck(u) {
                    instrs += new instructions.RecoverWith[A](x)
                    result(())
                }
            }
            case v => codeGenAlt(alt1, suspend(v.codeGen[Cont, R]))
        }
    }

    // Why is rest lazy? because Cont could be Id, and Id forces the argument immediately!
    private def codeGenAlt[A, Cont[_, +_]: ContOps, R](p: StrictParsley[A], rest: =>Cont[R, Unit])
                                                      (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val merge = state.getLabel(instructions.MergeErrorsAndFail)
        p match {
            case Attempt(u) => scopedState(u) {
                instrs += new instructions.RestoreAndPushHandler(merge)
                rest |> {
                    instrs += instructions.ErrorToHints
                }
            }
            case u => scopedCheck(u) {
                instrs += new instructions.Catch(merge)
                rest |> {
                    instrs += instructions.ErrorToHints
                }
            }
        }

    }

    @tailrec private def propagateExpecteds(expectedss: List[(Set[ExpectItem], Boolean)], all: Set[ExpectItem], corrected: List[Set[ExpectItem]]):
        List[Set[ExpectItem]] = expectedss match {
        case (expecteds, backtrack) :: expectedss => propagateExpecteds(expectedss, all, (if (backtrack) all else expecteds) :: corrected)
        case Nil => corrected
    }

    private def codeGenRoots[Cont[_, +_]: ContOps, R](roots: List[List[StrictParsley[_]]], ls: List[Int], end: Int)
                                                               (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = roots match {
        case root::roots_ =>
            instrs += new instructions.Label(ls.head)
            codeGenAlternatives(root) >> {
                instrs += instructions.ErrorToHints
                instrs += new instructions.JumpAndPopCheck(end)
                suspend(codeGenRoots[Cont, R](roots_, ls.tail, end))
            }
        case Nil => result(())
    }
    private def codeGenAlternatives[Cont[_, +_]: ContOps, R]
            (alts: List[StrictParsley[_]])
            (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen
        case alt::alts_ => codeGenAlt(alt, suspend(codeGenAlternatives[Cont, R](alts_)))
    }
    // TODO: Refactor
    @tailrec private def foldTablified(tablified: List[(StrictParsley[_], (Char, Option[ExpectItem], Int, Boolean))], // scalastyle:ignore parameter.number
                                       labelGen: CodeGenState,
                                       roots: mutable.Map[Char, mutable.ListBuffer[StrictParsley[_]]],
                                       backtracking: mutable.Map[Char, Boolean],
                                       leads: mutable.ListBuffer[Char],
                                       labels: mutable.ListBuffer[Int],
                                       size: Int,
                                       expecteds: Set[ExpectItem],
                                       // build in reverse!
                                       expectedss: List[Set[ExpectItem]]):
        (List[List[StrictParsley[_]]], List[Char], List[Int], Int, Set[ExpectItem], List[(Set[ExpectItem], Boolean)]) = tablified match {
        case (root, (c, expected, _size, backtracks))::tablified_ =>
            if (roots.contains(c)) {
                roots(c) += root
                backtracking(c) = backtracking(c) && backtracks
                foldTablified(tablified_, labelGen, roots, backtracking, leads, labels,
                              Math.max(size, _size), expected.fold(expecteds)(expecteds + _), expectedss)
            }
            else {
                roots(c) = mutable.ListBuffer(root)
                backtracking(c) = backtracks
                foldTablified(tablified_, labelGen, roots, backtracking, leads += c, labels += labelGen.freshLabel(),
                              Math.max(size, _size), expected.fold(expecteds)(expecteds + _), expecteds :: expectedss)
            }
        case Nil => (leads.toList.map(roots(_).toList), leads.toList, labels.toList, size,
                    // When 2.12 is dropped, the final toList can go
                     expecteds, expectedss.zip(leads.toList.reverseIterator.map(backtracking(_)).toList))
    }

    @tailrec private def tablable(p: StrictParsley[_], backtracks: Boolean): Option[(Char, Option[ExpectItem], Int, Boolean)] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case ct@CharTok(d)                       =>
            Some((d, ct.expected.fold[Option[ExpectItem]](Some(ExpectRaw(d)))(n => if (n.nonEmpty) Some(ExpectDesc(n)) else None), 1, backtracks))
        case st@StringTok(s)                     =>
            Some((s.head, st.expected.fold[Option[ExpectItem]](Some(ExpectRaw(s)))(n => if (n.nonEmpty) Some(ExpectDesc(n)) else None), s.size, backtracks))
        //case op@MaxOp(o)                         => Some((o.head, Some(Desc(o)), o.size, backtracks))
        //case _: StringLiteral | RawStringLiteral => Some(('"', Some(Desc("string")), 1, backtracks))
        // TODO: This can be done for case insensitive things too, but with duplicated branching
        case t@Specific(s) if t.caseSensitive    => Some((s.head, Some(ExpectDesc(s)), s.size, backtracks))
        case Attempt(t)                          => tablable(t, backtracks = true)
        case (_: Pure[_]) <*> t                  => tablable(t, backtracks)
        case Lift2(_, t, _)                      => tablable(t, backtracks)
        case Lift3(_, t, _, _)                   => tablable(t, backtracks)
        case t <*> _                             => tablable(t, backtracks)
        case Seq(before, r, _)                   => tablable(before.headOption.getOrElse(r), backtracks)
        case _                                   => None
    }

    private def codeGenJumpTable[Cont[_, +_]: ContOps, R, A](tablified: List[(StrictParsley[_], Option[(Char, Option[ExpectItem], Int, Boolean)])])
                                                    (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val needsDefault = tablified.last._2.isDefined
        val end = state.freshLabel()
        val default = state.freshLabel()
        val merge = state.getLabel(instructions.MergeErrorsAndFail)
        val tablified_ = tablified.collect {
            case (root, Some(info)) => (root, info)
        }
        val (roots, leads, ls, size, expecteds, expectedss) = foldTablified(tablified_, state, mutable.Map.empty, mutable.Map.empty,
                                                                            mutable.ListBuffer.empty, mutable.ListBuffer.empty, 0, Set.empty, Nil)
        instrs += new instructions.JumpTable(leads, ls, default, merge, size, expecteds, propagateExpecteds(expectedss, expecteds, Nil))
        codeGenRoots(roots, ls, end) >> {
            instrs += new instructions.Catch(merge) //This instruction is reachable as default - 1
            instrs += new instructions.Label(default)
            if (needsDefault) {
                instrs += instructions.Empty
                result(instrs += new instructions.Label(end))
            }
            else {
                tablified.last._1.codeGen |> {
                    instrs += instructions.ErrorToHints
                    instrs += new instructions.Label(end)
                }
            }
        }
    }
}


private [deepembedding] object <|> {
    def apply[A](left: StrictParsley[A], right: StrictParsley[A]): Choice[A] = new Choice(left, right, SinglyLinkedList.empty)
    private [backend] def unapply[A](self: Choice[A]): Some[(StrictParsley[A], StrictParsley[A])] = {
        if (self.alts.nonEmpty) throw new IllegalStateException("<|> assumed, but full Choice given") // scalastyle:ignore throw
        Some((self.alt1, self.alt2))
    }
}
