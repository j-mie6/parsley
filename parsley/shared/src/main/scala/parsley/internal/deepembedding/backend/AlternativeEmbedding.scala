/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.XAssert._

import parsley.internal.collection.mutable.SinglyLinkedList, SinglyLinkedList.LinkedListIterator
import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.errors.{ExpectDesc, ExpectItem}
import parsley.internal.machine.instructions

// scalastyle:off underscore.import
import Choice._
import StrictParsley.InstrBuffer
// scalastyle:on underscore.import

// TODO: can we tabilify across a Let?
// FIXME: It's annoying this doesn't work if the first thing is not tablable: let's make it more fine-grained to create groupings?
private [deepembedding] final class Choice[A](private [backend] val alt1: StrictParsley[A],
                                              private [backend] var alt2: StrictParsley[A],
                                              private [backend] var alts: SinglyLinkedList[StrictParsley[A]]) extends StrictParsley[A] {
    def inlinable: Boolean = false

    override def optimise: StrictParsley[A] = this match {
        // Assume that this is eliminated first, so not other alts
        case (u: Pure[_]) <|> _ => u
        case Empty.Zero <|> q => q
        case p <|> Empty.Zero => p
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
        case Choice(_, Choice(alt1_, alt2_, alts: SinglyLinkedList[StrictParsley[A]]), alts_) =>
            assume(!alts.exists(_.isInstanceOf[Choice[_]]), "alts can never contain a choice")
            assume(!alts_.exists(_.isInstanceOf[Choice[_]]), "alts_ can never contain a choice")
            this.alt2 = alt1_
            this.alts = alts
            alts.prependOne(alt2_)
            alts.stealAll(alts_)
            this
        case _ => this
    }

    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = codeGenTablified(this.tablify, producesResults)

    private def tablify: List[Either[StrictParsley[_], List[JumpTableGroup]]] =
        tablify((alt1::alt2::alts).iterator, mutable.ListBuffer.empty, mutable.ListBuffer.empty, mutable.ListBuffer.empty, mutable.Set.empty, None)

    @tailrec private def tablify(
        it: LinkedListIterator[StrictParsley[A]],
        acc: mutable.ListBuffer[Either[StrictParsley[_], List[JumpTableGroup]]],
        tableAcc: mutable.ListBuffer[JumpTableGroup],
        groupAcc: mutable.ListBuffer[TablableChar],
        seen: mutable.Set[Char],
        lastSeen: Option[Char],
    ): List[Either[StrictParsley[_], List[JumpTableGroup]]] = if (it.hasNext) {
        val u = it.next()
        tablable(u, backtracks = false) match {
            // Character, if we've not seen it before that's ok
            case Some(d@TablableCharDesc(c, _, _, _)) if !seen.contains(c) => tablify(it, acc, tableAcc, groupAcc += TablableChar(u, d), seen += c, Some(c))
            // Character, if we've seen it, then only a repeat of the last character is allowed
            case Some(d@TablableCharDesc(c, _, _, _)) if lastSeen.contains(c) => tablify(it, acc, tableAcc, groupAcc += TablableChar(u, d), seen, lastSeen)
            // Character, if it's seen and not the last character we have to stop building the table
            case Some(d@TablableCharDesc(c, _, _, _)) => tablify(it, appendTable(acc, appendGroup(tableAcc, groupAcc)), mutable.ListBuffer.empty, mutable.ListBuffer(TablableChar(u, d)), mutable.Set(c), Some(c))
            // Predicate, this is an option on it's own, create a new group. If it can backtrack, a new table is needed next
            case Some(d@TablablePredDesc(_, _, _, true)) => tablify(it, appendTable(acc, appendGroup(tableAcc, groupAcc) += TablablePred(u, d)), mutable.ListBuffer.empty, mutable.ListBuffer.empty, mutable.Set.empty, None)
            case Some(d: TablablePredDesc) => tablify(it, acc, appendGroup(tableAcc, groupAcc) += TablablePred(u, d), mutable.ListBuffer.empty, seen, lastSeen)
            // Non-tablable, this is a Right(...) in the list
            case _ => tablify(it, appendTable(acc, (appendGroup(tableAcc, groupAcc))) += Left(u), mutable.ListBuffer.empty, mutable.ListBuffer.empty, mutable.Set.empty, None)
        }
    } else appendTable(acc, (appendGroup(tableAcc, groupAcc))).toList

    // if groupAcc is empty, add nothing
    // if groupAcc has 1 option, add the parser straight to acc as Left(...)
    // otherwise, add groupAcc as a Right(...)
    private def appendGroup(
        acc: mutable.ListBuffer[JumpTableGroup],
        groupAcc: mutable.ListBuffer[TablableChar]
    ): mutable.ListBuffer[JumpTableGroup] = if (groupAcc.isEmpty) acc else acc += TablableChars(groupAcc.toList)
    
    private def appendTable(
        acc: mutable.ListBuffer[Either[StrictParsley[_], List[JumpTableGroup]]],
        tableAcc: mutable.ListBuffer[JumpTableGroup]
    ): mutable.ListBuffer[Either[StrictParsley[_], List[JumpTableGroup]]] = if (tableAcc.isEmpty) acc else acc += Right(tableAcc.toList)

    // $COVERAGE-OFF$
    final override def pretty: String = (alt1.pretty::alt2.pretty::alts.map(_.pretty).toList).mkString("choice(", ", ", ")")
    // $COVERAGE-ON$
}

private [backend] object Choice {
    sealed trait TablableDesc
    case class TablableCharDesc(char: Char, expecteds: Iterable[ExpectItem], size: Int, backtracks: Boolean) extends TablableDesc
    case class TablablePredDesc(pred: Char => Boolean, expecteds: Iterable[ExpectItem], size: Int, backtracks: Boolean) extends TablableDesc
    sealed trait Tablable
    case class TablableChar(p: StrictParsley[_], desc: TablableCharDesc) extends Tablable
    case class TablablePred(p: StrictParsley[_], desc: TablablePredDesc) extends Tablable with JumpTableGroup
    sealed trait JumpTableGroup
    case class TablableChars(ops: List[TablableChar]) extends JumpTableGroup

    def unapply[A](self: Choice[A]): Some[(StrictParsley[A], StrictParsley[A], SinglyLinkedList[StrictParsley[A]])] =
        Some((self.alt1, self.alt2, self.alts))

    private def scopedState[A, M[_, +_]: ContOps, R](p: StrictParsley[A], producesResults: Boolean)(generateHandler: =>M[R, Unit])
                                                   (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val handler = state.freshLabel()
        val skip = state.freshLabel()
        // FIXME: check this, this is the only one that uses this instruction, and I think it was a mistake
        instrs += new instructions.PushHandlerAndStateAndClearHints(handler)
        suspend(p.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.JumpAndPopState(skip)
            instrs += new instructions.Label(handler)
            generateHandler |> {
                instrs += new instructions.Label(skip)
            }
        }
    }

    private def scopedCheck[A, M[_, +_]: ContOps, R](p: StrictParsley[A], producesResults: Boolean)(generateHandler: =>M[R, Unit])
                                                   (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val handler = state.freshLabel()
        val skip = state.freshLabel()
        instrs += new instructions.PushHandlerAndClearHints(handler)
        suspend(p.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.JumpAndPopCheck(skip)
            instrs += new instructions.Label(handler)
            generateHandler |> {
                instrs += new instructions.Label(skip)
            }
        }
    }

    private object SingleParserTable {
        def unapply(table: Either[StrictParsley[_], List[JumpTableGroup]]): Option[StrictParsley[_]] = table match {
            case Left(p) => Some(p)
            case Right(TablableChars(TablableChar(p, _) :: Nil) :: Nil) => Some(p)
            case Right(TablablePred(p, _) :: Nil) => Some(p)
            case _ => None
        }
    }

    private def codeGenTablified[A, M[_, +_]: ContOps, R]
        (tablified: List[Either[StrictParsley[_], List[JumpTableGroup]]], producesResults: Boolean)
        (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = tablified match {
        case SingleParserTable(p) :: Nil => p.codeGen(producesResults)
        case Right(table) :: Nil => codeGenJumpTable(table, true, suspend(result(())), producesResults)
        case SingleParserTable(p) :: alts => codeGenAlt(p, suspend(codeGenTablified(alts, producesResults)), producesResults)
        case Right(table) :: alts => codeGenJumpTable(table, false, suspend(codeGenTablified(alts, producesResults)), producesResults)
        case _ => result(())
    }

    // Why is rest lazy? because Cont could be Id, and Id forces the argument immediately!
    private def codeGenAlt[A, M[_, +_]: ContOps, R](p: StrictParsley[A], rest: =>M[R, Unit], producesResults: Boolean)
                                                      (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val merge = state.getLabel(instructions.MergeErrorsAndFail)
        p match {
            case Atomic(u) => scopedState(u, producesResults) {
                instrs += new instructions.RestoreAndPushHandler(merge)
                rest |> {
                    instrs += instructions.ErrorToHints
                }
            }
            case u => scopedCheck(u, producesResults) {
                instrs += new instructions.Catch(merge)
                rest |> {
                    instrs += instructions.ErrorToHints
                }
            }
        }

    }

    @tailrec private def propagateExpecteds(tables: List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)], (Char => Boolean, Int, Iterable[ExpectItem], Boolean)]],
                                            all: Iterable[ExpectItem],
                                            corrected: List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], instructions.JumpTablePredDef]]
                                            ): List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], instructions.JumpTablePredDef]] = tables match {
        case Left(map) :: tables_ => {
            val newMap = map.map {
                case (k, (label, errs, backtrack)) => (k, (label, if (backtrack) all else errs))
            }.to(mutable.Map)
            propagateExpecteds(tables_, all, corrected :+ Left(newMap))
        }
        case Right((pred, label, expecteds, backtrack)) :: tables_ => propagateExpecteds(tables_, all, corrected :+ Right(instructions.JumpTablePredDef(pred, (label, (if (backtrack) all else expecteds)))))
        case Nil => corrected
    }

    private def codeGenRoots[M[_, +_]: ContOps, R](roots: List[(Int, List[StrictParsley[_]])], end: Int, producesResults: Boolean)
                                                 (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = roots match {
        case (l, root)::roots_ =>
            instrs += new instructions.Label(l)
            codeGenAlternatives(root, producesResults) >> {
                instrs += instructions.ErrorToHints
                instrs += new instructions.JumpAndPopCheck(end)
                suspend(codeGenRoots[M, R](roots_, end, producesResults))
            }
        case Nil => result(())
    }
    private def codeGenAlternatives[M[_, +_]: ContOps, R](alts: List[StrictParsley[_]], producesResults: Boolean)
                                                         (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen(producesResults)
        case alt::alts_ => codeGenAlt(alt, suspend(codeGenAlternatives[M, R](alts_, producesResults)), producesResults)
    }

    @tailrec private def foldTablableChars(tablified: List[TablableChar],
                                                  labelGen: CodeGenState,
                                                  roots: mutable.Map[Char, (Int, mutable.ListBuffer[StrictParsley[_]])],
                                                  map: mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)],
                                                  leads: List[Char],
                                                  size: Int,
                                                  expecteds: List[ExpectItem]):
        (List[(Int, List[StrictParsley[_]])], mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)], Int, List[ExpectItem]) // Roots, map, size, expecteds
            = tablified match {
                case TablableChar(root, TablableCharDesc(c, expected, _size, backtracks)) :: tablified_ =>
                    if (roots.contains(c)) {
                        roots(c)._2 += root
                        map(c) = (map(c)._1, map(c)._2, map(c)._3 && backtracks)
                        foldTablableChars(tablified_, labelGen, roots, map, leads,
                                                 Math.max(size, _size), expecteds :++ expected)
                    } else {
                        val label = labelGen.freshLabel()
                        roots(c) = (label, mutable.ListBuffer(root))
                        map(c) = (label, expecteds, backtracks)
                        foldTablableChars(tablified_, labelGen, roots, map, leads :+ c,
                                                 Math.max(size, _size), expecteds :++ expected)
                    }
                case Nil => (
                    leads.map(roots(_)).map({ case (l, ps) => (l, ps.toList) }),
                    map,
                    size,
                    expecteds
                )
            }

    private def foldJumpTableGroups(groups: List[JumpTableGroup], labelGen: CodeGenState):
        (List[(Int, List[StrictParsley[_]])], List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], instructions.JumpTablePredDef]], Int, List[ExpectItem]) =
            foldJumpTableGroups(groups, labelGen, mutable.ListBuffer.empty, mutable.ListBuffer.empty, 0, List.empty)
    
    @tailrec private def foldJumpTableGroups(groups: List[JumpTableGroup],
                                        labelGen: CodeGenState,
                                        rootsAcc: mutable.ListBuffer[(Int, List[StrictParsley[_]])],
                                        tableAcc: mutable.ListBuffer[Either[mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)], (Char => Boolean, Int, Iterable[ExpectItem], Boolean)]],
                                        size: Int,
                                        allExpecteds: List[ExpectItem]):
        (List[(Int, List[StrictParsley[_]])], List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], instructions.JumpTablePredDef]], Int, List[ExpectItem]) = groups match {
            case TablableChars(ops) :: def_ => {
                val (roots, map, size_, expecteds_) = foldTablableChars(ops, labelGen, mutable.Map.empty, mutable.Map.empty, List.empty, size, allExpecteds)
                foldJumpTableGroups(def_, labelGen, rootsAcc.addAll(roots), tableAcc += Left(map), size_, expecteds_)
            }
            case TablablePred(p, TablablePredDesc(pred, expecteds, size_, backtracks)) :: def_ => {
                val label = labelGen.freshLabel()
                foldJumpTableGroups(def_, labelGen, rootsAcc.addOne((label, List(p))), tableAcc += Right((pred, label, expecteds, backtracks)), Math.max(size, size_), allExpecteds :++ expecteds)
            }
            case Nil => (rootsAcc.toList, propagateExpecteds(tableAcc.toList, allExpecteds, List.empty), size, allExpecteds)
        }

    // TODO: `line.zip(col)` will not be caught!!!!
    private def tablable(p: StrictParsley[_], backtracks: Boolean): Option[TablableDesc] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case ct@CharTok(c, _)                    => Some(TablableCharDesc(c, ct.expected.asExpectItems(c), 1, backtracks))
        case ct@SupplementaryCharTok(c, _)       => Some(TablableCharDesc(Character.highSurrogate(c), ct.expected.asExpectItems(Character.toChars(c).mkString), 1, backtracks))
        case st@StringTok(s, _)                  => Some(TablableCharDesc(s.head, st.expected.asExpectItems(s), s.codePointCount(0, s.length), backtracks))
        //case op@MaxOp(o)                         => Some((o.head, Some(Desc(o)), o.size, backtracks))
        //case _: StringLiteral | RawStringLiteral => Some(('"', Some(Desc("string")), 1, backtracks))
        // TODO: This can be done for case insensitive things too, but with duplicated branching
        case t@token.SoftKeyword(s) if t.caseSensitive => Some(TablableCharDesc(s.head, t.expected.asExpectDescs(s), s.codePointCount(0, s.length), backtracks))
        case t@token.SoftOperator(s)             => Some(TablableCharDesc(s.head, t.expected.asExpectDescs(s), s.codePointCount(0, s.length), backtracks))
        case s@Satisfy(pred)                     => Some(TablablePredDesc(pred, s.expected.asExpectDescs, 1, backtracks))
        case Atomic(t)                           => tablable(t, backtracks = true)
        case ErrorLabel(t, label, labels)        => tablable(t, backtracks).map {
            case TablableCharDesc(c, _, width, backtracks) => TablableCharDesc(c, (label +: labels).map(new ExpectDesc(_)), width, backtracks)
            case TablablePredDesc(p, _, width, backtracks) => TablablePredDesc(p, (label +: labels).map(new ExpectDesc(_)), width, backtracks)
        }
        case ErrorHide(t)                        => tablable(t, backtracks).map {
            case TablableCharDesc(c, _, _, backtracks) => TablableCharDesc(c, None, 0, backtracks)
            case TablablePredDesc(p, _, _, backtracks) => TablablePredDesc(p, None, 0, backtracks)
        }
        case Profile(t)                          => tablable(t, backtracks)
        case TablableErrors(t)                   => tablable(t, backtracks)
        case (_: Pure[_] | _: Get[_]) <*> t      => tablable(t, backtracks)
        case Lift2(_, Line | Col | Offset | _: Get[_], t)    => tablable(t, backtracks)
        case Lift3(_, Line | Col | Offset | _: Get[_], t, _) => tablable(t, backtracks)
        case Lift2(_, t, _)                      => tablable(t, backtracks)
        case Lift3(_, t, _, _)                   => tablable(t, backtracks)
        case t <*> _                             => tablable(t, backtracks)
        case Seq(before, r, _)                   => tablable(before.headOption.getOrElse(r), backtracks)
        case Chainl(_: Pure[_], p, _)            => tablable(p, backtracks)
        case Chainl(init, _, _)                  => tablable(init, backtracks)
        case Chainr(p, _)                        => tablable(p, backtracks)
        case ChainPost(p, _)                     => tablable(p, backtracks)
        case Many(_: Pure[_], p)                 => tablable(p, backtracks)
        case Many(init, _)                       => tablable(init, backtracks)
        case ManyUntil(init, _)                  => tablable(init, backtracks)
        case SepEndBy1(p, _, _)                  => tablable(p, backtracks)
        case Branch(p, _, _)                     => tablable(p, backtracks)
        case _                                   => None
    }

    private def codeGenJumpTable[M[_, +_]: ContOps, R, A](groups: List[JumpTableGroup], needsDefault: Boolean, rest: =>M[R, Unit],
                                                          producesResults: Boolean)
                                                         (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val end = state.freshLabel()
        val default = state.freshLabel()
        val merge = state.getLabel(instructions.MergeErrorsAndFail)
        val (roots, jumpTable, size, expecteds) = foldJumpTableGroups(groups, state)
        instrs += new instructions.JumpTable(jumpTable, default, merge, size, expecteds)
        codeGenRoots(roots, end, producesResults) >> {
            instrs += new instructions.Catch(merge) //This instruction is reachable as default - 1
            instrs += new instructions.Label(default)
            if (needsDefault) {
                instrs += instructions.Empty.zero
                result(instrs += new instructions.Label(end))
            }
            else {
                rest |> {
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
