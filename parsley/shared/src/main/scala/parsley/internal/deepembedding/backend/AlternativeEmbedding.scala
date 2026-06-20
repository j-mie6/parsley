/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.XAssert.*

import parsley.internal.collection.mutable.SinglyLinkedList, SinglyLinkedList.LinkedListIterator
import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.frontend.LetMap
import parsley.internal.deepembedding.singletons.*
import parsley.internal.errors.{ExpectDesc, ExpectItem}
import parsley.internal.machine.instructions

// scalastyle:off underscore.import
import Choice.*
import StrictParsley.InstrBuffer
// scalastyle:on underscore.import

// FIXME: It's annoying this doesn't work if the first thing is not tablable: let's make it more fine-grained to create groupings?
private [deepembedding] final class Choice[A] private (private [backend] val alt1: StrictParsley[A],
                                                       private [backend] var alt2: StrictParsley[A],
                                                       private [backend] var alts: SinglyLinkedList[StrictParsley[A]]) extends StrictParsley[A] {
    def this(lalt: StrictParsley[A], ralt: StrictParsley[A]) = this(lalt, ralt, SinglyLinkedList.empty)
    def inlinable: Boolean = false

    override def optimise(implicit lets: LetMap): StrictParsley[A] = {
        // We make the assumption that nodes here are not reoptimised: as such, we can safely
        // assume that it is always in <|> form, with no alts on a choice (as this is the only public constructor)
        if (alts.nonEmpty) throw new IllegalStateException("<|> assumed, but full Choice given") // scalastyle:ignore throw
        if (alt2 eq Empty.Zero) alt1
        else alt1 match {
            case (u: Pure[?]) => u
            case Empty.Zero => alt2
            case FindChoice(ret@Choice(_, _, lalts: SinglyLinkedList[StrictParsley[A]] @unchecked)) => alt2 match {
                case FindChoice(Choice(ralt1, ralt2, ralts: SinglyLinkedList[StrictParsley[A]] @unchecked)) =>
                    assume(!lalts.exists(_.isInstanceOf[Choice[?]]), "ralts can never contain a choice")
                    assume(!ralts.exists(_.isInstanceOf[Choice[?]]), "lalts can never contain a choice")
                    lalts.addOne(ralt1)
                    lalts.addOne(ralt2)
                    lalts.stealAll(ralts)
                    ret
                case p =>
                    assume(!lalts.exists(_.isInstanceOf[Choice[?]]), "lalts can never contain a choice")
                    lalts.addOne(p)
                    ret
            }
            case _ => alt2 match {
                case FindChoice(Choice(ralt1, ralt2, ralts: SinglyLinkedList[StrictParsley[A]] @unchecked)) =>
                    assume(!ralts.exists(_.isInstanceOf[Choice[?]]), "ralts can never contain a choice")
                    this.alt2 = ralt1
                    this.alts = ralts
                    ralts.prependOne(ralt2)
                    this
                case _ => this
            }
        }
    }

    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = codeGenTablified(this.tablify, producesResults)

    private def tablify(implicit state: CodeGenState): List[Either[StrictParsley[?], List[JumpTableGroup]]] =
        tablify((alt1::alt2::alts).iterator, mutable.ListBuffer.empty, mutable.ListBuffer.empty, mutable.ListBuffer.empty, mutable.Set.empty, None)

    @tailrec private def tablify(
        it: LinkedListIterator[StrictParsley[A]],
        acc: mutable.ListBuffer[Either[StrictParsley[?], List[JumpTableGroup]]],
        tableAcc: mutable.ListBuffer[JumpTableGroup],
        groupAcc: mutable.ListBuffer[TablableChar],
        seen: mutable.Set[Char],
        lastSeen: Option[Char],
    )(implicit state: CodeGenState): List[Either[StrictParsley[?], List[JumpTableGroup]]] = if (it.hasNext) {
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
        acc: mutable.ListBuffer[Either[StrictParsley[?], List[JumpTableGroup]]],
        tableAcc: mutable.ListBuffer[JumpTableGroup]
    ): mutable.ListBuffer[Either[StrictParsley[?], List[JumpTableGroup]]] = if (tableAcc.isEmpty) acc else acc += Right(tableAcc.toList)

    // $COVERAGE-OFF$
    final override def pretty: String = (alt1.pretty::alt2.pretty::alts.map(_.pretty).toList).mkString("choice(", ", ", ")")
    // $COVERAGE-ON$
}

private [backend] object Choice {
    def unapply[A](self: Choice[A]): Some[(StrictParsley[A], StrictParsley[A], SinglyLinkedList[StrictParsley[A]])] = Some((self.alt1, self.alt2, self.alts))
    /** Creates a new Choice node. It is the caller's burden to ensure that this node does not have .optimise
      * called on it with non-empty alts, which would break the invariance of Choice.
      */
    def unsafe[A](alt1: StrictParsley[A], alt2: StrictParsley[A], alts: SinglyLinkedList[StrictParsley[A]]) = {
        assume(!alt1.isInstanceOf[Choice[?]] && !alt2.isInstanceOf[Choice[?]], "unsafe Choices should not contain nested Choices")
        new Choice(alt1, alt2, alts)
    }

    sealed trait TablableDesc
    final case class TablableCharDesc(char: Char, expecteds: Iterable[ExpectItem], size: Int, backtracks: Boolean) extends TablableDesc
    final case class TablablePredDesc(pred: Char => Boolean, expecteds: Iterable[ExpectItem], size: Int, backtracks: Boolean) extends TablableDesc
    sealed trait Tablable
    sealed trait JumpTableGroup
    final case class TablableChar(p: StrictParsley[?], desc: TablableCharDesc) extends Tablable
    final case class TablablePred(p: StrictParsley[?], desc: TablablePredDesc) extends Tablable with JumpTableGroup
    final case class TablableChars(ops: List[TablableChar]) extends JumpTableGroup

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
        def unapply(table: Either[StrictParsley[?], List[JumpTableGroup]]): Option[StrictParsley[?]] = table match {
            case Left(p) => Some(p)
            case Right(TablableChars(TablableChar(p, _) :: Nil) :: Nil) => Some(p)
            case Right(TablablePred(p, _) :: Nil) => Some(p)
            case _ => None
        }
    }

    private def codeGenTablified[A, M[_, +_]: ContOps, R]
        (tablified: List[Either[StrictParsley[?], List[JumpTableGroup]]], producesResults: Boolean)
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
                                            corrected: mutable.ListBuffer[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], (Char => Boolean, Int, Iterable[ExpectItem])]]
                                            ): instructions.JumpTablePreds = tables match {
        case Left(map) :: tables_ =>
            propagateExpecteds(tables_, all, corrected += Left(map.map { case (k, (label, errs, backtrack)) => (k, (label, if (backtrack) all else errs)) }))
        case Right((pred, label, expecteds, backtrack)) :: tables_ => propagateExpecteds(tables_, all, corrected += Right((pred, label, if (backtrack) all else expecteds)))
        case Nil => instructions.JumpTablePreds.fromList(corrected.toList)
    }

    private def codeGenRoots[M[_, +_]: ContOps, R](roots: List[(Int, List[StrictParsley[?]])], end: Int, producesResults: Boolean)
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
    private def codeGenAlternatives[M[_, +_]: ContOps, R](alts: List[StrictParsley[?]], producesResults: Boolean)
                                                         (implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen(producesResults)
        case alt::alts_ => codeGenAlt(alt, suspend(codeGenAlternatives[M, R](alts_, producesResults)), producesResults)
    }

    //FIXME: type aliases to aid readability
    @tailrec private def foldTablableChars(tablified: List[TablableChar],
                                           labelGen: CodeGenState,
                                           roots: mutable.Map[Char, (Int, mutable.ListBuffer[StrictParsley[?]])],
                                           map: mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)],
                                           leads: mutable.ListBuffer[Char],
                                           size: Int,
                                           expecteds: List[ExpectItem]):
        (List[(Int, List[StrictParsley[?]])], mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)], Int, List[ExpectItem]) // Roots, map, size, expecteds
            = tablified match {
                case TablableChar(root, TablableCharDesc(c, expected, _size, backtracks)) :: tablified_ =>
                    if (roots.contains(c)) {
                        roots(c)._2 += root
                        val (l, errs, b) = map(c)
                        map(c) = (l, errs, b && backtracks)
                        foldTablableChars(tablified_, labelGen, roots, map, leads, size.max(_size), expecteds ++ expected)
                    } else {
                        val label = labelGen.freshLabel()
                        roots(c) = (label, mutable.ListBuffer(root))
                        // the ++ is nasty, but we need to make sure it's not mutated to take the snapshot here
                        // this is plausibly less frequent, so perhaps a trick with copy-on-write could be played?
                        map(c) = (label, expecteds, backtracks)
                        foldTablableChars(tablified_, labelGen, roots, map, leads += c, size.max(_size), expecteds ++ expected)
                    }
                case Nil => (leads.toList.map { ls => val (l, ps) = roots(ls); (l, ps.toList) }, map, size, expecteds)
            }

    private def foldJumpTableGroups(groups: List[JumpTableGroup], labelGen: CodeGenState):
        (List[(Int, List[StrictParsley[?]])], instructions.JumpTablePreds, Int, List[ExpectItem]) =
            foldJumpTableGroups(groups, labelGen, mutable.ListBuffer.empty, mutable.ListBuffer.empty, 0, List.empty)

    @tailrec private def foldJumpTableGroups(groups: List[JumpTableGroup],
                                             labelGen: CodeGenState,
                                             rootsAcc: mutable.ListBuffer[(Int, List[StrictParsley[?]])],
                                             tableAcc: mutable.ListBuffer[Either[mutable.Map[Char, (Int, Iterable[ExpectItem], Boolean)], (Char => Boolean, Int, Iterable[ExpectItem], Boolean)]],
                                             size: Int,
                                             allExpecteds: List[ExpectItem]):
        (List[(Int, List[StrictParsley[?]])], instructions.JumpTablePreds, Int, List[ExpectItem]) = groups match {
            case TablableChars(ops) :: def_ =>
                val (roots, map, size_, allExpecteds_) = foldTablableChars(ops, labelGen, mutable.Map.empty, mutable.Map.empty, mutable.ListBuffer.empty, size, allExpecteds)
                foldJumpTableGroups(def_, labelGen, rootsAcc ++= roots, tableAcc += Left(map), size_, allExpecteds_)
            case TablablePred(p, TablablePredDesc(pred, expecteds, size_, backtracks)) :: defs_ =>
                val label = labelGen.freshLabel()
                foldJumpTableGroups(defs_, labelGen, rootsAcc += ((label, List(p))), tableAcc += Right((pred, label, expecteds, backtracks)), Math.max(size, size_), allExpecteds ++ expecteds)
            case Nil => (rootsAcc.toList, propagateExpecteds(tableAcc.toList, allExpecteds, mutable.ListBuffer.empty), size, allExpecteds)
        }

    private def tablable(p: StrictParsley[?], backtracks: Boolean)(implicit state: CodeGenState): Option[TablableDesc] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case ct@CharTok(c, _)                    => Some(TablableCharDesc(c, ct.expected.asExpectItems(c), 1, backtracks))
        case ct@SupplementaryCharTok(c, _)       => Some(TablableCharDesc(Character.highSurrogate(c), ct.expected.asExpectItems(Character.toChars(c).mkString), 1, backtracks))
        case st@StringTok(s, _)                  => Some(TablableCharDesc(s.head, st.expected.asExpectItems(s), s.codePointCount(0, s.length), backtracks))
        //case op@MaxOp(o)                         => Some((o.head, Some(Desc(o)), o.size, backtracks))
        //case _: StringLiteral | RawStringLiteral => Some(('"', Some(Desc("string")), 1, backtracks))
        // TODO: This can be done for case insensitive things too, but with duplicated branching
        case t@token.SoftKeyword(s) if t.caseSensitive => Some(TablableCharDesc(s.head, t.expected.asExpectDescs(s), s.codePointCount(0, s.length), backtracks = true))
        case t@token.SoftOperator(s)             => Some(TablableCharDesc(s.head, t.expected.asExpectDescs(s), s.codePointCount(0, s.length), backtracks = true))
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
        case NonConsuming() <*> t                => tablable(t, backtracks)
        case Lift2(_, NonConsuming(), t)         => tablable(t, backtracks)
        case Lift3(_, NonConsuming(), t, _)      => tablable(t, backtracks)
        case Lift2(_, t, _)                      => tablable(t, backtracks)
        case Lift3(_, t, _, _)                   => tablable(t, backtracks)
        case t <*> _                             => tablable(t, backtracks)
        case Seq(before, r, _)                   => tablable(before.headOption.getOrElse(r), backtracks)
        case Chainl(NonConsuming(), p, _)        => tablable(p, backtracks)
        case Chainl(init, _, _)                  => tablable(init, backtracks)
        case Chainr(p, _)                        => tablable(p, backtracks)
        case ChainPost(p, _)                     => tablable(p, backtracks)
        case Many(NonConsuming(), p)             => tablable(p, backtracks)
        case Many(init, _)                       => tablable(init, backtracks)
        case ManyUntil(init, _)                  => tablable(init, backtracks)
        case SepEndBy1(p, _, _)                  => tablable(p, backtracks)
        case Branch(p, _, _)                     => tablable(p, backtracks)
        case sub: Let[?]                         => tablable(state.getBody(sub), backtracks)
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

    private object NonConsuming {
        def unapply(p: StrictParsley[?])(implicit state: CodeGenState): Boolean = p match {
            case Line | Col | Offset | _: Get[?] | _: Pure[?] => true
            case sub: Let[?] => NonConsuming.unapply(state.getBody(sub))
            case Lift2(_, NonConsuming(), NonConsuming()) => true
            case Lift3(_, NonConsuming(), NonConsuming(), NonConsuming()) => true
            case _ => false
        }
    }

    private object FindChoice {
        def unapply[A](p: StrictParsley[A])(implicit lets: LetMap): Option[Choice[A]] = {
            @tailrec
            def go(p: StrictParsley[A], requiresCopy: Boolean = false): Option[Choice[A]] =
                p match {
                    case Choice(alt1, alt2, alts: SinglyLinkedList[StrictParsley[A]] @unchecked) =>
                        Some(new Choice(alt1, alt2, if (requiresCopy) alts.copy else alts))
                    case sub: Let[?] =>
                        val body = lets.findBody(sub)
                        if (body.isDefined) {
                            go(body.get.asInstanceOf[StrictParsley[A]], requiresCopy = true)
                        } else {
                            // Recursion point
                            None
                        }
                    case _ => None
                }

            go(p)
        }
    }
}


private [deepembedding] object <|> {
    def apply[A](left: StrictParsley[A], right: StrictParsley[A]): Choice[A] = new Choice(left, right)
}
