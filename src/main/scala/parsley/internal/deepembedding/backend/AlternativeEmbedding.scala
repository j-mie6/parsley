/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.errors.{Desc, ErrorItem, Raw}
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer
import scala.collection.immutable

// TODO: Tablification is too aggressive. It appears that `optional` is being compiled to jumptable
private [deepembedding] final class <|>[A](var left: StrictParsley[A], var right: StrictParsley[A]) extends StrictParsley[A] {
    def inlinable: Boolean = false

    override def optimise: StrictParsley[A] = (left, right) match {
        // left catch law: pure x <|> p = pure x
        case (u: Pure[A @unchecked], _) => u
        // alternative law: empty <|> p = p
        case (Empty, v)                 => v
        // alternative law: p <|> empty = p
        case (u, Empty)                 => u
        // associative law: (u <|> v) <|> w = u <|> (v <|> w)
        case (u <|> v, w)               =>
            left = u
            right = <|>[A](v, w).optimise
            this
        case _                          => this
    }
    // TODO: Refactor
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = tablify(this, mutable.ListBuffer.empty) match {
        // If the tablified list is single element, that implies that this should be generated as normal!
        case (_ :: Nil) | (_ :: (_, None) :: Nil) => left match {
            case Attempt(u) => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.PushHandlerAndState(handler, saveHints = true, hideHints = false)
                    suspend(u.codeGen[Cont, R]) |> {
                        instrs += new instructions.JumpAndPopState(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.AlwaysRecoverWith[A](x)
                        instrs += new instructions.Label(skip)
                    }
                case v       =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    val merge = state.getLabel(instructions.MergeErrorsAndFail)
                    instrs += new instructions.PushHandlerAndState(handler, saveHints = true, hideHints = false)
                    suspend(u.codeGen[Cont, R]) >> {
                        instrs += new instructions.JumpAndPopState(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.RestoreAndPushHandler(merge)
                        suspend(v.codeGen[Cont, R]) |> {
                            instrs += instructions.ErrorToHints //TODO: If we know v must consume input to succeed then this should be PopError!
                            instrs += new instructions.Label(skip)
                        }
                    }
            }
            case u          => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.PushHandlerAndCheck(handler, saveHints = true)
                    suspend(u.codeGen[Cont, R]) |> {
                        instrs += new instructions.JumpAndPopCheck(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.RecoverWith[A](x)
                        instrs += new instructions.Label(skip)
                    }
                case v       =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    val merge = state.getLabel(instructions.MergeErrorsAndFail)
                    instrs += new instructions.PushHandlerAndCheck(handler, saveHints = true)
                    suspend(u.codeGen[Cont, R]) >> {
                        instrs += new instructions.JumpAndPopCheck(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.Catch(merge)
                        suspend(v.codeGen[Cont, R]) |> {
                            instrs += instructions.ErrorToHints //TODO: If we know v must consume input to succeed then this should be PopError!
                            instrs += new instructions.Label(skip)
                        }
                    }
            }
        }
        // In case of None'd list, the codeGen cont continues by codeGenning that p, else we are done for this tree, call cont!
        case tablified =>
            val needsDefault = tablified.last._2.isDefined
            val end = state.freshLabel()
            val default = state.freshLabel()
            val merge = state.getLabel(instructions.MergeErrorsAndFail)
            val (tablified_, backtracks) = tablified.view.collect {
                case (root, Some((leading, backtrack))) => ((root, leading), backtrack)
            }.unzip
            val (roots, leads, ls, size, expecteds, expectedss) =
                foldTablified(tablified_.toList, state, mutable.Map.empty, mutable.ListBuffer.empty, mutable.ListBuffer.empty, 0, Set.empty, mutable.ListBuffer.empty)
            // The expectedss need to be adjusted for every backtracking parser
            val expectedss_ = propagateExpecteds(expectedss.zip(backtracks.toList).reverse, expecteds, Nil)
            instrs += new instructions.JumpTable(leads, ls, default, merge, size, expecteds, expectedss_)
            codeGenRoots(roots, ls, end) >> {
                instrs += new instructions.Catch(merge) //This instruction is reachable as default - 1
                instrs += new instructions.Label(default)
                if (needsDefault) {
                    instrs += instructions.Empty
                    result(instrs += new instructions.Label(end))
                }
                else {
                    tablified.last._1.codeGen |> {
                        instrs += instructions.ErrorToHints //TODO: If we know tablified.head._1 must consume input to succeed then this should be PopError!
                        instrs += new instructions.Label(end)
                    }
                }
            }
    }

    @tailrec def propagateExpecteds(expectedss: List[(Set[ErrorItem], Boolean)], previous: Set[ErrorItem], corrected: List[Set[ErrorItem]]):
        List[Set[ErrorItem]] = expectedss match {
        case (expecteds, backtrack) :: expectedss =>
            if (backtrack) propagateExpecteds(expectedss, previous, previous :: corrected)
            else           propagateExpecteds(expectedss, expecteds, expecteds :: corrected)
        case Nil => corrected
    }

    def codeGenRoots[Cont[_, +_], R](roots: List[List[StrictParsley[_]]], ls: List[Int], end: Int)
                                 (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = roots match {
        case root::roots_ =>
            instrs += new instructions.Label(ls.head)
            codeGenAlternatives(root) >> {
                instrs += instructions.ErrorToHints //TODO: If we know root must consume input to succeed then this should be PopError!
                instrs += new instructions.JumpAndPopCheck(end)
                suspend(codeGenRoots[Cont, R](roots_, ls.tail, end))
            }
        case Nil => result(())
    }
    def codeGenAlternatives[Cont[_, +_], R](alts: List[StrictParsley[_]])
                                        (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen
        case Attempt(alt)::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            val merge = state.getLabel(instructions.MergeErrorsAndFail)
            instrs += new instructions.PushHandlerAndState(handler, saveHints = true, hideHints = false)
            suspend(alt.codeGen[Cont, R]) >> {
                instrs += new instructions.JumpAndPopState(skip)
                instrs += new instructions.Label(handler)
                instrs += new instructions.RestoreAndPushHandler(merge)
                suspend(codeGenAlternatives[Cont, R](alts_)) |> {
                    instrs += instructions.ErrorToHints
                    instrs += new instructions.Label(skip)
                }
            }
        case alt::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            val merge = state.getLabel(instructions.MergeErrorsAndFail)
            instrs += new instructions.PushHandlerAndCheck(handler, saveHints = true)
            suspend(alt.codeGen[Cont, R]) >> {
                instrs += new instructions.JumpAndPopCheck(skip)
                instrs += new instructions.Label(handler)
                instrs += new instructions.Catch(merge)
                suspend(codeGenAlternatives[Cont, R](alts_)) |> {
                    instrs += instructions.ErrorToHints
                    instrs += new instructions.Label(skip)
                }
            }
    }
    // TODO: Refactor
    @tailrec def foldTablified(tablified: List[(StrictParsley[_], StrictParsley[_])], labelGen: CodeGenState,
                               roots: mutable.Map[Char, mutable.ListBuffer[StrictParsley[_]]],
                               leads: mutable.ListBuffer[Char],
                               labels: mutable.ListBuffer[Int],
                               size: Int,
                               expecteds: Set[ErrorItem],
                               expectedss: mutable.ListBuffer[Set[ErrorItem]]):
        (List[List[StrictParsley[_]]], List[Char], List[Int], Int, Set[ErrorItem], List[Set[ErrorItem]]) = tablified match {
        case (root, lead)::tablified_ =>
            val (c: Char, expected: ErrorItem, _size: Int) = lead match {
                case ct@CharTok(d) => (d, ct.expected.fold[ErrorItem](Raw(d))(Desc(_)), 1)
                case st@StringTok(s) => (s.head, st.expected.fold[ErrorItem](Raw(s))(Desc(_)), s.size)
                case st@Specific(s) => (s.head, Desc(s), s.size)
                case op@MaxOp(o) => (o.head, Desc(o), o.size)
                case sl: StringLiteral => ('"', Desc("string"), 1)
                case RawStringLiteral => ('"', Desc("string"), 1)
            }
            if (roots.contains(c)) {
                roots(c) += root
                foldTablified(tablified_, labelGen, roots, leads, labels, Math.max(size, _size), expecteds + expected, expectedss)
            }
            else {
                roots(c) = mutable.ListBuffer(root)
                foldTablified(tablified_, labelGen, roots, leads += c, labels += labelGen.freshLabel(), Math.max(size, _size), expecteds + expected, expectedss += expecteds)
            }
        case Nil => (leads.toList.map(roots(_).toList), leads.toList, labels.toList, size, expecteds, expectedss.toList)
    }
    @tailrec private def tablable(p: StrictParsley[_], backtracks: Boolean): Option[(StrictParsley[_], Boolean)] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case t@(_: CharTok | _: StringTok
              | _: StringLiteral
              | RawStringLiteral | _: MaxOp) => Some((t, backtracks))
        // TODO: This can be done for case insensitive things too, but with duplicated branching
        case t: Specific if t.caseSensitive   => Some((t, backtracks))
        case Attempt(t)                       => tablable(t, backtracks = true)
        case (_: Pure[_]) <*> t               => tablable(t, backtracks)
        case Lift2(_, t, _)                   => tablable(t, backtracks)
        case Lift3(_, t, _, _)                => tablable(t, backtracks)
        case t <*> _                          => tablable(t, backtracks)
        case t *> _                           => tablable(t, backtracks)
        case t <* _                           => tablable(t, backtracks)
        case _                                => None
    }
    @tailrec private [deepembedding] def tablify(p: StrictParsley[_], acc: mutable.ListBuffer[(StrictParsley[_], Option[(StrictParsley[_], Boolean)])]):
        List[(StrictParsley[_], Option[(StrictParsley[_], Boolean)])] = p match {
        case u <|> v =>
            val leading = tablable(u, false)
            if (leading.isDefined) tablify(v, acc += ((u, leading)))
            else (acc += ((p, None))).toList
        case _       => (acc += ((p, tablable(p, false)))).toList
    }
}

private [backend] object <|> {
    def apply[A](left: StrictParsley[A], right: StrictParsley[A]): <|>[A] = new <|>(left, right)
    def unapply[A](self: <|>[A]): Some[(StrictParsley[A], StrictParsley[A])] = Some((self.left, self.right))
}
