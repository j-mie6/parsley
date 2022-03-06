package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions
import parsley.internal.errors.{ErrorItem, Raw, Desc}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import StrictParsley.InstrBuffer

// TODO: Tablification is too aggressive. It appears that `optional` is being compiled to jumptable
private [deepembedding] final class <|>[A](var left: StrictParsley[A], var right: StrictParsley[A]) extends StrictParsley[A] {
    def inlinable = false

    override def optimise: StrictParsley[A] = (left, right) match {
        // left catch law: pure x <|> p = pure x
        case (u: Pure[A @unchecked], _) => u
        // alternative law: empty <|> p = p
        case (Empty, v) => v
        // alternative law: p <|> empty = p
        case (u, Empty) => u
        // associative law: (u <|> v) <|> w = u <|> (v <|> w)
        case (u <|> v, w) =>
            left = u
            right = <|>[A](v, w).optimise
            this
        case _ => this
    }
    // TODO: Refactor
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = tablify(this, Nil) match {
        // If the tablified list is single element, that implies that this should be generated as normal!
        case _::Nil => left match {
            case Attempt(u) => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    instrs += new instructions.PushHandlerAndState(handler, true, false)
                    u.codeGen |> {
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.AlwaysRecoverWith[A](x)
                    }
                case v =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    val merge = state.freshLabel()
                    instrs += new instructions.PushHandlerAndState(handler, true, false)
                    u.codeGen >> {
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.JumpGoodAttempt(skip, merge)
                        v.codeGen |> {
                            instrs += new instructions.Label(merge)
                            instrs += instructions.MergeErrors
                            instrs += new instructions.Label(skip)
                        }
                    }
            }
            case u => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.InputCheck(handler, true)
                    u.codeGen |> {
                        instrs += new instructions.JumpGood(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.RecoverWith[A](x)
                        instrs += new instructions.Label(skip)
                    }
                case v =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    val merge = state.freshLabel()
                    instrs += new instructions.InputCheck(handler, true)
                    u.codeGen >> {
                        instrs += new instructions.JumpGood(skip)
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.Catch(merge)
                        v.codeGen |> {
                            instrs += new instructions.Label(merge)
                            instrs += instructions.MergeErrors
                            instrs += new instructions.Label(skip)
                        }
                    }
            }
        }
        // In case of None'd list, the codeGen cont continues by codeGenning that p, else we are done for this tree, call cont!
        case tablified =>
            // This list is backwards :)
            val needsDefault = tablified.head._2.isDefined
            val end = state.freshLabel()
            val default = state.freshLabel()
            val merge = state.freshLabel()
            val (roots, leads, ls, size, expecteds) = foldTablified(tablified, state, mutable.Map.empty, Nil, Nil, 0, mutable.Set.empty)
            //println(leads, tablified)
            instrs += new instructions.JumpTable(leads, ls, default, merge, size, expecteds)
            codeGenRoots(roots, ls, end) >> {
                instrs += new instructions.Catch(merge) //This instruction is reachable as default - 1
                instrs += new instructions.Label(default)
                if (needsDefault) {
                    instrs += instructions.Empty
                    instrs += new instructions.Label(merge)
                    instrs += instructions.MergeErrors
                    result(instrs += new instructions.Label(end))
                }
                else {
                    tablified.head._1.codeGen |> {
                        instrs += new instructions.Label(merge)
                        instrs += instructions.MergeErrors
                        instrs += new instructions.Label(end)
                    }
                }
            }
    }
    def codeGenRoots[Cont[_, +_], R](roots: List[List[StrictParsley[_]]], ls: List[Int], end: Int)
                                 (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = roots match {
        case root::roots_ =>
            instrs += new instructions.Label(ls.head)
            codeGenAlternatives(root) >> {
                instrs += new instructions.JumpGood(end)
                codeGenRoots(roots_, ls.tail, end)
            }
        case Nil => result(())
    }
    def codeGenAlternatives[Cont[_, +_], R](alts: List[StrictParsley[_]])
                                        (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen
        case Attempt(alt)::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            val merge = state.freshLabel()
            instrs += new instructions.PushHandlerAndState(handler, true, false)
            alt.codeGen >> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.JumpGoodAttempt(skip, merge)
                codeGenAlternatives(alts_) |> {
                    instrs += new instructions.Label(merge)
                    instrs += instructions.MergeErrors
                    instrs += new instructions.Label(skip)
                }
            }
        case alt::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            val merge = state.freshLabel()
            instrs += new instructions.InputCheck(handler, true)
            alt.codeGen >> {
                instrs += new instructions.JumpGood(skip)
                instrs += new instructions.Label(handler)
                instrs += new instructions.Catch(merge)
                codeGenAlternatives(alts_) |> {
                    instrs += new instructions.Label(merge)
                    instrs += instructions.MergeErrors
                    instrs += new instructions.Label(skip)
                }
            }
    }
    // TODO: Refactor
    @tailrec def foldTablified(tablified: List[(StrictParsley[_], Option[StrictParsley[_]])], labelGen: CodeGenState,
                               roots: mutable.Map[Char, List[StrictParsley[_]]],
                               leads: List[Char],
                               labels: List[Int],
                               size: Int,
                               expecteds: mutable.Set[ErrorItem]):
        (List[List[StrictParsley[_]]], List[Char], List[Int], Int, Set[ErrorItem]) = tablified match {
        case (_, None)::tablified_ => foldTablified(tablified_, labelGen, roots, leads, labels, size, expecteds)
        case (root, Some(lead))::tablified_ =>
            val (c: Char, expected: ErrorItem, _size: Int) = lead match {
                case ct@CharTok(d) => (d, ct.expected.fold[ErrorItem](Raw(d))(Desc(_)), 1)
                case st@StringTok(s) => (s.head, st.expected.fold[ErrorItem](Raw(s))(Desc(_)), s.size)
                case st@Specific(s) => (s.head, Desc(s), s.size)
                case op@MaxOp(o) => (o.head, Desc(o), o.size)
                case sl: StringLiteral => ('"', Desc("string"), 1)
                case RawStringLiteral => ('"', Desc("string"), 1)
            }
            expecteds += expected
            if (roots.contains(c)) {
                roots(c) = root::roots(c)
                foldTablified(tablified_, labelGen, roots, leads, labelGen.freshLabel() :: labels, Math.max(size, _size), expecteds)
            }
            else {
                roots(c) = root::Nil
                foldTablified(tablified_, labelGen, roots, c::leads, labelGen.freshLabel() :: labels, Math.max(size, _size), expecteds)
            }
        case Nil => (leads.map(roots(_)), leads, labels, size, expecteds.toSet)
    }
    @tailrec private def tablable(p: StrictParsley[_]): Option[StrictParsley[_]] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case t@(_: CharTok | _: StringTok | _: StringLiteral | RawStringLiteral | _: MaxOp) => Some(t)
        // TODO: This can be done for case insensitive things too, but with duplicated branching
        case t: Specific if t.caseSensitive => Some(t)
        case Attempt(t) => tablable(t)
        case (_: Pure[_]) <*> t => tablable(t)
        case Lift2(_, t, _) => tablable(t)
        case Lift3(_, t, _, _) => tablable(t)
        case t <*> _ => tablable(t)
        case t *> _ => tablable(t)
        case t <* _ => tablable(t)
        case _ => None
    }
    @tailrec private [deepembedding] def tablify(p: StrictParsley[_], acc: List[(StrictParsley[_], Option[StrictParsley[_]])]):
        List[(StrictParsley[_], Option[StrictParsley[_]])] = p match {
        case u <|> v =>
            val leading = tablable(u)
            if (leading.isDefined) tablify(v, (u, leading)::acc)
            else (p, None)::acc
        case _ => (p, tablable(p))::acc
    }
}

private [backend] object <|> {
    def apply[A](left: StrictParsley[A], right: StrictParsley[A]): <|>[A] = new <|>(left, right)
    def unapply[A](self: <|>[A]): Some[(StrictParsley[A], StrictParsley[A])] = Some((self.left, self.right))
}