package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}, instructions.{ErrorItem, Raw, Desc}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [parsley] final class <|>[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Binary[A, B, B](_p, _q)((l, r) => s"($l <|> $r)", <|>.empty) {
    override val numInstrs = 3

    override def optimise: Parsley[B] = (left, right) match {
        // left catch law: pure x <|> p = pure x
        case (u: Pure[B @unchecked], _) => u
        // alternative law: empty <|> p = p
        case (e: Empty, v) if e.expected == null => v
        // alternative law: p <|> empty = p
        case (u: Parsley[B @unchecked], e: Empty) if e.expected == null => u
        // associative law: (u <|> v) <|> w = u <|> (v <|> w)
        case ((u: Parsley[T]) <|> (v: Parsley[A]), w) =>
            left = u.asInstanceOf[Parsley[A]]
            right = <|>[A, B](v, w).optimise
            this
        case _ => this
    }
    // TODO: Refactor
    override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = tablify(this, Nil) match {
        // If the tablified list is single element, that implies that this should be generated as normal!
        case _::Nil => left match {
            case Attempt(u) => right match {
                case Pure(x) =>
                    val handler = state.freshLabel()
                    instrs += new instructions.PushHandlerAndState(handler, true)
                    u.codeGen |> {
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.AlwaysRecoverWith[B](x)
                    }
                case v =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.PushHandlerAndState(handler, true)
                    u.codeGen >> {
                        instrs += new instructions.Label(handler)
                        instrs += new instructions.JumpGoodAttempt(skip)
                        val merge = state.freshLabel()
                        instrs += new instructions.PushHandler(merge)
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
                        instrs += new instructions.RecoverWith[B](x)
                        instrs += new instructions.Label(skip)
                    }
                case v =>
                    val handler = state.freshLabel()
                    val skip = state.freshLabel()
                    instrs += new instructions.InputCheck(handler, true)
                    u.codeGen >> {
                        instrs += new instructions.JumpGood(skip)
                        instrs += new instructions.Label(handler)
                        instrs += instructions.Catch
                        val merge = state.freshLabel()
                        instrs += new instructions.PushHandler(merge)
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
            val (roots, leads, ls, expecteds) = foldTablified(tablified, state, mutable.Map.empty, Nil, Nil, mutable.Map.empty)
            instrs += new instructions.JumpTable(leads, ls, default, expecteds)
            codeGenRoots(roots, ls, end) >> {
                instrs += instructions.Catch //This instruction is reachable as default - 1
                instrs += new instructions.Label(default)
                val merge = state.freshLabel()
                instrs += new instructions.PushHandler(merge)
                if (needsDefault) {
                    instrs += new instructions.Empty(null)
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
    def codeGenRoots[Cont[_, +_]: ContOps](roots: List[List[Parsley[_]]], ls: List[Int], end: Int)
                                 (implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = roots match {
        case root::roots_ =>
            instrs += new instructions.Label(ls.head)
            codeGenAlternatives(root) >> {
                instrs += new instructions.JumpGood(end)
                codeGenRoots(roots_, ls.tail, end)
            }
        case Nil => result(())
    }
    def codeGenAlternatives[Cont[_, +_]: ContOps](alts: List[Parsley[_]])
                                        (implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = (alts: @unchecked) match {
        case alt::Nil => alt.codeGen
        case Attempt(alt)::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            instrs += new instructions.PushHandlerAndState(handler, true)
            alt.codeGen >> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.JumpGoodAttempt(skip)
                val merge = state.freshLabel()
                instrs += new instructions.PushHandler(merge)
                codeGenAlternatives(alts_) |> {
                    instrs += new instructions.Label(merge)
                    instrs += instructions.MergeErrors
                    instrs += new instructions.Label(skip)
                }
            }
        case alt::alts_ =>
            val handler = state.freshLabel()
            val skip = state.freshLabel()
            instrs += new instructions.InputCheck(handler, true)
            alt.codeGen >> {
                instrs += new instructions.JumpGood(skip)
                instrs += new instructions.Label(handler)
                instrs += instructions.Catch
                val merge = state.freshLabel()
                instrs += new instructions.PushHandler(merge)
                codeGenAlternatives(alts_) |> {
                    instrs += new instructions.Label(merge)
                    instrs += instructions.MergeErrors
                    instrs += new instructions.Label(skip)
                }
            }
    }
    // TODO: Refactor
    @tailrec def foldTablified(tablified: List[(Parsley[_], Option[Parsley[_]])], labelGen: CodeGenState,
                               roots: mutable.Map[Char, List[Parsley[_]]],
                               leads: List[Char],
                               labels: List[Int],
                               expecteds: mutable.Map[Char, Set[ErrorItem]]):
        (List[List[Parsley[_]]], List[Char], List[Int], Map[Char, Set[ErrorItem]]) = tablified match {
        case (_, None)::tablified_ => foldTablified(tablified_, labelGen, roots, leads, labels, expecteds)
        case (root, Some(lead))::tablified_ =>
            val (c: Char, expected: ErrorItem) = lead match {
                case ct@CharTok(d) => (d, if (ct.expected == null) Raw(d) else Desc(ct.expected))
                case st@StringTok(s) => (s.head, if (st.expected == null) Raw(s) else Desc(st.expected))
                case st@Specific(s) => (s.head, Desc(if (st.expected == null) s else st.expected))
                case op@MaxOp(o) => (o.head, Desc(if (op.expected == null) o else op.expected))
                case sl: StringLiteral => ('"', Desc(if (sl.expected == null) "string" else sl.expected))
                case rs: RawStringLiteral => ('"', Desc(if (rs.expected == null) "string" else rs.expected))
            }
            if (roots.contains(c)) {
                roots(c) = root::roots(c)
                expecteds(c) = expecteds(c) + expected
                foldTablified(tablified_, labelGen, roots, leads, labelGen.freshLabel() :: labels, expecteds)
            }
            else {
                roots(c) = root::Nil
                expecteds(c) = Set(expected)
                foldTablified(tablified_, labelGen, roots, c::leads, labelGen.freshLabel() :: labels, expecteds)
            }
        case Nil => (leads.map(roots(_)), leads, labels, expecteds.toMap)
    }
    @tailrec private def tablable(p: Parsley[_]): Option[Parsley[_]] = p match {
        // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
        case t@(_: CharTok | _: StringTok | _: Specific | _: StringLiteral | _: RawStringLiteral | _: MaxOp) => Some(t)
        case Attempt(t) => tablable(t)
        case (_: Pure[_]) <*> t => tablable(t)
        case Lift2(_, t, _) => tablable(t)
        case Lift3(_, t, _, _) => tablable(t)
        case t <*> _ => tablable(t)
        case t *> _ => tablable(t)
        case t <* _ => tablable(t)
        case _ => None
    }
    @tailrec private [deepembedding] def tablify(p: Parsley[_], acc: List[(Parsley[_], Option[Parsley[_]])]): List[(Parsley[_], Option[Parsley[_]])] = p match {
        case u <|> v =>
            val leading = tablable(u)
            if (leading.isDefined) tablify(v, (u, leading)::acc)
            else (p, None)::acc
        case _ => (p, tablable(p))::acc
    }
}

private [parsley] class Empty(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Nothing]("empty", new Empty(_), new instructions.Empty(expected)) with MZero

private [deepembedding] object <|> {
    def empty[A, B]: A <|> B = new <|>(null, null)
    def apply[A, B](left: Parsley[A], right: Parsley[B]): A <|> B = empty.ready(left, right)
    def unapply[A, B](self: A <|> B): Option[(Parsley[A], Parsley[B])] = Some((self.left, self.right))
}