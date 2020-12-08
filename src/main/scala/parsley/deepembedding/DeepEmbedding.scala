package parsley.deepembedding

import parsley.deepembedding.ContOps._
import parsley.deepembedding.DeepToken._
import parsley.instructions
import parsley.{UnsafeOption, Var, Breakpoint, EntryBreak, FullBreak, ExitBreak}
import parsley.deepembedding.{Parsley, SubMap, CodeGenState, LetFinderState}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

private [parsley] object DeepEmbedding
{
    // Core Embedding
    private [parsley] final class Pure[A](private [Pure] val x: A) extends Parsley[A]
    {
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Push(x))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"pure($x)")
    }
    private [parsley] final class <*>[A, B](_pf: =>Parsley[A => B], _px: =>Parsley[A]) extends Parsley[B]
    {
        private [<*>] var pf: Parsley[A => B] = _
        private [<*>] var px: Parsley[A] = _
        override def preprocess[Cont[_, _], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
            if (label == null && processed) result(this) else for (pf <- this.pf.optimised; px <- this.px.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.pf = pf
                    this.px = px
                    this.size = pf.size + px.size + 1
                    this
                }
                else <*>(pf, px)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            pf = _pf
            px = _px
            pf.findLets >> px.findLets
        }
        override def optimise: Parsley[B] = (pf, px) match
        {
            // Fusion laws
            case (uf, Pure(x)) if uf.isInstanceOf[Pure[_]] || uf.isInstanceOf[_ <*> _] && uf.safe => uf match
            {
                // first position fusion
                case Pure(f) => new Pure(f(x))
                // second position fusion
                case Pure(f: (T => A => B) @unchecked) <*> (uy: Parsley[T]) =>
                    pf = new Pure((y: T) => f(y)(x))
                    px = uy.asInstanceOf[Parsley[A]]
                    this
                // third position fusion
                case Pure(f: (T => U => A => B) @unchecked) <*> (uy: Parsley[T]) <*> (uz: Parsley[U]) =>
                    pf = <*>(new Pure((y: T) => (z: U) => f(y)(z)(x)), uy)
                    px = uz.asInstanceOf[Parsley[A]]
                    this
                // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
                case _ =>
                    pf = new Pure((f: A => B) => f(x)).asInstanceOf[Parsley[A => B]]
                    px = uf.asInstanceOf[Parsley[A]]
                    this
            }
            // functor law: fmap f (fmap g p) == fmap (f . g) p where fmap f p = pure f <*> p from applicative
            case (Pure(f), Pure(g: (T => A) @unchecked) <*> (u: Parsley[T])) => <*>(new Pure(f.compose(g)), u)
            // TODO: functor law with lift2!
            // right absorption law: mzero <*> p = mzero
            case (z: MZero, _) => z
            /* RE-ASSOCIATION LAWS */
            // re-association law 1: (q *> pf) <*> px = q *> (pf <*> px)
            case (q *> uf, ux) => *>(q, <*>(uf, ux).optimise)
            case (uf, cont: Cont[_, _]) => cont match
            {
                // re-association law 2: pf <*> (px <* q) = (pf <*> px) <* q
                case ux <* v => <*(<*>(uf, ux).optimise, v).optimise
                // re-association law 3: p *> pure x = pure x <* p
                // consequence of re-association law 3: pf <*> (q *> pure x) = (pf <*> pure x) <* q
                case v *> (ux: Pure[_]) => <*(<*>(uf, ux).optimise, v).optimise
                case _ => this
            }
            // consequence of left zero law and monadic definition of <*>, preserving error properties of pf
            case (u, z: MZero) => *>(u, z)
            // interchange law: u <*> pure y == pure ($y) <*> u == ($y) <$> u (single instruction, so we benefit at code-gen)
            case (uf, Pure(x)) =>
                pf = new Pure((f: A => B) => f(x)).asInstanceOf[Parsley[A => B]]
                px = uf.asInstanceOf[Parsley[A]]
                this
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = pf match
        {
            // pure f <*> p = f <$> p
            case Pure(f) => px match
            {
                case ct@CharTok(c) => result(instrs += instructions.CharTokFastPerform[Char, B](c, f.asInstanceOf[Char => B], ct.expected))
                case st@StringTok(s) => result(instrs += new instructions.StringTokFastPerform(s, f.asInstanceOf[String => B], st.expected))
                case _ =>
                    px.codeGen |>
                    (instrs += new instructions.Perform(f))
            }
            case _ =>
                pf.codeGen >>
                px.codeGen |>
                (instrs += instructions.Apply)
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- pf.prettyASTAux; r <- px.prettyASTAux) yield s"($l <*> $r)"
    }
    private [parsley] final class <|>[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[B]
    {
        private [<|>] var p: Parsley[A] = _
        private [<|>] var q: Parsley[B] = _
        override def preprocess[Cont[_, _], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; q <- this.q.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 3
                    this
                }
                else <|>(p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            q = _q
            p.findLets >> q.findLets
        }
        override def optimise: Parsley[B] = (p, q) match
        {
            // left catch law: pure x <|> p = pure x
            case (u: Pure[B @unchecked], _) => u
            // alternative law: empty <|> p = p
            case (e: Empty, v) if e.expected == null => v
            // alternative law: p <|> empty = p
            case (u: Parsley[B @unchecked], e: Empty) if e.expected == null => u
            // associative law: (u <|> v) <|> w = u <|> (v <|> w)
            case ((u: Parsley[T]) <|> (v: Parsley[A]), w) =>
                p = u.asInstanceOf[Parsley[A]]
                q = <|>[A, B](v, w).optimise
                this
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = tablify(this, Nil) match
        {
            // If the tablified list is single element, that implies that this should be generated as normal!
            case _::Nil => p match
            {
                case Attempt(u) => q match
                {
                    case Pure(x) =>
                        val handler = state.freshLabel()
                        instrs += new instructions.PushHandler(handler)
                        u.codeGen |>
                        {
                            instrs += new instructions.Label(handler)
                            instrs += new instructions.AlwaysRecoverWith[B](x)
                        }
                    case v =>
                        val handler = state.freshLabel()
                        val skip = state.freshLabel()
                        instrs += new instructions.PushHandler(handler)
                        u.codeGen >>
                        {
                            instrs += new instructions.Label(handler)
                            instrs += new instructions.JumpGoodAttempt(skip)
                            v.codeGen |>
                            (instrs += new instructions.Label(skip))
                        }
                }
                case u => q match
                {
                    case Pure(x) =>
                        val handler = state.freshLabel()
                        val skip = state.freshLabel()
                        instrs += new instructions.InputCheck(handler)
                        u.codeGen |>
                        {
                            instrs += new instructions.JumpGood(skip)
                            instrs += new instructions.Label(handler)
                            instrs += new instructions.RecoverWith[B](x)
                            instrs += new instructions.Label(skip)
                        }
                    case v =>
                        val handler = state.freshLabel()
                        val skip = state.freshLabel()
                        instrs += new instructions.InputCheck(handler)
                        u.codeGen >>
                        {
                            instrs += new instructions.JumpGood(skip)
                            instrs += new instructions.Label(handler)
                            instrs += instructions.Catch
                            v.codeGen |>
                            (instrs += new instructions.Label(skip))
                        }
                }

            }
            // In case of None'd list, the codeGen cont continues by codeGenning that p, else we are done for this tree, call cont!
            case tablified =>
                // This list is backwards :)
                val needsDefault = tablified.head._2.isDefined
                val end = state.freshLabel()
                val default = state.freshLabel()
                val (roots, leads, ls, expecteds) = foldTablified(tablified, state, mutable.Map.empty, Nil, Nil, Nil)
                instrs += new instructions.JumpTable(leads, ls, default, expecteds)
                codeGenRoots(roots, ls, end) >>
                {
                    instrs += instructions.Catch //This instruction is reachable as default - 1
                    instrs += new instructions.Label(default)
                    if (needsDefault)
                    {
                        instrs += new instructions.Empty(null)
                        result(instrs += new instructions.Label(end))
                    }
                    else
                    {
                        tablified.head._1.codeGen |>
                        (instrs += new instructions.Label(end))
                    }
                }
        }
        def codeGenRoots[Cont[_, _]](roots: List[List[Parsley[_]]], ls: List[Int], end: Int)(implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = roots match
        {
            case root::roots_ =>
                instrs += new instructions.Label(ls.head)
                codeGenAlternatives(root) >>
                {
                    instrs += new instructions.JumpGood(end)
                    codeGenRoots(roots_, ls.tail, end)
                }
            case Nil => result(())
        }
        def codeGenAlternatives[Cont[_, _]](alts: List[Parsley[_]])(implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = (alts: @unchecked) match
        {
            case alt::Nil => alt.codeGen
            case Attempt(alt)::alts_ =>
                val handler = state.freshLabel()
                val skip = state.freshLabel()
                instrs += new instructions.PushHandler(handler)
                alt.codeGen >>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.JumpGoodAttempt(skip)
                    ops.|>(codeGenAlternatives(alts_), instrs += new instructions.Label(skip))
                }
            case alt::alts_ =>
                val handler = state.freshLabel()
                val skip = state.freshLabel()
                instrs += new instructions.InputCheck(handler)
                alt.codeGen >>
                {
                    instrs += new instructions.JumpGood(skip)
                    instrs += new instructions.Label(handler)
                    instrs += instructions.Catch
                    ops.|>(codeGenAlternatives(alts_), instrs += new instructions.Label(skip))
                }
        }
        @tailrec def foldTablified(tablified: List[(Parsley[_], Option[Parsley[_]])], labelGen: CodeGenState,
                                   roots: mutable.Map[Char, List[Parsley[_]]],
                                   leads: List[Char],
                                   labels: List[Int],
                                   expecteds: List[UnsafeOption[String]]):
            (List[List[Parsley[_]]], List[Char], List[Int], List[UnsafeOption[String]]) = tablified match
        {
            case (_, None)::tablified_ => foldTablified(tablified_, labelGen, roots, leads, labels, expecteds)
            case (root, Some(lead))::tablified_ =>
                val (c, expected) = lead match
                {
                    case ct@CharTok(d) => (d, ct.expected)
                    case st@StringTok(s) => (s.head, if (st.expected == null) "\"" + s + "\"" else st.expected)
                    case kw@Keyword(k) => (k.head, if (kw.expected == null) k else kw.expected)
                    case op@Operator(o) => (o.head, if (op.expected == null) o else op.expected)
                    case op@MaxOp(o) => (o.head, if (op.expected == null) o else op.expected)
                    case sl: StringLiteral => ('"', if (sl.expected == null) "string" else sl.expected)
                    case rs: RawStringLiteral => ('"', if (rs.expected == null) "string" else rs.expected)
                }
                if (roots.contains(c))
                {
                    roots.update(c, root::roots(c))
                    foldTablified(tablified_, labelGen, roots, leads, labelGen.freshLabel() :: labels, expected :: expecteds)
                }
                else
                {
                    roots.update(c, root::Nil)
                    foldTablified(tablified_, labelGen, roots, c::leads, labelGen.freshLabel() :: labels, expected :: expecteds)
                }
            case Nil => (leads.map(roots(_)), leads, labels, expecteds)
        }
        @tailrec private def tablable(p: Parsley[_]): Option[Parsley[_]] = p match
        {
            // CODO: Numeric parsers by leading digit (This one would require changing the foldTablified function a bit)
            case t@(_: CharTok | _: StringTok | _: Keyword | _: StringLiteral | _: RawStringLiteral | _: Operator | _: MaxOp) => Some(t)
            case Attempt(t) => tablable(t)
            case (_: Pure[_]) <*> t => tablable(t)
            case Lift2(_, t, _) => tablable(t)
            case Lift3(_, t, _, _) => tablable(t)
            case t <*> _ => tablable(t)
            case t *> _ => tablable(t)
            case t <* _ => tablable(t)
            case _ => None
        }
        @tailrec private [DeepEmbedding] def tablify(p: Parsley[_], acc: List[(Parsley[_], Option[Parsley[_]])]): List[(Parsley[_], Option[Parsley[_]])] = p match
        {
            case u <|> v =>
                val leading = tablable(u)
                if (leading.isDefined) tablify(v, (u, leading)::acc)
                else (p, None)::acc
            case _ => (p, tablable(p))::acc
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- q.prettyASTAux) yield s"($l <|> $r)"
    }
    private [parsley] final class >>=[A, B](_p: =>Parsley[A], private [>>=] var f: A => Parsley[B], val expected: UnsafeOption[String] = null) extends Parsley[B]
    {
        private [>>=] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else >>=(p, f, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise: Parsley[B] = p match
        {
            // CODO: We need to try and identify the Recs in the optimised binds, so we can remove the call instructions
            // monad law 1: pure x >>= f = f x
            //case Pure(x) if safe => new Rec(() => f(x), expected)
            // char/string x = char/string x *> pure x and monad law 1
            //case p@CharTok(c) => *>(p, new Rec(() => f(c.asInstanceOf[A]), expected))
            //case p@StringTok(s) => *>(p, new Rec(() => f(s.asInstanceOf[A]), expected))
            // (q *> p) >>= f = q *> (p >>= f)
            case u *> v => *>(u, >>=(v, f, expected).optimise)
            // monad law 3: (m >>= g) >>= f = m >>= (\x -> g x >>= f) Note: this *could* help if g x ended with a pure, since this would be optimised out!
            //case (m: Parsley[T] @unchecked) >>= (g: (T => A) @unchecked) =>
            //    p = m.asInstanceOf[Parsley[A]]
            //    f = (x: T) => >>=(g(x), f, expected).optimise
            //    this
            // monadplus law (left zero)
            case z: MZero => z
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.DynCall[A](x => f(x).instrs, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux) yield s"($l >>= ?)"
    }
    private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: UnsafeOption[String] = null) extends Parsley[Char]
    {
        override def preprocess[Cont[_, _], C >: Char](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[C]] =
        {
            if (label == null) result(this)
            else result(new Satisfy(f, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Satisfies(f, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"satisfy(?)")
    }
    private [parsley] abstract class Cont[A, +B] extends Parsley[B]
    {
        def result: Parsley[B]
        def discard: Parsley[A]
        def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): Cont[A, B_]
    }
    private [parsley] final class *>[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Cont[A, B]
    {
        private [*>] var p: Parsley[A] = _
        private [*>] var q: Parsley[B] = _
        override def preprocess[Cont[_, _], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
            if (label == null && processed) ops.wrap(this) else for (p <- this.p.optimised; q <- this.q.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 1
                    this
                }
                else *>(p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            q = _q
            p.findLets >> q.findLets
        }
        @tailrec override def optimise: Parsley[B] = p match
        {
            // pure _ *> p = p
            case _: Pure[_] => q
            // p *> pure _ *> q = p *> q
            case u *> (_: Pure[_]) =>
                p = u.asInstanceOf[Parsley[A]]
                optimise
            case ct1@CharTok(c) if q.isInstanceOf[CharTok] || q.isInstanceOf[StringTok] => q match
            {
                // char(c) *> char(d) = string(cd) *> pure(d)
                case ct2@CharTok(d) =>
                    p = new StringTok(c.toString + d, if (ct1.expected != null) ct1.expected else if (ct2.expected != null) ct2.expected else null).asInstanceOf[Parsley[A]]
                    q = new Pure(d).asInstanceOf[Parsley[B]]
                    optimise
                // char(c) *> string(s) = string(cs) *> pure(s)
                case st1@StringTok(s) =>
                    p = new StringTok(c.toString + s, if (ct1.expected != null) ct1.expected else if (st1.expected != null) st1.expected else null).asInstanceOf[Parsley[A]]
                    q = new Pure(s).asInstanceOf[Parsley[B]]
                    optimise
            }
            case st1@StringTok(s) if q.isInstanceOf[CharTok] || q.isInstanceOf[StringTok] => q match
            {
                // string(s) *> char(c) = string(sc) *> pure(c)
                case ct2@CharTok(c) =>
                    p = new StringTok(s + c, if (st1.expected != null) st1.expected else if (ct2.expected != null) ct2.expected else null).asInstanceOf[Parsley[A]]
                    q = new Pure(c).asInstanceOf[Parsley[B]]
                    optimise
                // string(s) *> string(t) = string(st) *> pure(t)
                case st2@StringTok(t) =>
                    p = new StringTok(s + t, if (st1.expected != null) st1.expected else if (st2.expected != null) st2.expected else null).asInstanceOf[Parsley[A]]
                    q = new Pure(t).asInstanceOf[Parsley[B]]
                    optimise
            }
            // mzero *> p = mzero (left zero and definition of *> in terms of >>=)
            case z: MZero => z
            case u => q match
            {
                // re-association - normal form of Then chain is to have result at the top of tree
                case v *> w =>
                    p = *>(u, v).asInstanceOf[Parsley[A]].optimiseDefinitelyNotTailRec
                    q = w
                    optimise
                case _ => this
            }
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = q match
        {
            case Pure(x) => p match
            {
                case ct@CharTok(c) => ops.wrap(instrs += instructions.CharTokFastPerform[Char, B](c, _ => x, ct.expected))
                case st@StringTok(s) => ops.wrap(instrs += new instructions.StringTokFastPerform(s, _ => x, st.expected))
                case st@Satisfy(f) => ops.wrap(instrs += new instructions.SatisfyExchange(f, x, st.expected))
                case u =>
                    u.codeGen |>
                    (instrs += new instructions.Exchange(x))
            }
            case v =>
                p.codeGen >>
                {
                    instrs += instructions.Pop
                    v.codeGen
                }
        }
        override def discard: Parsley[A] = p
        override def result: Parsley[B] = q
        override def copy[B_ >: B](prev: Parsley[A], next: Parsley[B_]): A *> B_ = *>(prev, next)
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- q.prettyASTAux) yield s"($l *> $r)"
    }
    private [parsley] final class <*[A, B](_p: =>Parsley[A], _q: =>Parsley[B]) extends Cont[B, A]
    {
        private [<*] var p: Parsley[A] = _
        private [<*] var q: Parsley[B] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) ops.wrap(this) else for (p <- this.p.optimised; q <- this.q.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 1
                    this
                }
                else <*(p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            q = _q
            p.findLets >> q.findLets
        }
        @tailrec override def optimise: Parsley[A] = q match
        {
            // p <* pure _ = p
            case _: Pure[_] => p
            // p <* (q *> pure _) = p <* q
            case v *> (_: Pure[_]) =>
                q = v.asInstanceOf[Parsley[B]]
                optimise
            case ct1@CharTok(d) if p.isInstanceOf[CharTok] || p.isInstanceOf[StringTok] => p match
            {
                // char(c) <* char(d) = string(cd) *> pure(c)
                case ct2@CharTok(c) => *>(new StringTok(c.toString + d, if (ct1.expected != null) ct1.expected else if (ct2.expected != null) ct2.expected else null), new Pure(c)).asInstanceOf[Parsley[A]]
                // string(s) <* char(d) = string(sd) *> pure(s)
                case st1@StringTok(s) => *>(new StringTok(s + d, if (ct1.expected != null) ct1.expected else if (st1.expected != null) st1.expected else null), new Pure(s)).asInstanceOf[Parsley[A]]
            }
            case st1@StringTok(t) if p.isInstanceOf[CharTok] || p.isInstanceOf[StringTok]  => p match
            {
                // char(c) <* string(t) = string(ct) *> pure(c)
                case ct1@CharTok(c) => *>(new StringTok(c.toString + t, if (st1.expected != null) st1.expected else if (ct1.expected != null) ct1.expected else null), new Pure(c)).asInstanceOf[Parsley[A]]
                // string(s) <* string(t) = string(st) *> pure(s)
                case st2@StringTok(s) => *>(new StringTok(s + t, if (st1.expected != null) st1.expected else if (st2.expected != null) st2.expected else null), new Pure(s)).asInstanceOf[Parsley[A]]
            }
            // p <* mzero = p *> mzero (by preservation of error messages and failure properties) - This moves the pop instruction after the failure
            case z: MZero => *>(p, z)
            case w => p match
            {
                // re-association law 3: pure x <* p = p *> pure x
                case u: Pure[_] => *>(w, u).optimise
                // mzero <* p = mzero (left zero law and definition of <* in terms of >>=)
                case z: MZero => z
                // re-association - normal form of Prev chain is to have result at the top of tree
                case u <* v =>
                    p = u
                    q = <*(v, w).asInstanceOf[Parsley[B]].optimiseDefinitelyNotTailRec
                    optimise
                case _ => this
            }
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = (p, q) match
        {
            case (Pure(x), ct@CharTok(c)) => ops.wrap(instrs += instructions.CharTokFastPerform[Char, A](c, _ => x, ct.expected))
            case (Pure(x), st@StringTok(s)) => ops.wrap(instrs += new instructions.StringTokFastPerform(s, _ => x, st.expected))
            case (Pure(x), st@Satisfy(f)) => ops.wrap(instrs += new instructions.SatisfyExchange(f, x, st.expected))
            case (Pure(x), v) =>
                v.codeGen |>
                (instrs += new instructions.Exchange(x))
            case _=>
                p.codeGen >>
                q.codeGen |>
                (instrs += instructions.Pop)
        }
        override def discard: Parsley[B] = q
        override def result: Parsley[A] = p
        override def copy[A_ >: A](prev: Parsley[B], next: Parsley[A_]): <*[A_, B] = <*(next, prev)
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- q.prettyASTAux) yield s"($l <* $r)"
    }
    private [parsley] final class Attempt[A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Attempt] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else Attempt(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += instructions.Attempt
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"attempt($c)"
    }
    private [parsley] final class Look[A](_p: =>Parsley[A]) extends Parsley[A]
    {
        private [Look] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 3
                    this
                }
                else Look(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += instructions.Look
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"lookAhead($c)"
    }
    private [parsley] sealed trait MZero extends Parsley[Nothing]
    private [parsley] class Empty(val expected: UnsafeOption[String] = null) extends MZero
    {
        override def preprocess[Cont[_, _], N >: Nothing](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[N]] =
        {
            if (label == null) result(this)
            else result(new Empty(label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Empty(expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("empty")
    }
    private [parsley] final class Fail(private [Fail] val msg: String, val expected: UnsafeOption[String] = null) extends MZero
    {
        override def preprocess[Cont[_, _], N >: Nothing](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[N]] =
        {
            if (label == null) result(this)
            else result(new Fail(msg, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Fail(msg, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"fail($msg)")
    }
    private [parsley] final class Unexpected(private [Unexpected] val msg: String, val expected: UnsafeOption[String] = null) extends MZero
    {
        override def preprocess[Cont[_, _], N >: Nothing](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[N]] =
        {
            if (label == null) result(this)
            else result(new Unexpected(msg, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Unexpected(msg, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"unexpected($msg)")
    }
    private [parsley] final class Rec[A](val p: Parsley[A], val expected: UnsafeOption[String] = null) extends Parsley[A]
    {
        //private [Rec] lazy val p = _p()
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
        {
            if (label == null) result(this)
            else result(new Rec(p, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Call(p.instrs, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"rec $p")
    }
    private [parsley] final class Subroutine[A](_p: =>Parsley[A], val expected: UnsafeOption[String] = null) extends Parsley[A]
    {
        private [Subroutine] var p: Parsley[A] = _

        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
        {
            if (!processed) for (p <- this.p.optimised(seen, sub, null, ops)) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this
                }
                else Subroutine(p, label)
            }
            else if (label == null) result(this)
            else result(Subroutine(p, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = if (p.size <= 1) p else this // This threshold might need tuning?
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val label = state.getSubLabel(p)
            result(instrs += new instructions.GoSub(label, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"+$c"
    }
    // Intrinsic Embedding
    private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: UnsafeOption[String] = null) extends Parsley[Char]
    {
        override def preprocess[Cont[_, _], C >: Char](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[C]] =
        {
            if (label == null) result(this)
            else result(new CharTok(c, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += instructions.CharTok(c, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"char($c)")
    }
    private [parsley] final class StringTok(private [StringTok] val s: String, val expected: UnsafeOption[String] = null) extends Parsley[String]
    {
        override def preprocess[Cont[_, _], S >: String](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S]] =
        {
            if (label == null) result(this)
            else result(new StringTok(s, label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def optimise = s match
        {
            case "" => new Pure("")
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.StringTok(s, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"string($s)")
    }
    // TODO: Perform applicative fusion optimisations
    private [parsley] final class Lift2[A, B, +C](private [Lift2] val f: (A, B) => C, _p: =>Parsley[A], _q: =>Parsley[B]) extends Parsley[C]
    {
        private [Lift2] var p: Parsley[A] = _
        private [Lift2] var q: Parsley[B] = _
        override def preprocess[Cont[_, _], C_ >: C](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[C_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; q <- this.q.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 1
                    this
                }
                else Lift2(f, p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            q = _q
            p.findLets >> q.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen >>
            q.codeGen |>
            (instrs += new instructions.Lift2(f))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- q.prettyASTAux) yield s"lift2(f, $l, $r)"
    }
    private [parsley] final class Lift3[A, B, C, +D](private [Lift3] val f: (A, B, C) => D, _p: =>Parsley[A], _q: =>Parsley[B], _r: =>Parsley[C]) extends Parsley[D]
    {
        private [Lift3] var p: Parsley[A] = _
        private [Lift3] var q: Parsley[B] = _
        private [Lift3] var r: Parsley[C] = _
        override def preprocess[Cont[_, _], D_ >: D](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[D_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; q <- this.q.optimised; r <- this.r.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.q = q
                    this.r = r
                    this.size = p.size + q.size + r.size + 1
                    this
                }
                else Lift3(f, p, q, r)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            q = _q
            r = _r
            p.findLets >> q.findLets >> r.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen >>
            q.codeGen >>
            r.codeGen |>
            (instrs += new instructions.Lift3(f))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (f <- p.prettyASTAux; s <- q.prettyASTAux; t <- r.prettyASTAux) yield s"lift3(f, $f, $s, $t)"
    }
    private [parsley] final class FastFail[A](_p: =>Parsley[A], private [FastFail] val msggen: A => String, val expected: UnsafeOption[String] = null) extends MZero
    {
        private [FastFail] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], N >: Nothing](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[N]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else FastFail(p, msggen, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case Pure(x) => new Fail(msggen(x))
            case z: MZero => z
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.FastFail(msggen, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"fastfail($c)"
    }
    private [parsley] final class FastUnexpected[A](_p: =>Parsley[A], private [FastUnexpected] val msggen: A => String, val expected: UnsafeOption[String] = null) extends MZero
    {
        private [FastUnexpected] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], N >: Nothing](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[N]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else FastUnexpected(p, msggen, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case Pure(x) => new Unexpected(msggen(x))
            case z: MZero => z
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.FastUnexpected(msggen, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"fastunexpected($c)"
    }
    private [parsley] final class Ensure[A](_p: =>Parsley[A], private [Ensure] val pred: A => Boolean, val expected: UnsafeOption[String] = null) extends Parsley[A]
    {
        private [Ensure] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else Ensure(p, pred, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case px@Pure(x) => if (pred(x)) px else new Empty
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.Ensure(pred, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"ensure($c, ?)"
    }
    private [parsley] final class Guard[A](_p: =>Parsley[A], private [Guard] val pred: A => Boolean, private [Guard] val msg: String, val expected: UnsafeOption[String] = null) extends Parsley[A]
    {
        private [Guard] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else Guard(p, pred, msg, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case px@Pure(x) => if (pred(x)) px else new Fail(msg)
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.Guard(pred, msg, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"guard($c)"
    }
    private [parsley] final class FastGuard[A](_p: =>Parsley[A], private [FastGuard] val pred: A => Boolean, private [FastGuard] val msggen: A => String, val expected: UnsafeOption[String] = null) extends Parsley[A]
    {
        private [FastGuard] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else FastGuard(p, pred, msggen, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case px@Pure(x) => if (pred(x)) px else new Fail(msggen(x))
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.FastGuard(pred, msggen, expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"guard($c)"
    }
    private [parsley] final class Many[A](_p: =>Parsley[A]) extends Parsley[List[A]]
    {
        private [Many] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], L >: List[A]](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[L]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else Many(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case _: Pure[A @unchecked] => throw new Exception("many given parser which consumes no input")
            case _: MZero => new Pure(Nil)
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Many(body)
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"many($c)"
    }
    private [parsley] final class SkipMany[A](_p: =>Parsley[A]) extends Parsley[Unit]
    {
        private [SkipMany] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else SkipMany(p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def optimise = p match
        {
            case _: Pure[A @unchecked] => throw new Exception("skipMany given parser which consumes no input")
            case _: MZero => new Pure(()).asInstanceOf[Parsley[Nothing]]
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SkipMany(body)
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"skipMany($c)"
    }
    private [parsley] final class ChainPost[A](_p: =>Parsley[A], _op: =>Parsley[A => A]) extends Parsley[A]
    {
        private [ChainPost] var p: Parsley[A] = _
        private [ChainPost] var op: Parsley[A => A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; op <- this.op.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.op = op
                    this.size = p.size + op.size + 2
                    this
                }
                else ChainPost(p, op)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            op = _op
            p.findLets >> op.findLets
        }
        override def optimise = op match
        {
            case _: Pure[(A => A) @unchecked] => throw new Exception("left chain given parser which consumes no input")
            case _: MZero => p
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                instrs += new instructions.Label(body)
                op.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.ChainPost(body)
                }
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- op.prettyASTAux) yield s"chainPost($l, $r)"
    }
    private [parsley] final class ChainPre[A](_p: =>Parsley[A], _op: =>Parsley[A => A]) extends Parsley[A]
    {
        private [ChainPre] var p: Parsley[A] = _
        private [ChainPre] var op: Parsley[A => A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; op <- this.op.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.op = op
                    this.size = p.size + op.size + 2
                    this
                }
                else ChainPre(p, op)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            op = _op
            p.findLets >> op.findLets
        }
        override def optimise = op match
        {
            case _: Pure[(A => A) @unchecked] => throw new Exception("right chain given parser which consumes no input")
            case _: MZero => p
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            op.codeGen >>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPre(body)
                p.codeGen |>
                (instrs += instructions.Apply)
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- op.prettyASTAux; r <- p.prettyASTAux) yield s"chainPre($l, $r)"
    }
    private [parsley] final class Chainl[A, B](_p: =>Parsley[A], _op: =>Parsley[(B, A) => B], private [Chainl] val wrap: A => B) extends Parsley[B]
    {
        private [Chainl] var p: Parsley[A] = _
        private [Chainl] var op: Parsley[(B, A) => B] = _
        override def preprocess[Cont[_, _], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; op <- this.op.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.op = op
                    this.size = p.size*2 + op.size + 2
                    this
                }
                else Chainl(p, op, wrap)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            op = _op
            p.findLets >> op.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                instrs += new instructions.Label(body)
                op.codeGen >>
                p.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.Chainl(body, wrap)
                }
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- op.prettyASTAux) yield s"chainl1($l, $r)"
    }
    private [parsley] final class Chainr[A, B](_p: =>Parsley[A], _op: =>Parsley[(A, B) => B], private [Chainr] val wrap: A => B) extends Parsley[B]
    {
        private [Chainr] var p: Parsley[A] = _
        private [Chainr] var op: Parsley[(A, B) => B] = _
        override def preprocess[Cont[_, _], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; op <- this.op.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.op = op
                    this.size = p.size + op.size + 3
                    this
                }
                else Chainr(p, op, wrap)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            op = _op
            p.findLets >> op.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit]=
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                op.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.Chainr(body, wrap)
                }
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- op.prettyASTAux) yield s"chainr1($l, $r)"
    }
    private [parsley] final class SepEndBy1[A, B](_p: =>Parsley[A], _sep: =>Parsley[B]) extends Parsley[List[A]]
    {
        private [SepEndBy1] var p: Parsley[A] = _
        private [SepEndBy1] var sep: Parsley[B] = _
        override def preprocess[Cont[_, _], L >: List[A]](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[L]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; sep <- this.sep.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.sep = sep
                    this.size = p.size + sep.size + 3
                    this
                }
                else SepEndBy1(p, sep)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            sep = _sep
            p.findLets >> sep.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val body = state.freshLabel()
            val handler = state.freshLabel()
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            p.codeGen >>
            {
                instrs += new instructions.InputCheck(handler)
                sep.codeGen |>
                {
                    instrs += new instructions.Label(handler)
                    instrs += new instructions.SepEndBy1(body)
                }
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- sep.prettyASTAux) yield s"sepEndBy1($l, $r)"
    }
    private [parsley] final class ManyUntil[A](_body: Parsley[Any]) extends Parsley[List[A]]
    {
        private [ManyUntil] var body: Parsley[Any] = _
        override def preprocess[Cont[_, _], L >: List[A]](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[L]] =
            if (label == null && processed) result(this) else for (body <- this.body.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.body = body
                    this.size = body.size + 2
                    this
                }
                else ManyUntil(body)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            body = _body
            body.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val start = state.freshLabel()
            val loop = state.freshLabel()
            instrs += new instructions.PushFallthrough(loop)
            instrs += new instructions.Label(start)
            body.codeGen |>
            {
                instrs += new instructions.Label(loop)
                instrs += new instructions.ManyUntil(start)
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- body.prettyASTAux) yield s"manyUntil($c)"
    }
    private [parsley] final class Ternary[A](_b: =>Parsley[Boolean], _p: =>Parsley[A], _q: =>Parsley[A]) extends Parsley[A]
    {
        private [Ternary] var b: Parsley[Boolean] = _
        private [Ternary] var p: Parsley[A] = _
        private [Ternary] var q: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (b <- this.b.optimised; p <- this.p.optimised; q <- this.q.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.b = b
                    this.p = p
                    this.q = q
                    this.size = b.size + p.size + q.size + 2
                    this
                }
                else Ternary(b, p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            b = _b
            p = _p
            q = _q
            b.findLets >> p.findLets >> q.findLets
        }
        override def optimise = b match
        {
            case Pure(true) => p
            case Pure(false) => q
            case _ => this
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val success = state.freshLabel()
            val end = state.freshLabel()
            b.codeGen >>
            {
                instrs += new instructions.If(success)
                q.codeGen >>
                {
                    instrs += new instructions.Jump(end)
                    instrs += new instructions.Label(success)
                    p.codeGen |>
                    (instrs += new instructions.Label(end))
                }
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (f <- b.prettyASTAux; s <- p.prettyASTAux; t <- q.prettyASTAux) yield s"($f ? $s : $t)"
    }
    private [parsley] final class NotFollowedBy[A](_p: =>Parsley[A], val expected: UnsafeOption[String] = null) extends Parsley[Unit]
    {
        private [NotFollowedBy] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else NotFollowedBy(p, label)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.PushHandler(handler)
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.NotFollowedBy(expected)
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"notFollowedBy($c)"
    }
    private [parsley] final class Eof(val expected: UnsafeOption[String] = null) extends Parsley[Unit]
    {
        override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
        {
            if (label == null) result(this)
            else result(new Eof(label))
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Eof(expected))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("eof")
    }
    private [parsley] object Line extends Parsley[Int]
    {
        override def preprocess[Cont[_, _], I >: Int](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[I]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += instructions.Line)
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("line")
    }
    private [parsley] object Col extends Parsley[Int]
    {
        override def preprocess[Cont[_, _], I >: Int](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[I]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += instructions.Col)
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("col")
    }
    private [parsley] final class Get[S](v: Var) extends Parsley[S]
    {
        override def preprocess[Cont[_, _], S_ >: S](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S_]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Get(v.v))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"get($v)")
    }
    private [parsley] final class Put[S](private [Put] val v: Var, _p: =>Parsley[S]) extends Parsley[Unit]
    {
        private [Put] var p: Parsley[S] = _
        override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 1
                    this
                }
                else Put(v, p)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen |>
            (instrs += new instructions.Put(v.v))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"put($v, $c)"
    }
    private [parsley] final class Modify[S](v: Var, f: S => S) extends Parsley[Unit]
    {
        override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] = result(this)
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            result(instrs += new instructions.Modify(v.v, f))
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"modify($v, ?)")
    }
    private [parsley] final class Local[S, A](private [Local] val v: Var, _p: =>Parsley[S], _q: =>Parsley[A]) extends Parsley[A]
    {
        private [Local] var p: Parsley[S] = _
        private [Local] var q: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised; q <- this.q.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.q = q
                    this.size = p.size + q.size + 2
                    this
                }
                else Local(v, p, q)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            q = _q
            p.findLets >> q.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            p.codeGen >>
            {
                instrs += new instructions.LocalEntry(v.v)
                q.codeGen |>
                (instrs += new instructions.LocalExit(v.v))
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] =
            for (l <- p.prettyASTAux; r <- q.prettyASTAux) yield s"local($v, $l, $r)"
    }
    private [parsley] final class ErrorRelabel[+A](_p: =>Parsley[A], msg: String) extends Parsley[A]
    {
        lazy val p = _p
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
        {
            if (label == null) p.optimised(seen, sub, msg, ops)
            else p.optimised
        }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = p.findLets
        override def optimise = throw new Exception("Error relabelling should not be in optimisation!")
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = throw new Exception("Error relabelling should not be in code gen!")
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield s"($c ? $msg)"
    }
    private [parsley] final class Debug[A](_p: =>Parsley[A], name: String, break: Breakpoint) extends Parsley[A]
    {
        private [Debug] var p: Parsley[A] = _
        override def preprocess[Cont[_, _], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] =
            if (label == null && processed) result(this) else for (p <- this.p.optimised) yield
            {
                if (label == null)
                {
                    processed = true
                    this.p = p
                    this.size = p.size + 2
                    this
                }
                else Debug(p, name, break)
            }
        override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            processed = false
            p = _p
            p.findLets
        }
        override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
        {
            val handler = state.freshLabel()
            instrs += new instructions.LogBegin(handler, name, (break eq EntryBreak) || (break eq FullBreak))
            p.codeGen |>
            {
                instrs += new instructions.Label(handler)
                instrs += new instructions.LogEnd(name, (break eq ExitBreak) || (break eq FullBreak))
            }
        }
        override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = p.prettyASTAux
    }

    private [DeepEmbedding] object Pure
    {
        def unapply[A](self: Pure[A]): Option[A] = Some(self.x)
    }
    private [DeepEmbedding] object <*>
    {
        def apply[A, B](pf: Parsley[A=>B], px: Parsley[A]): <*>[A, B] =
        {
            val res: A <*> B = new <*>(null, null)
            res.processed = true
            res.pf = pf
            res.px = px
            res.size = pf.size + px.size + 1
            res
        }
        def unapply[A, B](self: <*>[A, B]): Option[(Parsley[A=>B], Parsley[A])] = Some((self.pf, self.px))
    }
    private [DeepEmbedding] object <|>
    {
        def apply[A, B](p: Parsley[A], q: Parsley[B]): A <|> B =
        {
            val res: A <|> B = new <|>(null, null)
            res.processed = true
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[A, B](self: A <|> B): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q))
    }
    private [DeepEmbedding] object >>=
    {
        def apply[A, B](p: Parsley[A], f: A => Parsley[B], expected: UnsafeOption[String]): >>=[A, B] =
        {
            val res: A >>= B = new >>=(null, f, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 3
            res
        }
        def unapply[A, B](self: >>=[A, B]): Option[(Parsley[A], A => Parsley[B])] = Some((self.p, self.f))
    }
    private [DeepEmbedding] object Cont
    {
        def unapply[A, B](self: Cont[A, B]): Option[(Parsley[A], Parsley[B])] = Some((self.discard, self.result))
    }
    private [DeepEmbedding] object *>
    {
        def apply[A, B](p: Parsley[A], q: Parsley[B]): A *> B =
        {
            val res: A *> B = new *>(null, null)
            res.processed = true
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[A, B](self: A *> B): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q))
    }
    private [DeepEmbedding] object <*
    {
        def apply[A, B](p: Parsley[A], q: Parsley[B]): A <* B =
        {
            val res: A <* B = new <*(null, null)
            res.processed = true
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[A, B](self: A <* B): Option[(Parsley[A], Parsley[B])] = Some((self.p, self.q))
    }
    private [DeepEmbedding] object Attempt
    {
        def apply[A](p: Parsley[A]): Attempt[A] =
        {
            val res: Attempt[A] = new Attempt(null)
            res.processed = true
            res.p = p
            res.size = p.size + 2
            res
        }
        def unapply[A](self: Attempt[A]): Option[Parsley[A]] = Some(self.p)
    }
    private [DeepEmbedding] object Look
    {
        def apply[A](p: Parsley[A]): Look[A] =
        {
            val res: Look[A] = new Look(null)
            res.processed = true
            res.p = p
            res.size = p.size + 3
            res
        }
    }
    private [parsley] object Subroutine
    {
        def apply[A](p: Parsley[A], expected: UnsafeOption[String]): Subroutine[A] =
        {
            val res: Subroutine[A] = new Subroutine(null, expected)
            res.processed = true
            res.p = p
            res
        }
        def unapply[A](self: Subroutine[A]): Option[Parsley[A]] = Some(self.p)
    }
    private [DeepEmbedding] object CharTok
    {
        def unapply(self: CharTok): Option[Char] = Some(self.c)
    }
    private [DeepEmbedding] object StringTok
    {
        def unapply(self: StringTok): Option[String] = Some(self.s)
    }
    private [DeepEmbedding] object Satisfy
    {
        def unapply(self: Satisfy): Option[Char => Boolean] = Some(self.f)
    }
    private [DeepEmbedding] object Lift2
    {
        def apply[A, B, C](f: (A, B) => C, p: Parsley[A], q: Parsley[B]): Lift2[A, B, C] =
        {
            val res: Lift2[A, B, C] = new Lift2(f, null, null)
            res.processed = true
            res.p = p
            res.q = q
            res.size = p.size + q.size + 1
            res
        }
        def unapply[A, B, C](self: Lift2[A, B, C]): Option[((A, B) => C, Parsley[A], Parsley[B])] = Some((self.f, self.p, self.q))
    }
    private [DeepEmbedding] object Lift3
    {
        def apply[A, B, C, D](f: (A, B, C) => D, p: Parsley[A], q: Parsley[B], r: Parsley[C]): Lift3[A, B, C, D] =
        {
            val res: Lift3[A, B, C, D] = new Lift3(f, null, null, null)
            res.processed = true
            res.p = p
            res.q = q
            res.r = r
            res.size = p.size + q.size + r.size + 1
            res
        }
        def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Option[((A, B, C) => D, Parsley[A], Parsley[B], Parsley[C])] = Some((self.f, self.p, self.q, self.r))
    }
    private [DeepEmbedding] object FastFail
    {
        def apply[A](p: Parsley[A], msggen: A => String, expected: UnsafeOption[String]): FastFail[A] =
        {
            val res: FastFail[A] = new FastFail(null, msggen, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object FastUnexpected
    {
        def apply[A](p: Parsley[A], msggen: A => String, expected: UnsafeOption[String]): FastUnexpected[A] =
        {
            val res: FastUnexpected[A] = new FastUnexpected(null, msggen, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Ensure
    {
        def apply[A](p: Parsley[A], pred: A => Boolean, expected: UnsafeOption[String]): Ensure[A] =
        {
            val res: Ensure[A] = new Ensure(null, pred, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Guard
    {
        def apply[A](p: Parsley[A], pred: A => Boolean, msg: String, expected: UnsafeOption[String]): Guard[A] =
        {
            val res: Guard[A] = new Guard(null, pred, msg, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object FastGuard
    {
        def apply[A](p: Parsley[A], pred: A => Boolean, msggen: A => String, expected: UnsafeOption[String]): FastGuard[A] =
        {
            val res: FastGuard[A] = new FastGuard(null, pred, msggen, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Many
    {
        def apply[A](p: Parsley[A]): Many[A] =
        {
            val res: Many[A] = new Many(null)
            res.processed = true
            res.p = p
            res.size = p.size + 2
            res
        }
    }
    private [DeepEmbedding] object SkipMany
    {
        def apply[A](p: Parsley[A]): SkipMany[A] =
        {
            val res: SkipMany[A] = new SkipMany(null)
            res.processed = true
            res.p = p
            res.size = p.size + 2
            res
        }
    }
    private [DeepEmbedding] object ChainPost
    {
        def apply[A](p: Parsley[A], op: Parsley[A => A]): ChainPost[A] =
        {
            val res: ChainPost[A] = new ChainPost(null, null)
            res.processed = true
            res.p = p
            res.op = op
            res.size = p.size + op.size + 2
            res
        }
    }
    private [DeepEmbedding] object ChainPre
    {
        def apply[A](p: Parsley[A], op: Parsley[A => A]): ChainPre[A] =
        {
            val res: ChainPre[A] = new ChainPre(null, null)
            res.processed = true
            res.p = p
            res.op = op
            res.size = p.size + op.size + 2
            res
        }
    }
    private [DeepEmbedding] object Chainl
    {
        def apply[A, B](p: Parsley[A], op: Parsley[(B, A) => B], wrap: A => B): Chainl[A, B] =
        {
            val res: Chainl[A, B] = new Chainl(null, null, wrap)
            res.processed = true
            res.p = p
            res.op = op
            res.size = p.size*2 + op.size + 2
            res
        }
    }
    private [DeepEmbedding] object Chainr
    {
        def apply[A, B](p: Parsley[A], op: Parsley[(A, B) => B], wrap: A => B): Chainr[A, B] =
        {
            val res: Chainr[A, B] = new Chainr(null, null, wrap)
            res.processed = true
            res.p = p
            res.op = op
            res.size = p.size + op.size + 3
            res
        }
    }
    private [DeepEmbedding] object SepEndBy1
    {
        def apply[A, B](p: Parsley[A], sep: Parsley[B]): SepEndBy1[A, B] =
        {
            val res: SepEndBy1[A, B] = new SepEndBy1(null, null)
            res.processed = true
            res.p = p
            res.sep = sep
            res.size = p.size + sep.size + 3
            res
        }
    }
    private [parsley] object ManyUntil
    {
        object Stop
        def apply[A](body: Parsley[Any]): ManyUntil[A] =
        {
            val res: ManyUntil[A] = new ManyUntil(null)
            res.processed = true
            res.body = body
            res.size = body.size + 2
            res
        }
    }
    private [DeepEmbedding] object Ternary
    {
        def apply[A](b: Parsley[Boolean], p: Parsley[A], q: Parsley[A]): Ternary[A] =
        {
            val res: Ternary[A] = new Ternary(null, null, null)
            res.processed = true
            res.b = b
            res.p = p
            res.q = q
            res.size = b.size + p.size + q.size + 1
            res
        }
    }
    private [DeepEmbedding] object NotFollowedBy
    {
        def apply[A](p: Parsley[A], expected: UnsafeOption[String]): NotFollowedBy[A] =
        {
            val res: NotFollowedBy[A] = new NotFollowedBy(null, expected)
            res.processed = true
            res.p = p
            res.size = p.size + 2
            res
        }
    }
    private [DeepEmbedding] object Put
    {
        def apply[S](v: Var, p: Parsley[S]): Put[S] =
        {
            val res: Put[S] = new Put(v, null)
            res.processed = true
            res.p = p
            res.size = p.size + 1
            res
        }
    }
    private [DeepEmbedding] object Local
    {
        def apply[S, A](v: Var, p: Parsley[S], q: Parsley[A]): Local[S, A] =
        {
            val res: Local[S, A] = new Local[S, A](v, null, null)
            res.processed = true
            res.p = p
            res.q = q
            res.size = p.size + q.size + 2
            res
        }
    }
    private [DeepEmbedding] object Debug
    {
        def apply[A](p: Parsley[A], name: String, break: Breakpoint): Debug[A] =
        {
            val res: Debug[A] = new Debug(null, name, break)
            res.processed = true
            res.p = p
            res.size = p.size + 2
            res
        }
    }
}