package parsley

import java.io.IOException

import parsley.ExpressionParser._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

private [parsley] object ParsleyBench
{
    import parsley.Parsley._
    import parsley.Combinator._
    import parsley.Char._
    val liftab: Parsley[String] = lift2[Char, Char, String]((x, y) => x.toString + y.toString, 'a', 'b')
    println(liftab.pretty)
    val aconsb: Parsley[List[Char]] = 'a' <::> ('b' #> Nil)
    println(aconsb.pretty)
    val athenb: Parsley[String] = 'a' *> 'b' #> "ab"
    println(athenb.pretty)
    val manya: Parsley[List[Char]] = many('a') <* 'b'
    println(manya.pretty)
    def chain: Parsley[Int] = chainl1('1' <#> (_.toInt), '+' #> ((x: Int, y: Int) => x + y))
    println(chain.pretty)
    
    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp
    
    // This is an optimisation for the logic inside. Since this is the last in a chain of ors
    // it doesn't need to account for the other symbols (just needs to not accidentally consume ])
    private val whitespaceBF = satisfy(_ != ']')
    
    def brainfuck: Parsley[List[BrainFuckOp]] = 
    {
        lazy val bf: Parsley[List[BrainFuckOp]] = 
            many('>' #> Some(RightPointer)
             <|> '<' #> Some(LeftPointer)
             <|> '+' #> Some(Increment)
             <|> '-' #> Some(Decrement)
             <|> '.' #> Some(Output)
             <|> ',' #> Some(Input)
             <|> between('[', ']' <|> fail("unclosed loop"), bf.map(p => Some(Loop(p))))
             <|> (whitespaceBF #> None)).map(_.flatten)
        attempt(bf <* eof) <|> fail("\"]\" closes a loop, but there isn't one open")
    }
    println(brainfuck.pretty)

    // https://github.com/Jellonator/Nandlang
    sealed trait NandExpr
    case class NandNand(l: NandExpr, r: NandExpr) extends NandExpr
    case class NandCall(f: String, args: List[NandExpr]) extends NandExpr
    case class NandLit(c: Char) extends NandExpr
    case class NandId(v: String, idx: Option[Int]) extends NandExpr
    sealed trait NandStmt
    case class NandFunc(name: String, args: (List[NandId], List[NandId]), block: NandBlock) extends NandStmt
    case class NandIf(cond: NandExpr, block: NandBlock, elseBlock: Option[NandBlock]) extends NandStmt
    case class NandWhile(cond: NandExpr, block: NandBlock) extends NandStmt
    case class NandVar(idlist: List[NandId], exprlist: List[NandExpr]) extends NandStmt
    case class NandNaked(expr: NandExpr) extends NandStmt
    case class NandBlock(stmts: List[NandStmt])

    def nand =
    {
        val nandlang =
            LanguageDef(
                /*Comment start*/     "",
                /*Comment end*/       "",
                /*Line comment*/      "//",
                /*Nested comments?*/  false,
                /*Identifier start*/  Predicate(c => c.isLetter || c == '_'),
                /*Identifier letter*/ Predicate(c => c.isLetterOrDigit || c == '_'),
                /*Operator start*/    NotRequired,
                /*Operator letter*/   NotRequired,
                /*Keywords*/          Set("if", "else", "function", "while", "var"),
                /*Operators*/         Set("!"),
                /*Case sensitive*/    true,
                /*Whitespace*/        Predicate(Char.isWhitespace))
        val tok = new TokenParser(nandlang)
        val identifier = tok.identifier
        val index = tok.brackets(tok.natural)
        val variable = lift2(NandId, identifier, option(index))
        val literal = tok.lexeme('0'.map(NandLit)) <|> tok.lexeme('1'.map(NandLit)) <|> tok.charLiteral.map(NandLit)
        lazy val expr: Parsley[NandExpr] = chainl1(nandexpr, tok.lexeme('!' #> (NandNand(_, _))))
        lazy val nandexpr = literal <|> attempt(funccall) <|> variable
        lazy val funccall = lift2(NandCall, identifier, tok.parens(exprlist))
        lazy val exprlist = tok.commaSep(expr)
        val exprlist1 = tok.commaSep1(+expr)
        val varlist = tok.commaSep(variable)
        val varlist1 = tok.commaSep1(variable)
        val funcparam = varlist <~> (tok.symbol(':') *> varlist).getOrElse(Nil)
        val varstmt = lift2(NandVar, optional(tok.keyword("var")) *> varlist1, tok.symbol('=') *> exprlist1 <* tok.semi)
        lazy val ifstmt = tok.keyword("if") *> lift3(NandIf, +expr, block, option(tok.keyword("else") *> block))
        lazy val whilestmt = tok.keyword("while") *> lift2(NandWhile, +expr, block)
        lazy val statement = ifstmt <|> whilestmt <|> attempt(varstmt) <|> ((+expr).map(NandNaked) <* tok.semi)
        lazy val block: Parsley[NandBlock] = tok.braces(many(statement)).map(NandBlock)
        val funcdef = tok.keyword("function") *> lift3(NandFunc, identifier, tok.parens(funcparam), block)
        tok.whiteSpace *> many(funcdef) <* eof
    }

    trait WhileAexp
    case class WhileNat(n: Int) extends WhileAexp
    case class WhileAVar(v: String) extends WhileAexp
    trait WhileArithOp
    case object WhileAdd extends WhileArithOp
    case object WhileSub extends WhileArithOp
    case object WhileMul extends WhileArithOp
    case class WhileOp(op: WhileArithOp)
                      (l: WhileAexp, r: WhileAexp) extends WhileAexp
    trait WhileBexp
    case object WhileTrue extends WhileBexp
    case object WhileFalse extends WhileBexp
    case class WhileBVar(v: String) extends WhileBexp
    trait WhileCompOp
    case object WhileLt extends WhileCompOp
    case object WhileLe extends WhileCompOp
    case object WhileEq extends WhileCompOp
    case class WhileComp(l: WhileAexp, op: WhileCompOp, r: WhileAexp) extends WhileBexp
    case class WhileAnd(l: WhileBexp, r: WhileBexp) extends WhileBexp
    case class WhileOr(l: WhileBexp, r: WhileBexp) extends WhileBexp
    case class WhileNot(c: WhileBexp) extends WhileBexp
    trait WhileStm
    case class WhileSeq(s1: WhileStm, s2: WhileStm) extends WhileStm
    case class WhileIf(cond: WhileBexp, t: WhileStm, e: WhileStm) extends WhileStm
    case class WhileWhile(cond: WhileBexp, body: WhileStm) extends WhileStm
    case class WhileAss(v: String, to: Either[WhileAexp, WhileBexp]) extends WhileStm
    case object WhileSkip extends WhileStm

    def whileLang =
    {
        val whilelang =
            LanguageDef(
                /*Comment start*/     "/*",
                /*Comment end*/       "*/",
                /*Line comment*/      "//",
                /*Nested comments?*/  false,
                /*Identifier start*/  Predicate(c => c.isLetter || c == '_'),
                /*Identifier letter*/ Predicate(c => c.isLetterOrDigit || c == '_'),
                /*Operator start*/    CharSet('+', ':', '=', '&', '|', '¬', '*', '-', '<', ';'),
                /*Operator letter*/   CharSet('+', ':', '=', '&', '|', '¬', '*', '-', '<', ';'),
                /*Keywords*/          Set("true", "false", "if", "then",
                                          "else", "while", "do", "skip"),
                /*Operators*/         Set("+", ":=", ";", "=", "&", "|",
                                          "¬", "*", "-", "<", "<="),
                /*Case sensitive*/    true,
                /*Whitespace*/        Predicate(Char.isWhitespace))
        val tok = new TokenParser(whilelang)
        lazy val aexp: Parsley[WhileAexp] = new ExpressionParser(
            List(Infixes(List(tok.operator("+") #> WhileOp(WhileAdd) _,
                              tok.operator("-") #> WhileOp(WhileSub) _), AssocLeft),
                 Infixes(List(tok.operator("*") #> WhileOp(WhileMul) _), AssocLeft)), aexp_atom).expr
        lazy val aexp_atom = tok.integer.map(WhileNat) <|> tok.identifier.map(WhileAVar) <|> tok.parens(aexp)
        val compop = tok.operator("<=") #> WhileLt <|> tok.operator("<") #> WhileLt <|> tok.operator("=") #> WhileEq
        lazy val bexp: Parsley[WhileBexp] = new ExpressionParser(
            List(Infixes(List(tok.operator("|") #> WhileOr), AssocLeft),
                 Infixes(List(tok.operator("&") #> WhileAnd), AssocLeft),
                 Prefixes(List(tok.operator("¬") #> WhileNot))), bexp_atom).expr
        lazy val bexp_atom = (tok.keyword("true") #> WhileTrue
                          <|> tok.keyword("false") #> WhileFalse
                          <|> lift3(WhileComp, attempt(aexp), compop, aexp)
                          <|> tok.parens(bexp)
                          <|> tok.identifier.map(WhileBVar))
        lazy val stm: Parsley[WhileStm] = chainl1(stm_, tok.operator(";") #> WhileSeq)
        lazy val stm_ = +(lift2(WhileAss, tok.identifier <* tok.operator(":="), attempt(aexp.map(Left(_))) <|> bexp.map(Right(_)))
                      <|> lift3(WhileIf, tok.keyword("if") *> bexp, tok.keyword("then") *> stm, tok.keyword("else") *> stm)
                      <|> lift2(WhileWhile, tok.keyword("while") *> bexp, tok.keyword("do") *> stm)
                      <|> tok.keyword("skip") #> WhileSkip)
        tok.whiteSpace *> stm <* eof
    }

    type JSProgram = List[JSElement]
    trait JSElement
    case class JSFunction(fn: String, args: List[String], stm: JSCompoundStm) extends JSElement
    type JSCompoundStm = List[JSStm]
    trait JSStm extends JSElement
    case object JSSemi extends JSStm
    case class JSIf(cond: JSExpr, t: JSStm, e: Option[JSStm]) extends JSStm
    case class JSWhile(cond: JSExpr, body: JSStm) extends JSStm
    case class JSFor(init: Option[Either[List[JSVar], JSExpr]], cond: Option[JSExpr], step: Option[JSExpr])(body: JSStm) extends JSStm
    case class JSForIn(init: Either[List[JSVar], JSExpr], expr: JSExpr)(body: JSStm) extends JSStm
    case object JSBreak extends JSStm
    case object JSContinue extends JSStm
    case class JSWith(expr: JSExpr, body: JSStm) extends JSStm
    case class JSReturn(e: Option[JSExpr]) extends JSStm
    case class JSBlock(stms: JSCompoundStm) extends JSStm
    case class JSNaked(n: Either[List[JSVar], JSExpr]) extends JSStm
    case class JSVar(v: String, asgn: Option[JSExpr_])
    type JSExpr = List[JSExpr_]
    trait JSExpr_
    case class JSAsgn(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSCond(c: JSExpr_, t: JSExpr_, e: JSExpr_) extends JSExpr_
    case class JSOr(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSAnd(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSBitOr(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSBitXor(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSBitAnd(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSEq(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSNe(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSLt(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSGt(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSLe(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSGe(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSShl(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSShr(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSAdd(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSSub(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSMul(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSDiv(l: JSExpr_, r: JSExpr_) extends JSExpr_
    case class JSMod(l: JSExpr_, r: JSExpr_) extends JSExpr_
    trait JSUnary extends JSExpr_
    case class JSPlus(l: JSUnary) extends JSUnary
    case class JSNeg(l: JSUnary) extends JSUnary
    case class JSBitNeg(l: JSUnary) extends JSUnary
    case class JSNot(l: JSUnary) extends JSUnary
    case class JSInc(l: JSUnary) extends JSUnary
    case class JSDec(l: JSUnary) extends JSUnary
    case class JSNew(c: JSCons) extends JSUnary
    case class JSDel(m: JSMember) extends JSUnary
    trait JSMember extends JSUnary
    case class JSPrimExp(a: JSAtom) extends JSMember
    case class JSAccess(a: JSAtom, m: JSMember) extends JSMember
    case class JSIndex(a: JSAtom, e: JSExpr) extends JSMember
    case class JSCall(a: JSAtom, args: List[JSExpr_]) extends JSMember
    trait JSCons extends JSUnary
    case class JSQual(v: String, con: JSCons) extends JSCons
    case class JSConCall(v: String, args: List[JSExpr_]) extends JSCons
    trait JSAtom
    case class JSParens(expr: JSExpr) extends JSAtom
    case class JSId(v: String) extends JSAtom
    case class JSInt(n: Int) extends JSAtom
    case class JSFloat(f: Double) extends JSAtom
    case class JSString(s: String) extends JSAtom
    case object JSTrue extends JSAtom
    case object JSFalse extends JSAtom
    case object JSNull extends JSAtom
    case object JSThis extends JSAtom

    def javascript: Parsley[JSProgram] =
    {
        val jslang =
            LanguageDef(
                /*Comment start*/     "",
                /*Comment end*/       "",
                /*Line comment*/      "",
                /*Nested comments?*/  false,
                /*Identifier start*/  Predicate(c => c.isLetter || c == '_'),
                /*Identifier letter*/ Predicate(c => c.isLetterOrDigit || c == '_'),
                /*Operator start*/    CharSet('+', '-', '*', '/', '=', '<', '>', '!', '~', '&', '|', '.', '%', '^'),
                /*Operator letter*/   CharSet('+', '-', '*', '/', '=', '<', '>', '!', '~', '&', '|', '.', '%', '^'),
                /*Keywords*/          Set("true", "false", "if", "else",
                                          "for", "while", "break", "continue",
                                          "function", "var", "new", "delete",
                                          "this", "null", "return"),
                /*Operators*/         Set("+", "-", "*", "/", "<", ">",
                                          "<=", ">=", "==", "!=", "!",
                                          "~", "&&", "||", "&", "|", "^",
                                          "<<", ">>", "++", "--", "%", "."),
                /*Case sensitive*/    true,
                /*Whitespace*/        Predicate(Char.isWhitespace))
        val tok = new TokenParser(jslang)
        lazy val primaryExpr: Parsley[JSAtom] = tok.parens(expr).map(JSParens) <|> tok.identifier.map(JSId) <|> tok.naturalOrFloat.map
        {
            case Left(x) => JSInt(x)
            case Right(f) => JSFloat(f)
        } <|> tok.stringLiteral.map(JSString) <|> tok.keyword("true") #> JSTrue <|> tok.keyword("false") #> JSFalse <|> tok.keyword("null") #> JSNull <|> tok.keyword("this") #> JSThis
        lazy val member: Parsley[JSMember] = primaryExpr <**> (
            tok.parens(tok.commaSep(asgn)).map(args => (fn: JSAtom) => JSCall(fn, args))
        <|> tok.brackets(expr).map(idx => (obj: JSAtom) => JSIndex(obj, idx))
        <|> tok.dot *> member.map(attr => (obj: JSAtom) => JSAccess(obj, attr))
        </> JSPrimExp)
        lazy val conCall: Parsley[JSCons] = tok.identifier <**>
            (tok.dot *> conCall.map(con => (id: String) => JSQual(id, con))
         <|> tok.parens(tok.commaSep(asgn)).map(args => (id: String) => JSConCall(id, args))
         </> (JSConCall(_, Nil)))
        lazy val con = lift2(JSQual, tok.keyword("this") #> "this", tok.dot *> conCall) <|> conCall
        lazy val memOrCon =
            (tok.keyword("delete") *> member.map(JSDel)
         <|> tok.keyword("new") *> con
         <|> member)
        lazy val _expr = new ExpressionParser(
            List(Infixes(List(tok.operator("||") #> JSOr), AssocLeft),
                 Infixes(List(tok.operator("&&") #> JSAnd), AssocLeft),
                 Infixes(List(tok.operator("|") #> JSBitOr), AssocLeft),
                 Infixes(List(tok.operator("^") #> JSBitXor), AssocLeft),
                 Infixes(List(tok.operator("&") #> JSBitAnd), AssocLeft),
                 Infixes(List(tok.operator("==") #> JSEq, tok.operator("!=") #> JSNe), AssocLeft),
                 Infixes(List(tok.operator("<=") #> JSLe, tok.operator("<") #> JSLt,
                              tok.operator(">=") #> JSGe, tok.operator(">") #> JSGt), AssocLeft),
                 Infixes(List(tok.operator("<<") #> JSShl, tok.operator(">>") #> JSShr), AssocLeft),
                 Infixes(List(tok.operator("+") #> JSAdd, tok.operator("-") #> JSSub), AssocLeft),
                 Infixes(List(tok.operator("*") #> JSMul, tok.operator("/") #> JSDiv,
                              tok.operator("%") #> JSMod), AssocLeft),
                Prefixes(List(tok.operator("--") #> JSDec, tok.operator("++") #> JSInc,
                              tok.operator("-") #> JSNeg, tok.operator("+") #> JSPlus,
                              tok.operator("~") #> JSBitNeg, tok.operator("!") #> JSNot))), memOrCon).expr
        // TODO: There must be a better way to do this...
        lazy val condExpr = lift2((c: JSExpr_, o: Option[(JSExpr_, JSExpr_)]) => o match
        {
            case Some((t, e)) => JSCond(c, t, e)
            case None => c
        }, _expr, option(tok.symbol('?') *> asgn <~> (tok.symbol(':') *> asgn)))
        lazy val asgn: Parsley[JSExpr_] = chainl1(condExpr, tok.symbol('=') #> JSAsgn)
        lazy val expr: Parsley[JSExpr] = tok.commaSep1(+asgn)
        val variable = lift2(JSVar, tok.identifier, option(tok.symbol('=') *> asgn))
        val varsOrExprs = tok.keyword("var") *> tok.commaSep1(variable).map(Left(_)) <|> expr.map(Right(_))
        lazy val stmt: Parsley[JSStm] =
            (tok.semi #> JSSemi
         <|> tok.keyword("if") *> lift3(JSIf, tok.parens(expr), stmt, option(tok.keyword("else") *> stmt))
         <|> tok.keyword("while") *> lift2(JSWhile, tok.parens(expr), stmt)
         <|> (tok.keyword("for") *> tok.parens(lift2(JSForIn(_, _), varsOrExprs, tok.keyword("in") *> expr)
                                           <\> lift3(JSFor(_, _, _), option(varsOrExprs), option(expr), option(expr))) <*> stmt)
         <|> tok.keyword("break") #> JSBreak
         <|> tok.keyword("continue") #> JSContinue
         <|> tok.keyword("with") *> lift2(JSWith, tok.parens(expr), stmt)
         <|> tok.keyword("return") *> option(expr).map(JSReturn)
         <|> compound.map(JSBlock)
         <|> varsOrExprs.map(JSNaked))
        lazy val compound = tok.braces(many(stmt))
        val element = tok.keyword("function") *> lift3(JSFunction, tok.identifier, tok.parens(tok.commaSep(tok.identifier)), compound) <|> stmt
        tok.whiteSpace *> many(element) <* eof
    }

    println(whileLang.pretty)
}

/*private [parsley] object BenchParser extends scala.util.parsing.combinator.Parsers
{
    import scala.util.parsing.input.{NoPosition, Reader}
    override type Elem = Char
    private val elem: Parser[Int] = accept("1", {case '1' => '1'.toInt})
    private val op: Parser[(Int, Int) => Int] = accept("+", {case '+' => _ + _})
    val bench = chainl1(elem, op)

    private class BenchReader(tokens: String) extends Reader[Elem]
    {
        override def first = tokens.head
        override def atEnd = tokens.isEmpty
        override def pos = NoPosition
        override def rest = new BenchReader(tokens.tail)
    }

    def apply(input: String) = bench(new BenchReader(input))
}*/

/*
// TODO: Test out https://github.com/djspiewak/parseback
object Parseback
{
    ??? 
}
*/

private [parsley] object Native
{
    val recursiveDescent: String => Either[String, Int] = (input: String) => expr(input, 0)._1
    def expr(input: String, index: Int): (Either[String, Int], Int) =
    {
        one(input, index) match
        {
            case (Right(x), index_) => plus(input, index_) match
            {
                case (Right(op), index__) => expr(input, index__) match
                {
                    case (Right(y), index___) => (Right(op(x)(y)), index___)
                    case (err, index___) => (err, index___)
                }
                case (_, index__) => (Right(x), index__)
            }
            case (err, index_) => (err, index_)
        }
    }
    def exprl(input: String, index: Int): (Either[String, Int], Int) =
    {
        one(input, index) match
        {
            case (Right(x), index_) =>
                val (ops, index__) = rep(plusone)(input, index_)
                (Right(ops.foldLeft(x)((acc, op) => op(acc))), index__)
            case err => err
        }
    }
    @tailrec def rep[A](p: (String, Int) => (Either[String, A], Int))
                       (input: String, index: Int, acc: List[A] = Nil): (List[A], Int) = p(input, index) match
    {
        case (Right(x), index_) => rep(p)(input, index_, x::acc)
        case (_, index_) => (acc.reverse, index_)
    }
    def one(input: String, index: Int): (Either[String, Int], Int) =
    {
        if (index < input.length && input(index) == '1') (Right('1'.toInt), index + 1)
        else (Left(s"$index: Expected 1, got ${if (index < input.length) input(index) else "end of input"}"), index)
    }
    def plus(input: String, index: Int): (Either[String, Int => Int => Int], Int) =
    {
        if (index < input.length && input(index) == '+') (Right((x: Int) => (y: Int) => x + y), index + 1)
        else (Left(s"$index: Expected +, got ${if (index < input.length) input(index) else "end of input"}"), index)
    }
    def plusone(input: String, index: Int): (Either[String, Int => Int], Int) =
    {
        plus(input, index) match
        {
            case (Right(op), index_) => one(input, index_) match
            {
                case (Right(y), index__) => (Right((z: Int) => op(z)(y)), index__)
                case (Left(err), index__) => (Left(err), index__)
            }
            case (Left(err), index__) => (Left(err), index__)
        }
    }

    val parseTail: String => Int = (input: String) => parseTail_(input, 0, 0)
    @tailrec def parseTail_(input: String, index: Int, sum: Int): Int =
    {
        if (index >= input.length) sum
        else input(index) match
        {
            case c@'1' => parseTail_(input, index + 1, sum + c)
            case '+' => parseTail_(input, index + 1, sum)
        }
    }
}

private [parsley] object FastParser
{
    import fastparse.all._
    //import fastparse.WhitespaceApi
    type Parser[A] = fastparse.all.Parser[A]
    /*val x = P("1").!.map(_(0).toInt)
    val y = P("+").!.map(_ => (x: Int) => (y: Int) => x + y)
    def chainlf[A](p: Parser[A], op: Parser[A => A => A]): Parser[A] =
    {
        for (x <- p;
             fs <- (for (f <- op;
                         y <- p)
                 yield ((x: A) => f(x)(y))).rep)
            yield fs.foldLeft(x)((y, f) => f(y))
        //val ops = (op ~ p).map{case (f, x) => (y: A) => f(y)(x)}
        //(p ~ ops.rep).map{case (x, (xs: Seq[A=>A])) => xs.foldLeft(x)((y, f) => f(y))}
    }
    val z = chainlf(x, y)
    def repeat[A](p: Parser[A], n: Int): Parser[A] =
    {
        if (n > 0) for (_ <- p; x <- repeat(p, n-1)) yield x
        else p
    }
    val big = repeat(P("1"), 5000)*/

    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp

    // This is an optimisation for the logic inside. Since this is the last in a chain of ors
    // it doesn't need to account for the other symbols (just needs to not accidentally consume ])
    private val whitespaceBF = P(CharPred(_ != ']'))

    def brainfuck: Parser[List[BrainFuckOp]] =
    {
        lazy val bf: Parser[List[BrainFuckOp]] =
            (P(">").map(_ => Some(RightPointer))
           | P("<").map(_ => Some(LeftPointer))
           | P("+").map(_ => Some(Increment))
           | P("-").map(_ => Some(Decrement))
           | P(".").map(_ => Some(Output))
           | P(",").map(_ => Some(Input))
           | P("[" ~ bf ~ "]").map(p => Some(Loop(p)))
           | whitespaceBF.map(_ => None)).rep.map(_.flatten.toList)
        bf ~ End
    }

    /*val White = WhitespaceApi.Wrapper {
        import fastparse.all._
        NoTrace(" ".rep)
    }

    import White._
    import fastparse.noApi._

    lazy val ops: Parser[BrainFuckOp] =
        CharIn("<>+-.,").!.map {
            case "<" => LeftPointer
            case ">" => RightPointer
            case "+" => Increment
            case "-" => Decrement
            case "." => Output
            case "," => Input
        }.opaque("operators(<>+-.,)")

    lazy val loop: Parser[List[BrainFuckOp]] =
        P("[".opaque("Opening bracket '['") ~/
            (expr | PassWith(Nil)).opaque("expression") ~ // [] is ok
            "]".opaque("']' Closing bracket"))
            .map { l => Loop(l.toList) :: Nil }

    lazy val expr = (loop | ops.rep(1)).rep.map(_.flatten.toList) // empty should fail

    lazy val parser: Parser[List[BrainFuckOp]] = Start ~ expr ~ End*/
}

private [parsley] object Benchmark
{
    def read(filename: String) = Try(Source.fromFile(filename).getLines().mkString("\n") + "\n").getOrElse("")
    def parseParsley(p: Any, s: String): Any = runParserFastUnsafe(p.asInstanceOf[Parsley[_]], s)
    def parseFastParse(p: Any, s: String): Any = p.asInstanceOf[fastparse.all.Parser[_]].parse(s)

    val benchmarks: Array[(String, Any, (Any, String) => Any, Int)] =
        Array(("inputs/helloworld_golfed.bf", ParsleyBench.brainfuck, parseParsley, 1000000),
              ("inputs/helloworld_golfed.bf", FastParser.brainfuck, parseFastParse, 1000000),
              ("inputs/helloworld.bf", ParsleyBench.brainfuck, parseParsley, 100000),
              ("inputs/helloworld.bf", FastParser.brainfuck, parseFastParse, 100000),
              ("inputs/arrays.nand", ParsleyBench.nand, parseParsley, 100000),
              ("inputs/test.while", ParsleyBench.whileLang, parseParsley, 100000),
              ("inputs/fibonacci.js", ParsleyBench.javascript, parseParsley, 100000))

    def main(args: Array[String]): Unit =
    {
        //Console.in.read()
        //val p = ParsleyBench.chain
        //val input = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"
        //val exec = runParserFastUnsafe _
        val (filename, p, exec, iters) = benchmarks(6)
        val input = read(filename)
        val start = System.currentTimeMillis()
        println(exec(p, input))
        for (_ <- 0 to iters) exec(p, input)
        println(System.currentTimeMillis() - start)
    }
}