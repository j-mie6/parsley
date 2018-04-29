package parsley

import fastparse.WhitespaceApi
import parsley.ExpressionParser._

import scala.annotation.{switch, tailrec}
import scala.io.Source
import scala.util.Try

private [parsley] object ParsleyBench
{
    import parsley.Parsley._
    import parsley.Combinator._
    import parsley.Char._
    val liftab: Parsley[String] = lift2[Char, Char, String]((x, y) => x.toString + y.toString, 'a', 'b')
    val aconsb: Parsley[List[Char]] = 'a' <::> ('b' #> Nil)
    val athenb: Parsley[String] = 'a' *> 'b' #> "ab"
    val manya: Parsley[List[Char]] = many('a') <* 'b'
    def chain: Parsley[Int] = chainl1('1' <#> (_.toInt), '+' #> ((x: Int, y: Int) => x + y))
    
    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp

    def brainfuck: Parsley[List[BrainFuckOp]] =
    {
        val bflang = LanguageDef.plain.copy(space = Predicate(c => (c: @switch) match
        {
            case '>' | '<' | '+' | '-' | '.' | ',' | '[' | ']' => false
            case _ => true
        }))
        val tok = new TokenParser(bflang)
        lazy val bf: Parsley[List[BrainFuckOp]] =
            many(tok.lexeme('>' #> RightPointer)
             <|> tok.lexeme('<' #> LeftPointer)
             <|> tok.lexeme('+' #> Increment)
             <|> tok.lexeme('-' #> Decrement)
             <|> tok.lexeme('.' #> Output)
             <|> tok.lexeme(',' #> Input)
             <|> tok.brackets(bf.map(Loop)))
        tok.whiteSpace *> attempt(bf <* eof) <|> fail("\"]\" closes a loop, but there isn't one open")
    }

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
        lazy val aexp: Parsley[WhileAexp] = new ExpressionParser(aexp_atom,
            Infixes(AssocLeft, tok.operator("+") #> WhileOp(WhileAdd) _,
                               tok.operator("-") #> WhileOp(WhileSub) _),
            Infixes(AssocLeft, tok.operator("*") #> WhileOp(WhileMul) _)).expr
        lazy val aexp_atom = tok.integer.map(WhileNat) <|> tok.identifier.map(WhileAVar) <|> tok.parens(aexp)
        val compop = tok.operator("<=") #> WhileLt <|> tok.operator("<") #> WhileLt <|> tok.operator("=") #> WhileEq
        lazy val bexp: Parsley[WhileBexp] = new ExpressionParser(bexp_atom,
            Infixes(AssocLeft, tok.operator("|") #> WhileOr),
            Infixes(AssocLeft, tok.operator("&") #> WhileAnd),
            Prefixes(tok.operator("¬") #> WhileNot)).expr
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

    import JavascriptAST._

    def javascript: Parsley[JSProgram] =
    {
        val jslang =
            LanguageDef(
                /*Comment start*/     "/*",
                /*Comment end*/       "*/",
                /*Line comment*/      "//",
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
        lazy val primaryExpr: Parsley[JSAtom] = tok.parens(expr).map(JSParens) <|> tok.brackets(tok.commaSep(+asgn)).map(JSArray) <|> tok.identifier.map(JSId) <|> tok.naturalOrFloat.map
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
        lazy val _expr = new ExpressionParser(memOrCon,
            Prefixes(tok.operator("--") #> JSDec, tok.operator("++") #> JSInc,
                     tok.operator("-") #> JSNeg, tok.operator("+") #> JSPlus,
                     tok.operator("~") #> JSBitNeg, tok.operator("!") #> JSNot),
            Postfixes(tok.operator("++") #> JSDec, tok.operator("++") #> JSInc),
            Infixes(AssocLeft, tok.operator("*") #> JSMul, tok.operator("/") #> JSDiv,
                               tok.operator("%") #> JSMod),
            Infixes(AssocLeft, tok.operator("+") #> JSAdd, tok.operator("-") #> JSSub),
            Infixes(AssocLeft, tok.operator("<<") #> JSShl, tok.operator(">>") #> JSShr),
            Infixes(AssocLeft, tok.operator("<=") #> JSLe, tok.operator("<") #> JSLt,
                               tok.operator(">=") #> JSGe, tok.operator(">") #> JSGt),
            Infixes(AssocLeft, tok.operator("==") #> JSEq, tok.operator("!=") #> JSNe),
            Infixes(AssocLeft, attempt(tok.operator("&")) #> JSBitAnd),
            Infixes(AssocLeft, tok.operator("^") #> JSBitXor),
            Infixes(AssocLeft, attempt(tok.operator("|")) #> JSBitOr),
            Infixes(AssocLeft, tok.operator("&&") #> JSAnd),
            Infixes(AssocLeft, tok.operator("||") #> JSOr)).expr
        lazy val condExpr = lift2((c: JSExpr_, o: Option[(JSExpr_, JSExpr_)]) => o match
        {
            case Some((t, e)) => JSCond(c, t, e)
            case None => c
        }, _expr, option(tok.symbol('?') *> asgn <~> (tok.symbol(':') *> asgn)))
        lazy val asgn: Parsley[JSExpr_] = chainl1(condExpr, tok.symbol('=') #> JSAsgn)
        lazy val expr: Parsley[JSExpr] = tok.commaSep1(+asgn)
        val optExpr = +option(expr)
        val parensExpr = +tok.parens(expr)
        val variable = lift2(JSVar, tok.identifier, option(tok.symbol('=') *> +asgn))
        val varsOrExprs = +(tok.keyword("var") *> tok.commaSep1(variable).map(Left(_)) <|> expr.map(Right(_)))
        lazy val stmt: Parsley[JSStm] =
            (tok.semi #> JSSemi
         <|> tok.keyword("if") *> lift3(JSIf, parensExpr, stmt, option(tok.keyword("else") *> stmt))
         <|> tok.keyword("while") *> lift2(JSWhile, parensExpr, stmt)
         <|> (tok.keyword("for") *> tok.parens(lift2(JSForIn(_, _), varsOrExprs, tok.keyword("in") *> expr)
                                           <\> lift3(JSFor(_, _, _), option(varsOrExprs) <* tok.semi, optExpr <* tok.semi, optExpr)) <*> stmt)
         <|> tok.keyword("break") #> JSBreak
         <|> tok.keyword("continue") #> JSContinue
         <|> tok.keyword("with") *> lift2(JSWith, parensExpr, stmt)
         <|> tok.keyword("return") *> optExpr.map(JSReturn)
         <|> compound.map(JSBlock)
         <|> varsOrExprs.map(JSNaked))
        lazy val compound = tok.braces(many(stmt))
        val element = tok.keyword("function") *> lift3(JSFunction, tok.identifier, tok.parens(tok.commaSep(tok.identifier)), compound) <|> stmt
        tok.whiteSpace *> many(element) <* eof
    }

    def json: Parsley[Any] =
    {
        val jsontoks = LanguageDef.plain.copy(space = Predicate(Char.isWhitespace))
        val tok = new TokenParser(jsontoks)
        lazy val obj: Parsley[Map[String, Any]] = tok.braces(tok.commaSep(+(tok.lexeme(tok.rawStringLiteral) <~> tok.colon *> value)).map(_.toMap))
        lazy val array = tok.brackets(tok.commaSep(value))
        lazy val value: Parsley[Any] =
            (tok.lexeme(tok.rawStringLiteral)
         <|> tok.symbol("true")
         <|> tok.symbol("false")
         <|> array
         <|> attempt(tok.float)
         <|> tok.integer
         <|> obj)
        tok.whiteSpace *> (obj <|> array) <* eof
    }
    //println(javascript.pretty.replace("; ", ";\n"))
    def maths: Parsley[Int] =
    {
        val tok = new TokenParser(LanguageDef.plain.copy(space = Predicate(Char.isWhitespace)))
        lazy val expr = new ExpressionParser[Int](atom,
            Prefixes[Int](tok.lexeme('-' #> (x => -x)), tok.lexeme('+' #> (x => +x))),
            Infixes[Int](AssocLeft, tok.lexeme('^' #> ((x, y) => math.pow(x.toDouble, y.toDouble).toInt))),
            Infixes[Int](AssocLeft, tok.lexeme('/' #> (_ / _)), tok.lexeme('%' #> (_ % _))),
            Infixes[Int](AssocLeft, tok.lexeme('*' #> (_ * _))),
            Infixes[Int](AssocLeft, tok.lexeme('+' #> (_ + _)), tok.lexeme('-' #> (_ - _)))).expr
        lazy val atom: Parsley[Int] = tok.natural <|> tok.parens(expr)
        tok.whiteSpace *> expr <* eof
    }
    println(maths.pretty)
    def maths_unsub: Parsley[Int] =
    {
        val tok = new TokenParser(LanguageDef.plain.copy(space = Predicate(Char.isWhitespace)))
        lazy val expr: Parsley[Int] = chainl1[Int](mul, choice(tok.lexeme('+' #> (_ + _)), tok.lexeme('-' #> (_ - _))))
        lazy val mul = chainl1[Int](div, tok.lexeme('*' #> (_ * _)))
        lazy val div = chainl1[Int](pow, choice(tok.lexeme('/' #> (_ / _)), tok.lexeme('%' #> (_ % _))))
        lazy val pow = chainl1[Int](signs, tok.lexeme('^' #> ((x, y) => math.pow(x.toDouble, y.toDouble).toInt)))
        lazy val signs = chainPre[Int](atom, choice(tok.lexeme('-' #> (x => -x)), tok.lexeme('+' #> (x => +x))))
        lazy val atom = tok.natural <|> tok.parens(expr)
        tok.whiteSpace *> expr <* eof
    }
    println(maths_unsub.pretty)
}

object JavascriptAST
{
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
    case class JSArray(expr: JSExpr) extends JSAtom
    case class JSId(v: String) extends JSAtom
    case class JSInt(n: Int) extends JSAtom
    case class JSFloat(f: Double) extends JSAtom
    case class JSString(s: String) extends JSAtom
    case object JSTrue extends JSAtom
    case object JSFalse extends JSAtom
    case object JSNull extends JSAtom
    case object JSThis extends JSAtom
}

/*private [parsley] object PfS
{
    import parsec.Parser
    import parsec._
    import parsec.ParsecE._
    def json: Parser[Any] =
    {
        val jsontoks = LanguageDef[String, Unit, Default]("", "", "", false, empty, empty, empty, empty, Nil, Nil, true, parsec.Char.whitespace)
        val tok = new parsec.TokenParser[String, Unit, Default](jsontoks)
        lazy val obj: Parser[Map[String, Any]] = tok.braces(tok.commaSep(lift2((x: String) => (y: Any) => (x, y), tok.lexeme(tok.stringLiteral), tok.colon *> value)).map(_.toMap))
        lazy val array: Parser[Any] = tok.brackets(tok.commaSep(value))
        lazy val value: Parser[Any] =
            (tok.lexeme(tok.stringLiteral)
             <|> tok.symbol("true")
             <|> tok.symbol("false")
             <|> array
             <|> tryParse(tok.float)
             <|> tok.integer
             <|> obj)
        tok.whiteSpace *> (obj <|> array) <* parsec.Combinator.eof
    }
    def parseJson(s: String) = parsec.runParser(json, (), "", StringStream(s), 1, 1)
}*/

private [parsley] object BenchParser extends scala.util.parsing.combinator.Parsers
{
    import scala.util.parsing.input.{NoPosition, Reader}
    override type Elem = Char
    private val elem: Parser[Int] = accept("1", {case '1' => '1'.toInt})
    private val op: Parser[(Int, Int) => Int] = accept("+", {case '+' => _ + _})
    val bench = chainl1(elem, op)
    //val json = scala.util.parsing.json.JSON

    private class BenchReader(tokens: String) extends Reader[Elem]
    {
        override def first = tokens.head
        override def atEnd = tokens.isEmpty
        override def pos = NoPosition
        override def rest = new BenchReader(tokens.tail)
    }

    def apply(input: String) = bench(new BenchReader(input))
}

private [parsley] object ScalaParserCombinatorsBrainFuck extends scala.util.parsing.combinator.Parsers
{
    import scala.util.parsing.input.{NoPosition, Reader}
    override type Elem = Char

    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp

    val ws = rep(acceptIf(c => (c: @switch) match
    {
        case '>' | '<' | '+' | '-' | '.' | ',' | '[' | ']' => false
        case _ => true
    })(_ => ""))
    val bf: Parser[List[BrainFuckOp]] = rep((accept("operator",
    {
        case '>' => RightPointer
        case '<' => LeftPointer
        case '+' => Increment
        case '-' => Decrement
        case '.' => Output
        case ',' => Input
    }) | (accept('[') ~> ws ~> (bf ^^ Loop) <~ accept(']'))) <~ ws)

    private class BenchReader(tokens: String) extends Reader[Elem]
    {
        override def first = tokens.head
        override def atEnd = tokens.isEmpty
        override def pos = NoPosition
        override def rest = new BenchReader(tokens.tail)
    }

    def apply(input: String) = (ws ~> bf)(new BenchReader(input))
}

private [parsley] object ScalaParserCombinatorsMath extends scala.util.parsing.combinator.Parsers
{
    import scala.util.parsing.input.{NoPosition, Reader}
    override type Elem = Char

    val ws = rep(acceptIf(c => c == ' ' || c == '\n' || c == '\t')(_ => ""))
    val digit = acceptIf(_.isDigit)(_ => "")
    def tok[A](p: Parser[A]): Parser[A] = p <~ ws
    def chainPre[A](p: =>Parser[A], op: Parser[A => A]): Parser[A] = for (fs <- rep(op); x <- p) yield fs.foldRight(x)((f, y) => f(y))

    lazy val expr: Parser[Int] = chainl1[Int](mul, tok(accept('+') ^^^ ((x: Int, y: Int) => x + y)) | tok(accept('+') ^^^ ((x: Int, y: Int) => x - y)))
    lazy val mul = chainl1[Int](div, tok(accept('*') ^^^ ((x: Int, y: Int) => x * y)))
    lazy val div = chainl1[Int](pow, tok(accept('/') ^^^ ((x: Int, y: Int) => x / y)) | tok(accept('%') ^^^ ((x: Int, y: Int) => x % y)))
    lazy val pow = chainl1[Int](signs, tok(accept('^') ^^^ ((x, y) => math.pow(x.toDouble, y.toDouble).toInt)))
    lazy val signs = chainPre[Int](atom, tok(accept('+') ^^^ ((x: Int) => x)) | tok(accept('+') ^^^ ((x: Int) => -x)))
    lazy val atom = tok(rep1(digit).map(_.mkString.toInt)) | tok(accept('(')) ~> expr <~ tok(accept(')'))

    private class BenchReader(tokens: String) extends Reader[Elem]
    {
        override def first = tokens.head
        override def atEnd = tokens.isEmpty
        override def pos = NoPosition
        override def rest = new BenchReader(tokens.tail)
    }

    def apply(input: String) = (ws ~> expr)(new BenchReader(input))
}

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

    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp

    def brainfuck(in: String): List[BrainFuckOp] =
    {
        @tailrec def walk(in: List[Char], acc: List[BrainFuckOp] = Nil): (List[BrainFuckOp], List[Char]) = in match
        {
            case (']'::_) | Nil => (acc.reverse, in)
            case c::rest => (c: @switch) match
            {
                case '>' => walk(rest, RightPointer::acc)
                case '<' => walk(rest, LeftPointer::acc)
                case '+' => walk(rest, Increment::acc)
                case '-' => walk(rest, Decrement::acc)
                case '.' => walk(rest, Output::acc)
                case ',' => walk(rest, Input::acc)
                case '[' =>
                    val (body, rest_) = loop(rest)
                    walk(rest_, Loop(body)::acc)
                case _ => walk(rest, acc)
            }
        }
        def loop(in: List[Char]): (List[BrainFuckOp], List[Char]) =
        {
            val (body, rest) = walk(in)
            rest match
            {
                case ']'::rest_ => (body, rest_)
                case _ => throw new Exception("Unclosed loop :(")
            }
        }
        walk(in.toList) match
        {
            case (res, Nil) => res
            case _ => throw new Exception("] closes a loop, but no loop was opened...")
        }
    }
}

private [parsley] object FastParse
{
    import fastparse.all._
    type Parser[A] = fastparse.all.Parser[A]
    val x = P("1").!.map(_(0).toInt)
    val y = P("+").!.map(_ => (x: Int) => (y: Int) => x + y)
    def chainlf[A](p: Parser[A], op: Parser[A => A => A]): Parser[A] =
    {
        val ops = (op ~ p).map{case (f, x) => (y: A) => f(y)(x)}
        (p ~ ops.rep).map{case (x, (xs: Seq[A=>A])) => xs.foldLeft(x)((y, f) => f(y))}
    }
    val z = chainlf(x, y)
    def repeat[A](p: Parser[A], n: Int): Parser[A] =
    {
        if (n > 0) for (_ <- p; x <- repeat(p, n-1)) yield x
        else p
    }
    val big = repeat(P("1"), 5000)
}

private [parsley] object FastParseBrainfuck
{
    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp

    val White = WhitespaceApi.Wrapper {
        import fastparse.all._
        val Reserved = "<>+-.,[]".toSet
        NoTrace(ElemsWhile(!Reserved.contains(_)).rep)
    }

    import White._
    import fastparse.noApi._
    type Parser[A] = fastparse.noApi.Parser[A]

    lazy val ops: Parser[BrainFuckOp] =
        CharIn("<>+-.,").!.map {
            case "<" => LeftPointer
            case ">" => RightPointer
            case "+" => Increment
            case "-" => Decrement
            case "." => Output
            case "," => Input
        }.opaque(s"keywords(<>+-.,)")

    lazy val loop: Parser[List[BrainFuckOp]] =
        P("[".opaque("Opening bracket '['") ~/
          (expr | PassWith(Nil)).opaque("expression") ~ // [] is ok
          "]".opaque("']' Closing bracket"))
            .map {Loop(_) :: Nil}

    lazy val expr: Parser[List[BrainFuckOp]] = (loop | ops.rep(1)).rep
        .map {_.flatten.toList} // empty should fail

    lazy val parser: Parser[List[BrainFuckOp]] = Start ~ expr ~ End
}

private [parsley] object FastParseWhite
{
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
    val White = WhitespaceApi.Wrapper{
        import fastparse.all._
        lazy val inCommentSingle: fastparse.all.Parser[Unit] =
            P("*/"
            | CharsWhile(c => c != '/' && c != '*') ~ inCommentSingle
            | CharPred(c => c == '/' || c == '*') ~ inCommentSingle)
        val multiline = "/*" ~ inCommentSingle
        val comment = ("//" ~ CharsWhile(_ != '\n')) | multiline
        val spaces = (CharsWhileIn(" \n\t\r") | comment).rep
        NoTrace(spaces)
    }
    import fastparse.noApi._
    import White._
    import JavascriptAST._
    private type Parser[A] = fastparse.noApi.Parser[A]
    private lazy val inCommentSingle: Parser[Unit] =
        P("*/"
          | CharsWhile(c => c != '/' && c != '*') ~ inCommentSingle
          | CharPred(c => c == '/' || c == '*') ~ inCommentSingle)
    private val multiline = "/*" ~ inCommentSingle
    private val comment = ("//" ~ CharsWhile(_ != '\n')) | multiline
    private val spaces = (CharsWhileIn(" \n\t\r") | comment).rep
    private val identifier = (CharPred(c => c.isLetter || c == '_') ~ CharsWhile(c => c.isLetterOrDigit || c == '_').?).!
    private def key(s: String) = s ~~ !CharPred(c => c.isLetterOrDigit || c == '_')
    private def op(s: String) = spaces ~~ s ~~ !CharIn("+-*/=<>!~&|.%^")

    private val index = P("[" ~ CharIn('0'to'9').rep(1).!.map(_.toInt) ~/ "]")
    private val nvariable = P((identifier ~ index.?).map(NandId.tupled))
    private val literal = P("0").map(_ => NandLit('0')) | P("1").map(_ => NandLit('1')) | ("'" ~~ CharPred(_.isValidChar).! ~~ "'").map(s => NandLit(s(0)))
    private val nexpr: Parser[NandExpr] = P(nandexpr.rep(sep="!".~/, min=1).map(_.reduce(NandNand)))
    private lazy val nandexpr = P(literal | funccall | nvariable)
    private lazy val funccall = P((identifier ~ "(" ~/ exprlist ~/ ")").map(NandCall.tupled))
    private lazy val exprlist = P(nexpr.rep(sep=",".~/).map(_.toList))
    private val exprlist1 = P(nexpr.rep(sep=",".~/, min=1).map(_.toList))
    private val varlist = P(nvariable.rep(sep=",".~/).map(_.toList))
    private val varlist1 = P(nvariable.rep(sep=",".~/, min=1).map(_.toList))
    private val funcparam = P(varlist ~ (":" ~/ varlist | Pass.map(_ => Nil)))
    private val varstmt = P((key("var").? ~ varlist1 ~ "=" ~/ exprlist1 ~/ ";").map(NandVar.tupled))
    private val ifstmt = P(key("if") ~/ (nexpr ~/ block ~/ (key("else") ~/ block).?).map(NandIf.tupled))
    private val whilestmt = P(key("while") ~/ (nexpr ~/ block).map(NandWhile.tupled))
    private val statement = P(ifstmt| whilestmt | varstmt | (nexpr.map(NandNaked) ~/ ";"))
    private lazy val block: Parser[NandBlock] = P("{" ~ statement.rep.map(stmts => NandBlock(stmts.toList)) ~ "}")
    private val funcdef = P(key("function") ~/ (identifier.! ~/ "(" ~/ funcparam ~/ ")" ~/ block).map(NandFunc.tupled))
    val nand = spaces ~ funcdef.rep.map(_.toList) ~ spaces ~ End

    def chainPre[A](p: Parser[A], op: Parser[A => A]): Parser[A] = for (fs <- op.rep; x <- p.~/) yield fs.foldRight(x)((f, y) => f(y))
    def chainPost[A](p: Parser[A], op: Parser[A => A]): Parser[A] = for (x <- p; fs <- op.rep) yield fs.foldLeft(x)((y, f) => f(y))
    def chainl1[A](p: Parser[A], op: Parser[(A, A) => A]): Parser[A] = chainPost(p, for (f <- op; y <- p) yield (x: A) => f(x, y))
    private val mexpr: Parser[Int] = chainl1(mmul, (spaces ~~ "+").map[(Int, Int) => Int](_ => _+_) | (spaces ~~ "-").map[(Int, Int) => Int](_ => _-_))
    private lazy val mmul = P(mdiv.rep(sep=spaces ~~ "*", min=1).map(_.product))
    private lazy val mdiv = chainl1(mpow, (spaces ~~ "/").map[(Int, Int) => Int](_ => _/_) | P(spaces ~~ "%").map[(Int, Int) => Int](_ => _/_))
    private lazy val mpow = P(msigns.rep(sep=spaces ~~ "^", min=1).map(_.reduceLeft((x, y) => scala.math.pow(x.toDouble, y.toDouble).toInt)))
    private lazy val msigns = chainPre(matom, (spaces ~~ "+").map[Int => Int](_ => x => +x) | (spaces ~~ "-").map[Int => Int](_ => x => -x))
    private lazy val matom = spaces ~~ (CharIn('0'to'9').rep(1).!.map(_.toInt) | "(" ~~ mexpr ~/ ")")
    val math = mexpr ~~ spaces ~~ End

    /* JAVASCRIPT */
    private val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
    private val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
    private val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
    private val strChars      = P( CharsWhile(!"\"\\".contains(_: Char)) )
    private val string        = P( "\"" ~/ (strChars | escape).rep.! ~ "\"")
    private val digits        = P( CharsWhileIn("0123456789"))
    private val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
    private val fractional    = P( "." ~ digits )
    private val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )
    private val number        = P( integral ~ fractional.? ~ exponent.? ).!.map(_.toDouble)
    private lazy val primaryExpr: Parser[JSAtom] = P(
          parensExpr.map(JSParens)
        | ("[" ~ asgn.rep(sep=",") ~ "]").map(asgns => JSArray(asgns.toList))
        | identifier.map(JSId)
        | number.map(JSFloat)
        | string.map(JSString)
        | key("true").map(_ => JSTrue)
        | key("false").map(_ => JSFalse)
        | key("null").map(_ => JSNull)
        | key("this").map(_ => JSThis)
    )
    private lazy val member: Parser[JSMember] = P(primaryExpr ~
         (("(" ~ asgn.rep(sep=",") ~ ")").map(args => (fn: JSAtom) => JSCall(fn, args.toList))
        | ("[" ~/ expr ~ "]").map(idx => (obj: JSAtom) => JSIndex(obj, idx))
        | ("." ~ member.map(attr => (obj: JSAtom) => JSAccess(obj, attr)))
        | Pass.map(_ => JSPrimExp))
    ).map{case (x, f) => f(x)}
    private lazy val conCall: Parser[JSCons] = P(identifier ~
         (("." ~ conCall.map(con => (id: String) => JSQual(id, con)))
        | ("(" ~ asgn.rep(sep=",") ~ ")").map(args => (id: String) => JSConCall(id, args.toList))
        | Pass.map(_ => (id: String) => JSConCall(id, Nil)))
    ).map{case (x, f) => f(x)}
    private lazy val con = P((key("this").! ~ "." ~/ conCall).map(JSQual.tupled) | conCall)
    private lazy val memOrCon = P(key("delete") ~/ member.map(JSDel) | key("new") ~/ con | member)
    private lazy val _expr: Parser[JSExpr_] = P(
        chainl1(chainl1(chainl1(chainl1(chainl1(chainPost(chainPre(spaces ~~ memOrCon,
            op("++").map(_ => JSInc) | op("--").map(_ => JSDec)
          | op("-").map(_ => JSNeg) | op("+").map(_ => JSPlus)
          | op("~").map(_ => JSBitNeg) | op("!").map(_ => JSNot)),
            op("++").map(_ => JSInc) | op("--").map(_ => JSDec)),
            op("*").map(_ => JSMul) | op("/").map(_ => JSDiv) | op("%").map(_ => JSMod)),
            op("+").map(_ => JSAdd) | op("-").map(_ => JSSub)),
            op("<<").map(_ => JSShl) | op(">>").map(_ => JSShr)),
            op("<=").map(_ => JSLe) | op("<").map(_ => JSLt)
          | op(">=").map(_ => JSGe) | op(">").map(_ => JSGt)),
            op("==").map(_ => JSEq) | op("!=").map(_ => JSNe))
       .rep(sep=op("&"), min=1).map(_.reduceLeft(JSBitAnd))
       .rep(sep=op("^"), min=1).map(_.reduceLeft(JSBitXor))
       .rep(sep=op("|"), min=1).map(_.reduceLeft(JSBitOr))
       .rep(sep=op("&&"), min=1).map(_.reduceLeft(JSAnd))
       .rep(sep=op("||"), min=1).map(_.reduceLeft(JSOr))
    )
    private lazy val condExpr = P(
        (_expr ~ ("?" ~/ asgn ~ ":" ~/ asgn).?).map
        {
            case (c, Some((t, e))) => JSCond(c, t, e)
            case (c, None) => c
        })
    private lazy val asgn: Parser[JSExpr_] = P(chainl1(spaces ~~ condExpr, op("=").map(_ => JSAsgn)))
    private lazy val expr: Parser[JSExpr] = P(asgn.rep(sep=",", min=1).map(_.toList))
    private val optExpr = expr.?
    private val parensExpr = "(" ~ expr ~ ")"
    private val variable = (identifier ~ ("=" ~/ asgn).?).map(JSVar.tupled)
    private val varsOrExprs = key("var") ~ variable.rep(sep=",", min=1).map(xs => Left(xs.toList)) | expr.map(Right(_))
    private lazy val stmt: Parser[JSStm] = P(
          P(";").map(_ => JSSemi)
        | key("if") ~/ (parensExpr ~ stmt ~ (key("else") ~/ stmt).?).map(JSIf.tupled)
        | key("while") ~/ (parensExpr ~ stmt).map(JSWhile.tupled)
        | key("for") ~/ "(" ~ (((varsOrExprs ~ key("in") ~/ expr).map{case (x, y) => JSForIn(x, y)(_)}
                            | (varsOrExprs.? ~ ";" ~/ optExpr ~/ ";" ~/ optExpr).map{case (x, y, z) => JSFor(x, y, z)(_)}) ~/ ")" ~/ stmt).map{case (f, x) => f(x)}
        | key("break").map(_ => JSBreak)
        | key("continue").map(_ => JSContinue)
        | key("with") ~/ (parensExpr ~ stmt).map(JSWith.tupled)
        | key("return") ~/ optExpr.map(JSReturn)
        | compound.map(JSBlock)
        | varsOrExprs.map(JSNaked))
    private lazy val compound = P("{" ~/ stmt.rep.map(_.toList) ~ "}")
    private val element = P(key("function") ~/ identifier ~/ "(" ~/ identifier.rep(sep=",".~/).map(_.toList) ~/ ")" ~/ compound).map(JSFunction.tupled) | stmt
    val javascript = spaces ~/ element.rep ~ spaces ~ End
}

// From the fastparse blog - straight from the horses mouth
private [parsley] object FastParseJson
{
    import fastparse.all._
    private val space         = P( CharsWhileIn(" \r\n").? )
    private val digits        = P( CharsWhileIn("0123456789"))
    private val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
    private val fractional    = P( "." ~ digits )
    private val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

    private val number        = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(_.toDouble)
    private val `null`        = P( "null" ).!
    private val `false`       = P( "false" ).!
    private val `true`        = P( "true" ).!
    private val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
    private val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
    private val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
    private val strChars      = P( CharsWhile(!"\"\\".contains(_: Char)) )
    private val string        = P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"")
    private val array         = P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(_.toList)
    private val pair          = P( string ~/ ":" ~/ jsonExpr )
    private val obj           = P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(_.toMap)

    lazy val jsonExpr: P[Any] = P(space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space)
}

private [parsley] object Atto
{
    import parsley.Native._
    import atto.parser._
    import atto.parser.combinator._
    import atto.parser.text._
    import atto.parser.character._
    import atto.syntax.parser._
    type Parser[A] = atto.Parser[A]
    private lazy val num = token(attempt(numeric.float).map(x => x: Any) | numeric.int.map(x => x: Any))
    private lazy val `null` = token(string("null")).asInstanceOf[Parser[Any]]
    private lazy val `true` = token(string("true")).asInstanceOf[Parser[Any]]
    private lazy val `false` = token(string("false")).asInstanceOf[Parser[Any]]
    private lazy val value: Parser[Any] = `null` | `true` | `false` | num | token(stringLiteral).asInstanceOf[Parser[Any]] | array | obj
    private lazy val obj = token(char('{')) ~> sepBy(pairBy(token(stringLiteral), token(char(':')), value), token(char(','))).map(_.toMap.asInstanceOf[Any]) <~ token(char('}'))
    private lazy val array = token(char('[')) ~> sepBy(value, token(char(','))).asInstanceOf[Parser[Any]] <~ token(char(']'))
    lazy val json: Parser[Any] = skipWhitespace ~> orElse(obj, array) <~ endOfInput
    def parseJson(input: String): Any = json.parseOnly(input)

    def chainPre[A](p: =>Parser[A], op: Parser[A => A]): Parser[A] = for (fs <- many(op); x <- p) yield fs.foldRight(x)((f, y) => f(y))
    def chainl1[A](p: =>Parser[A], op: Parser[(A, A) => A]): Parser[A] = for (x <- p; fs <- many(for (f <- op; y <- p) yield (x: A) => f(x, y))) yield fs.foldLeft(x)((y, f) => f(y))

    lazy val mexpr: Parser[Int] = chainl1(mmul, token(char('+')) >| ((x: Int, y: Int) => x + y) | token(char('-') >| ((x: Int, y: Int) => x - y)))
    lazy val mmul = chainl1(mdiv, token(char('*')) >| ((x: Int, y: Int) => x * y))
    lazy val mdiv = chainl1(mpow, token(char('/')) >| ((x: Int, y: Int) => x / y) | token(char('%') >| ((x: Int, y: Int) => x % y)))
    lazy val mpow = chainl1(msigns, token(char('^')) >| ((x: Int, y: Int) => scala.math.pow(x.toDouble, y.toDouble).toInt))
    lazy val msigns = chainPre(matom, token(char('+')) >| ((x: Int) => x) | token(char('-')) >| ((x: Int) => -x))
    lazy val matom = token(numeric.int) | (token(char('(')) ~> mexpr <~ token(char(')')))
    val math = skipWhitespace ~> mexpr <~ endOfInput
    def parseMath(input: String): Any = math.parseOnly(input)

    def brainfuck: Parser[List[BrainFuckOp]] =
    {
        val ws = skipMany(noneOf("<>+-.,[]"))
        def tok[A](p: Parser[A]): Parser[A] = p <~ ws
        lazy val bf: Parser[List[BrainFuckOp]] =
            many(choice[BrainFuckOp](
                tok(char('>')) >| RightPointer,
                tok(char('<')) >| LeftPointer,
                tok(char('+')) >| Increment,
                tok(char('-')) >| Decrement,
                tok(char('.')) >| Output,
                tok(char(',')) >| Input
              | (tok(char('[')) ~> (bf -| (xs => Loop(xs): BrainFuckOp))) <~ tok(char(']'))))
        (ws ~> bf <~ endOfInput) | err("] closes a loop but there isn't one open")
    }
    val bf = brainfuck
    def parseBrainfuck(input: String) = bf.parseOnly(input)
}

private [parsley] object Benchmark
{
    def read(filename: String) = Try(Source.fromFile(filename).getLines().mkString("\n") + "\n").getOrElse("")
    def parseParsley(p: Any, s: String): Any = runParserFastUnsafe(p.asInstanceOf[Parsley[_]], s)
    def parseFastParse(p: Any, s: String): Any = p.asInstanceOf[fastparse.all.Parser[_]].parse(s)
    def parseFunction(f: Any, s: String): Any = f.asInstanceOf[String => Any](s)

    val benchmarks: Array[(String, Any, (Any, String) => Any, Int)] =
        Array(
            /*0*/  ("inputs/helloworld_golfed.bf", ParsleyBench.brainfuck, parseParsley, 1000000),
            /*1*/  ("inputs/helloworld_golfed.bf", FastParseBrainfuck.parser, parseFastParse, 1000000),
            /*2*/  ("inputs/helloworld.bf", ParsleyBench.brainfuck, parseParsley, 1000000),
            /*3*/  ("inputs/helloworld.bf", FastParseBrainfuck.parser, parseFastParse, 1000000),
            /*4*/  ("inputs/arrays.nand", ParsleyBench.nand, parseParsley, 50000),
            /*5*/  ("inputs/arrays.nand", FastParseWhite.nand, parseFastParse, 50000),
            /*6*/  ("inputs/test.while", ParsleyBench.whileLang, parseParsley, 100000),
            /*7*/  ("inputs/fibonacci.js", ParsleyBench.javascript, parseParsley, 100000),
            /*8*/  ("inputs/mediumdata.json", ParsleyBench.json, parseParsley, 50000),
            /*9*/  ("inputs/mediumdata.json", FastParseJson.jsonExpr, parseFastParse, 50000),
            /*10*/ ("inputs/bigdata.json", ParsleyBench.json, parseParsley, 50000),
            /*11*/ ("inputs/bigdata.json", FastParseJson.jsonExpr, parseFastParse, 50000),
            /*12*/ ("inputs/hugedata.json", ParsleyBench.json, parseParsley, 2000),
            /*13*/ ("inputs/hugedata.json", FastParseJson.jsonExpr, parseFastParse, 2000),
            /*14*/ ("inputs/smalldata.json", ParsleyBench.json, parseParsley, 1000000),
            /*15*/ ("inputs/smalldata.json", FastParseJson.jsonExpr, parseFastParse, 1000000),
            /*16*/ ("inputs/heapsort.js", ParsleyBench.javascript, parseParsley, 100000),
            /*17*/ ("inputs/game.js", ParsleyBench.javascript, parseParsley, 100000),
            /*18*/ ("inputs/big.js", ParsleyBench.javascript, parseParsley, 1000),
            /*19*/ ("inputs/fibonacci.nand", ParsleyBench.nand, parseParsley, 100000),
            /*20*/ ("inputs/fibonacci.nand", FastParseWhite.nand, parseFastParse, 100000),
            /*21*/ ("inputs/fizzbuzz.nand", ParsleyBench.nand, parseParsley, 50000),
            /*22*/ ("inputs/fizzbuzz.nand", FastParseWhite.nand, parseFastParse, 50000),
            /*23*/ ("inputs/compiler.bf", ParsleyBench.brainfuck, parseParsley, 10000),
            /*24*/ ("inputs/compiler.bf", FastParseBrainfuck.parser, parseFastParse, 10000),
            /*25*/ ("inputs/bigequation.txt", ParsleyBench.maths, parseParsley, 2000000),
            /*26*/ ("inputs/bigequation.txt", ParsleyBench.maths_unsub, parseParsley, 2000000),
            /*27*/ ("inputs/bigequation.txt", FastParseWhite.math, parseFastParse, 2000000),
            /*28*/ ("inputs/bigequation.txt", Atto.parseMath _, parseFunction, 2000000),
            /*29*/ ("inputs/bigequation.txt", ScalaParserCombinatorsMath.apply _, parseFunction, 2000000),
            /*30*/ ("inputs/helloworld.bf", Atto.parseBrainfuck _, parseFunction, 20000),
            /*31*/ ("inputs/helloworld.bf", ScalaParserCombinatorsBrainFuck.apply _, parseFunction, 20000),
            /*32*/ ("inputs/heapsort.js", FastParseWhite.javascript, parseFastParse, 100000),
            /*33*/ ("inputs/game.js", FastParseWhite.javascript, parseFastParse, 100000),
            /*34*/ ("inputs/big.js", FastParseWhite.javascript, parseFastParse, 1000),
        )

    def main(args: Array[String]): Unit =
    {
        //Console.in.read()
        //val p = ParsleyBench.chain
        //val input = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"
        //val exec = runParserFastUnsafe _
        //new nandlang.NandLang().run(read("inputs/arrays.nand"))
        val nand = new nandlang.NandLang
        val (filename, p, exec, iters) = benchmarks(34)
        val input = read(filename)
        val start = System.currentTimeMillis()
        println(exec(p, input))
        //println(BenchParser.json.parseFull(input))
        //println(PfS.parseJson(input))
        for (_ <- 0 to iters) exec(p, input)
        println(System.currentTimeMillis() - start)
    }
}