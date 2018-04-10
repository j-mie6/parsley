package parsley

import scala.annotation.tailrec

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
        lazy val exprlist1 = tok.commaSep1(expr)
        val varlist = tok.commaSep(variable)
        val varlist1 = tok.commaSep1(variable)
        val funcparam = varlist <~> (tok.symbol(':') *> varlist).getOrElse(Nil)
        val varstmt = lift2(NandVar, optional(tok.keyword("var")) *> varlist1, tok.symbol('=') *> exprlist1 <* tok.semi)
        lazy val ifstmt = lift3(NandIf, tok.keyword("if") *> expr, block, option(block))
        lazy val whilestmt = tok.keyword("while") *> lift2(NandWhile, expr, block)
        lazy val statement = ifstmt <|> whilestmt <|> attempt(varstmt) <|> (expr.map(NandNaked) <* tok.semi)
        lazy val block: Parsley[NandBlock] = tok.braces(many(statement)).map(NandBlock)
        val funcdef = lift3(NandFunc, tok.keyword("function") *> identifier, tok.parens(funcparam), block)
        tok.whiteSpace *> many(funcdef) <* eof
    }
    val start = System.currentTimeMillis()
    println(nand.pretty)
    println(System.currentTimeMillis() - start)
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

/*private [parsley] object FastParser
{
    import fastparse.all._
    val x = P("1").!.map(_(0).toInt)
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
    val big = repeat(P("1"), 5000)

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
}*/

private [parsley] object Benchmark
{
    def main(args: Array[String]): Unit =
    {
        //Console.in.read()
        val p = ParsleyBench.nand
        val input1 = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"
        val input2 = "[+++++++<[<<>>>>]..hahah this is brainfuck.,,,,,-[---]-++]"
        val input3 =
            """// not logic gate
              |function not(in : out) {
              |    out = in ! in;
              |}
              |
              |// and logic gate
              |function and(a, b : out) {
              |    out = not(a ! b);
              |}
              |
              |// or logic gate
              |function or(a, b : out) {
              |    out = not(a) ! not(b);
              |}
              |
              |// xor logic gate
              |function xor(a, b : out) {
              |    out = or(and(a, not(b)), and(not(a), b));
              |}
              |
              |// returns true if a and b are equal
              |function eq(a, b : out) {
              |    out = not(xor(a, b));
              |}
              |
              |// full adder
              |function add(a, b, cin : v, cout) {
              |    v = xor(cin, xor(a, b));
              |    cout = or(and(a, b), and(xor(a, b), cin));
              |}
              |
              |// 8 bit adder
              |function add8(a[8], b[8] : o[8]) {
              |    var c = 0;
              |    o[7], c = add(a[7], b[7], c);
              |    o[6], c = add(a[6], b[6], c);
              |    o[5], c = add(a[5], b[5], c);
              |    o[4], c = add(a[4], b[4], c);
              |    o[3], c = add(a[3], b[3], c);
              |    o[2], c = add(a[2], b[2], c);
              |    o[1], c = add(a[1], b[1], c);
              |    o[0], c = add(a[0], b[0], c);
              |}
              |
              |// returns the two's complement of the given integer
              |function complement8(i[8] : o[8]) {
              |    o = add8(
              |        not(i[0]), not(i[1]), not(i[2]), not(i[3]),
              |        not(i[4]), not(i[5]), not(i[6]), not(i[7]),
              |        0, 0, 0, 0, 0, 0, 0, 1);
              |}
              |
              |// 8 bit subtraction
              |function sub8(a[8], b[8] : o[8]) {
              |    o = add8(a, complement8(b));
              |}
              |
              |// 8 bit equality
              |function equal8(a[8], b[8] : out) {
              |    out = and(
              |        and(and(eq(a[1], b[1]), eq(a[2], b[2])),
              |            and(eq(a[3], b[3]), eq(a[4], b[4]))),
              |        and(and(eq(a[5], b[5]), eq(a[6], b[6])),
              |            and(eq(a[7], b[7]), eq(a[0], b[0]))));
              |}
              |
              |// returns the Fibonacci number for the given input
              |function fibonacci(i[8] : o[8]) {
              |    var check[7], _ = i;
              |    var is_equal = equal8(check,0, 0,0,0,0,0,0,0,0);
              |    if is_equal {
              |        // return input if equal to 1 or 0
              |        o = i;
              |    }
              |    if not(is_equal) {
              |        // o = fibonacci(i - 1) + fibonacci(i - 2);
              |        o = add8(
              |            fibonacci(sub8(i, 0,0,0,0,0,0,0,1)),
              |            fibonacci(sub8(i, 0,0,0,0,0,0,1,0))
              |        );
              |    }
              |}
              |
              |function main()
              |{
              |    var value[8] = 0,0,0,0,0,0,0,0;
              |    while not(equal8(value, 0,0,0,0,1,1,1,0)) {
              |        // to output strings multiple individual putc calls are needed
              |        putc('F');
              |        putc('i');
              |        putc('b');
              |        putc(' ');
              |        puti8(value);
              |        putc(' ');
              |        putc('=');
              |        putc(' ');
              |        puti8(fibonacci(value));
              |        endl();
              |        // increment
              |        value = add8(value, 0,0,0,0,0,0,0,1);
              |    }
              |}
            """.stripMargin
        val input = input3
        //println(runParser(p, "aaaab"))
        //println(runParser(p, "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"))
        println(runParserFastUnsafe(p, input))
        val start = System.currentTimeMillis()
        //println(runParser(p, input))
        for (_ <- 0 to 100000)
            runParserFastUnsafe(p, input)
            //p(input)
            //p.parse(input)
        println(System.currentTimeMillis() - start)
    }
}