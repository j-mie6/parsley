import parsley.Char.digit
import parsley.Implicits.{charLift, stringLift}
import parsley.Combinator.{chainPost, chainPre, chainl1, chainr1}
import parsley.ExpressionParser._
import parsley.Parsley._
import parsley._

class ExpressionParserTests extends ParsleyTest
{
    "chainPost" must "require an initial value" in
    {
        runParser(chainPost('1' #> 1, '+' #> ((x: Int) => x + 1)), "1") should be (Success(1))
    }
    it must "parse all operators that follow" in
    {
        runParser(chainPost('1' #> 1, '+' #> ((x: Int) => x + 1)), "1++++++++++++++") should not be a [Failure]
    }
    it must "apply the functions" in
    {
        runParser(chainPost('1' #> 1, '+' #> ((x: Int) => x + 1)), "1++++++++++++++") should be (Success(15))
    }
    it must "fail if an operator fails after consuming input" in
    {
        runParser(chainPost('1' #> 1, "++" #> ((x: Int) => x + 1)), "1+++++++++++++") shouldBe a [Failure]
    }
    it must "not leave the stack in an inconsistent state on failure" in
    {
        val p = chainPost[Int]('1' #> 1, (col #>[Int => Int] (_ + 1)) <* '+')
        val q = chainl1[Int](p, '*' #> (_ * _))
        noException should be thrownBy println(runParser(q, "1+*1+"))
    }

    "chainPre" must "parse an operatorless value" in
    {
        runParser(chainPre('+' #> ((x: Int) => x + 1), '1' #> 1), "1") should be (Success(1))
    }
    it must "parse all operators that precede a value" in
    {
        runParser(chainPre('+' #> ((x: Int) => x + 1), '1' #> 1), "+++++++++++1") should not be a [Failure]
    }
    it must "fail if the final value is absent" in
    {
        runParser(chainPre('+' #> ((x: Int) => x + 1), '1' #> 1), "+++++++++++") shouldBe a [Failure]
    }
    it must "apply the functions" in
    {
        runParser(chainPre('+' #> ((x: Int) => x + 1), '1' #> 1), "+++++++++++1") should be (Success(12))
    }

    "chainr1" must "require an initial value" in
    {
        runParser(chainr1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)), "11") should be (Success(1))
    }
    it must "parse all operators and values that follow" in
    {
        runParser(chainr1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)), "11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in
    {
        runParser(chainr1(digit.map(_.asDigit), '%' #> ((x: Int, y: Int) => x % y)), "6%5%2%7") should be (Success(0))
    }
    it must "fail if an operator or p fails after consuming input" in
    {
        runParser(chainr1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)), "11+11+11+11+11") shouldBe a [Failure]
        runParser(chainr1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)), "11++11++11++1++11") shouldBe a [Failure]
    }

    "chainl1" must "require an initial value" in
    {
        runParser(chainl1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)), "11") should be (Success(1))
    }
    it must "parse all operators and values that follow" in
    {
        runParser(chainl1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)), "11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in
    {
        runParser(chainl1(digit.map(_.asDigit), '%' #> ((x: Int, y: Int) => x % y)), "6%5%2%7") should be (Success(1))
    }
    it must "fail if an operator fails after consuming input" in
    {
        runParser(chainl1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)), "11+11+11+11+11") shouldBe a [Failure]
        runParser(chainl1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)), "11++11++11++1++11") shouldBe a [Failure]
    }
    it must "not leave the stack in an inconsistent state on failure" in
    {
        val p = chainl1[Int]('1' #> 1, (col #>[(Int, Int) => Int] (_ + _)) <* '+')
        val q = chainl1[Int](p, '*' #> (_ * _))
        noException should be thrownBy println(runParser(q, "1+1*1+1"))
    }

    "expression parsers" should "result in correct precedence" in
    {
        val expr = new ExpressionParser[Int](digit.map(_.asDigit), Infixes[Int](AssocLeft, '*' #> (_*_)),
                                                                   Infixes[Int](AssocLeft, '+' #> (_+_)))
        runParser(expr.expr, "1+2*3+4") should be (Success(11))
        runParser(expr.expr, "1*2+3*4") should be (Success(14))
    }
    they should "work for multiple operators at the same level" in
    {
        val expr = new ExpressionParser[Int](digit.map(_.asDigit), Infixes[Int](AssocLeft, '+' #> (_+_), '-' #> (_-_)))
        runParser(expr.expr, "1+2-3+4") should be (Success(4))
        runParser(expr.expr, "1-2+3-4") should be (Success(-2))
    }
    they should "work for mixed associativity operators" in
    {
        val expr = new ExpressionParser[Int](digit.map(_.asDigit), Infixes[Int](AssocLeft,'*' #> (_*_)),
                                                                   Infixes[Int](AssocRight,'+' #> (_+_)))
        runParser(expr.expr, "1+2*3+4") should be (Success(11))
        runParser(expr.expr, "1*2+3*4") should be (Success(14))
    }
    they should "parse mathematical expressions" in
    {
        lazy val expr = new ExpressionParser[Int](atom,
            Prefixes[Int]('-' #> (x => -x)),
            Infixes[Int](AssocLeft, '/' #> (_/_)),
            Infixes[Int](AssocLeft, '*' #> (_*_)),
            Infixes[Int](AssocLeft, '+' #> (_+_), '-' #> (_-_)))
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr.expr <* ')')
        runParser(expr.expr, "(2+3)*8") should be (Success(40))
        runParser(expr.expr, "-3+4") should be (Success(1))
        runParser(expr.expr, "-(3+4)") should be (Success(-7))
        runParser(expr.expr, "(3+-7)*(-2--4)/2") should be (Success(-4))
    }
    they should "parse prefix operators mixed with infix operators" in
    {
        lazy val expr = new ExpressionParser[Int](atom, Prefixes[Int]('-' #> (x => -x)),
                                                        Infixes[Int](AssocLeft, '-' #> (_-_)))
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr.expr <* ')')
        runParser(expr.expr, "-1") should be (Success(-1))
        runParser(expr.expr, "2-1") should be (Success(1))
        runParser(expr.expr, "-2-1") should be (Success(-3))
        runParser(expr.expr, "-(2-1)") should be (Success(-1))
        runParser(expr.expr, "(-0)-1") should be (Success(-1))
    }
    they should "be able to parse prefix operators weaker than an infix" in
    {
        sealed trait Expr
        case class Lt(x: Expr, y: Expr) extends Expr
        case class Inc(x: Expr) extends Expr
        case class Num(x: Int) extends Expr
        val expr = new ExpressionParser[Expr](digit.map(_.asDigit).map(Num), Infixes[Expr](AssocLeft, '<' #> Lt),
                                                                             Prefixes("++" #> Inc))
        runParser(expr.expr, "++1<2") should be (Success(Inc(Lt(Num(1), Num(2)))))
    }

    "mixed expressions" should "also be parsable" in
    {
        val lang = LanguageDef.plain.copy(
            identStart     = Predicate(_.isLetter),
            identLetter    = Predicate(_.isLetter)
        )

        sealed trait Expr
        case class Binary(l: Expr, r: Expr) extends Expr
        case class Unary(c: Expr) extends Expr
        case class Constant(x: String) extends Expr

        val tok = new TokenParser(lang)

        lazy val ops: List[OpList[Expr]] = List(
            Postfixes(tok.parens(expr </> Constant("")) <#> (e1 => (e2: Expr) => Binary(e2, e1))),
            Infixes(AssocLeft, '.' #> Binary),
            Infixes(AssocRight, ".=" #> Binary),
            Infixes(AssocRight, ',' #> Binary)
        )

        lazy val atom: Parsley[Expr] = tok.identifier.map(Constant)
        lazy val expr: Parsley[Expr] = new ExpressionParser(atom, ops: _*).expr

        runParser(expr, "o.f()") shouldBe a [Success[_]]
        runParser(expr, "o.f(x,y)") shouldBe a [Success[_]]
    }
}
