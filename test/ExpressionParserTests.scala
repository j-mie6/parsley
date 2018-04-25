import parsley.Combinator.{chainPost, chainPre, chainl1, chainr1}
import parsley.Parsley._
import parsley._
import parsley.Char.{charLift, digit, stringLift}
import parsley.ExpressionParser.{AssocLeft, AssocRight, Infixes, Prefixes}

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

    "chainPre" must "parse an operatorless value" in
    {
        runParser(chainPre('1' #> 1, '+' #> ((x: Int) => x + 1)), "1") should be (Success(1))
    }
    it must "parse all operators that precede a value" in
    {
        runParser(chainPre('1' #> 1, '+' #> ((x: Int) => x + 1)), "+++++++++++1") should not be a [Failure]
    }
    it must "fail if the final value is absent" in
    {
        runParser(chainPre('1' #> 1, '+' #> ((x: Int) => x + 1)), "+++++++++++") shouldBe a [Failure]
    }
    it must "apply the functions" in
    {
        runParser(chainPre('1' #> 1, '+' #> ((x: Int) => x + 1)), "+++++++++++1") should be (Success(12))
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

    "expression parsers" should "result in correct precedence" in
    {
        val expr = new ExpressionParser[Int](List(Infixes[Int](List('*' #> (_*_)), AssocLeft),
                                                  Infixes[Int](List('+' #> (_+_)), AssocLeft)), digit.map(_.asDigit))
        runParser(expr.expr, "1+2*3+4") should be (Success(11))
        runParser(expr.expr, "1*2+3*4") should be (Success(14))
    }
    they should "work for multiple operators at the same level" in
    {
        val expr = new ExpressionParser[Int](List(Infixes[Int](List('+' #> (_+_), '-' #> (_-_)), AssocLeft)), digit.map(_.asDigit))
        runParser(expr.expr, "1+2-3+4") should be (Success(4))
        runParser(expr.expr, "1-2+3-4") should be (Success(-2))
    }
    they should "work for mixed associativity operators" in
    {
        val expr = new ExpressionParser[Int](List(Infixes[Int](List('*' #> (_*_)), AssocLeft),
                                                  Infixes[Int](List('+' #> (_+_)), AssocRight)), digit.map(_.asDigit))
        runParser(expr.expr, "1+2*3+4") should be (Success(11))
        runParser(expr.expr, "1*2+3*4") should be (Success(14))
    }
    they should "parse mathematical expressions" in
    {
        lazy val expr = new ExpressionParser[Int](
            List(Prefixes[Int](List('-' #> (x => -x))),
                 Infixes[Int](List('/' #> (_/_)), AssocLeft),
                 Infixes[Int](List('*' #> (_*_)), AssocLeft),
                 Infixes[Int](List('+' #> (_+_), '-' #> (_-_)), AssocLeft)), atom)
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr.expr <* ')')
        runParser(expr.expr, "(2+3)*8") should be (Success(40))
        runParser(expr.expr, "-3+4") should be (Success(1))
        runParser(expr.expr, "-(3+4)") should be (Success(-7))
        runParser(expr.expr, "(3+-7)*(-2--4)/2") should be (Success(-4))
    }
    // TODO More tests, stuff like ri && !r[--ri] from js parser is not working, ++x < 10 will crash the intrinsic too!
}
