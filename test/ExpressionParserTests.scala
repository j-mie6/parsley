import parsley.Combinator.{chainPost, chainPre, chainl1, chainr1}
import parsley.Parsley._
import parsley.{Failure, Success, runParser}
import parsley.Char.{charLift, stringLift, digit}

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
        runParser(chainr1("11" #> 1, '+' #> ((x: Int) => (y: Int) => x + y)), "11") should be (Success(1))
    }
    it must "parse all operators and values that follow" in
    {
        runParser(chainr1("11" #> 1, '+' #> ((x: Int) => (y: Int) => x + y)), "11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in
    {
        runParser(chainr1(digit.map(_.asDigit), '%' #> ((x: Int) => (y: Int) => x % y)), "6%5%2%7") should be (Success(0))
    }
    it must "fail if an operator or p fails after consuming input" in
    {
        runParser(chainr1("11" #> 1, "++" #> ((x: Int) => (y: Int) => x + y)), "11+11+11+11+11") shouldBe a [Failure]
        runParser(chainr1("11" #> 1, "++" #> ((x: Int) => (y: Int) => x + y)), "11++11++11++1++11") shouldBe a [Failure]
    }

    "chainl1" must "require an initial value" in
    {
        runParser(chainl1("11" #> 1, '+' #> ((x: Int) => (y: Int) => x + y)), "11") should be (Success(1))
    }
    it must "parse all operators and values that follow" in
    {
        runParser(chainl1("11" #> 1, '+' #> ((x: Int) => (y: Int) => x + y)), "11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in
    {
        runParser(chainl1(digit.map(_.asDigit), '%' #> ((x: Int) => (y: Int) => x % y)), "6%5%2%7") should be (Success(1))
    }
    it must "fail if an operator fails after consuming input" in
    {
        runParser(chainl1("11" #> 1, "++" #> ((x: Int) => (y: Int) => x + y)), "11+11+11+11+11") shouldBe a [Failure]
        runParser(chainl1("11" #> 1, "++" #> ((x: Int) => (y: Int) => x + y)), "11++11++11++1++11") shouldBe a [Failure]
    }

    "expression parsers" should "result in correct precedence" in pending
    they should "work for multiple operators at the same level" in pending
    they should "work for mixed associativity operators at same level" in pending
    they should "parse mathematical expressions" in pending
}
