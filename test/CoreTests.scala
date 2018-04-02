import parsley.{Failure, Parsley, Success, runParser}
import parsley.Parsley._
import parsley.Char.charLift

class CoreTests extends ParsleyTest
{
    private val add: (Int, Int) => Int = _+_
    private val add1 = (x: Int) => x + 1
    private val mult5 = (x: Int) => x * 5
    private val toUpper = (c: Char) => c.toUpper

    "Character parsers" should "fail on no input" in
    {
        runParser('a', "") should not be a [Success[_]]
    }

    they should "fail when given incorrect input" in
    {
        runParser('a', "b") should not be a [Success[_]]
    }

    they should "succeed when given correct input" in
    {
        runParser('a', "a") should be (Success('a'))
    }

    they must "only consume a single character of input at most" in
    {
        var res = runParser('a' *> 'b', "aaaaaa")
        res shouldBe a [Failure]
        res match
        {
            case Failure(err) => err should startWith ("(line 1, column 2)")
            case _ =>
        }
        res = runParser('a' *> 'b', "bc")
        res shouldBe a [Failure]
        res match
        {
            case Failure(err) => err should startWith ("(line 1, column 1)")
            case _ =>
        }
    }

    "Pure parsers" should "not require input" in
    {
        runParser(unit, "") should not be a [Failure]
    }

    they must "result in their correct value" in
    {
        runParser(pure(42), "") should be (Success(42))
        runParser(pure("hello world"), "") should be (Success("hello world"))
        runParser(pure(add), "") should be (Success(add))
    }

    they must "not consume any input" in
    {
        runParser(pure('a') *> 'a', "a") should be (Success('a'))
    }

    // APPLICATIVE LAWS
    they must "obey the homomorphism law: pure f <*> pure x = pure (f x)" in
    {
        runParser(pure(add1) <*> pure(42), "") should be (Success(43))
    }

    they must "obey the fmap law: pure f <*> p = f <$> p" in
    {
        runParser(pure(toUpper) <*> 'a', "a") should equal (runParser(toUpper <#> 'a', "a"))
        runParser(pure(toUpper) <*> ('a' <|> 'b'), "a") should equal (runParser(toUpper <#> ('a' <|> 'b'), "a"))
        runParser(pure(toUpper) <*> ('a' <|> 'b'), "b") should equal (runParser(toUpper <#> ('a' <|> 'b'), "b"))
    }

    they must "obey the interchange law: u <*> pure x = ($x) <$> u" in
    {
        runParser(('a' #> add1) <*> pure(41), "a") should equal (runParser(((f: Int => Int) => f(41)) <#> ('a' #> add1), "a"))
        runParser((('a' <|> 'b') #> add1) <*> pure(41), "a") should equal (runParser(((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1), "a"))
        runParser((('a' <|> 'b') #> add1) <*> pure(41), "b") should equal (runParser(((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1), "b"))
    }

    they must "obey the composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" in
    {
        val u: Parsley[Int => Int] = 'a' #> add1
        val v: Parsley[Int => Int] = 'b' #> mult5
        val w: Parsley[Int] = 'c' #> 7
        val compose: (Int => Int) => (Int => Int) => (Int => Int) = f => g => f.compose(g)
        runParser(pure(compose) <*> u <*> v <*> w, "abc") should equal (runParser(u <*> (v <*> w), "abc"))
    }

    // MONAD LAWS
    they must "obey the left identity law: pure x >>= f = f x" in pending
    "Parsers" must "obey the right identity law: m >>= pure = m" in pending
    they must "obey the associativity law: (m >>= f) >>= g = m >>= (x => f x >>= g)" in pending

    "mzero parsers" should "always fail" in pending

    "<|>" should "not try the second alternative if the first succeeded" in pending
    it should "only try second alternative if the first failed without consuming input" in pending
    it should "not try the second alternative if the first failed after consuming input" in pending

    "attempt" should "cause <|> to try second alternative even if input consumed" in pending
    "lookAhead" should "consume no input on success" in pending
    it must "fail when input is consumed, and input is consumed" in pending
}
