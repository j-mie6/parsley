import parsley.{Failure, Success, runParser}
import parsley.Combinator._
import parsley.Char.{charLift, char, string, stringLift}
import parsley.Parsley._

class CombinatorTests extends ParsleyTest
{
    "choice" should "fail if given the empty list" in
    {
        runParser(choice(Nil), "") shouldBe a [Failure]
    }
    it should "behave like p for List(p)" in
    {
        runParser(choice(List(char('a'))), "") should equal (runParser('a', ""))
        runParser(choice(List(char('a'))), "a") should equal (runParser('a', "a"))
    }
    it should "parse in order" in
    {
        runParser(choice(List(string("a"), string("b"), string("bc"), string("bcd"))), "bcd") should be (Success("b"))
    }
    it should "fail if none of the parsers succeed" in
    {
        runParser(choice(List(string("a"), string("b"), string("bc"), string("bcd"))), "c") shouldBe a [Failure]
    }

    "repeat" should "be pure(Nil) for n <= 0" in
    {
        runParser(repeat(0, 'a'), "a") should be (Success(Nil))
        runParser(repeat(-1, 'a'), "a") should be (Success(Nil))
    }
    it should "parse n times for n > 0" in
    {
        for (n <- 0 to 100) runParser(repeat(n, 'a'), "a"*n) should be (Success(("a" * n).toList))
    }
    it must "fail if n inputs are not present" in
    {
        runParser(repeat(2, 'a'), "a") shouldBe a [Failure]
    }

    "option" should "succeed with Some if p succeeds" in
    {
        runParser(option(pure(7)), "") should be (Success(Some(7)))
    }
    it should "succeed with None if p fails without consumption" in
    {
        runParser(option('a'), "") should be (Success(None))
    }
    it should "fail if p fails with consumption" in
    {
        runParser(option("ab"), "a") shouldBe a [Failure]
    }

    "decide" must "succeed for Some" in
    {
        runParser(decide('a' <#> (Option(_))), "a") should be (Success('a'))
    }
    it must "fail for None" in
    {
        runParser(decide(pure(None)), "") shouldBe a [Failure]
    }
    it must "compose with option to become identity" in
    {
        runParser(decide(option(pure(7))), "") should be (runParser(pure(7), ""))
        runParser(decide(option('a')), "") should be (runParser('a', ""))
        runParser(decide(option("ab")), "a") should be (runParser("ab", "a"))
    }

    "optional" must "succeed if p succeeds" in
    {
        runParser(optional('a'), "a") should be (Success(()))
    }
    it must "also succeed if p fails without consumption" in
    {
        runParser(optional('a'), "b") should be (Success(()))
    }
    it must "fail if p failed with consumption" in
    {
        runParser(optional("ab"), "a") shouldBe a [Failure]
    }

    "manyN" must "ensure that n are parsed" in
    {
        for (n <- 0 to 10) runParser(manyN(n, 'a'), "a"*n) should be (Success(("a"*n).toList))
        for (n <- 0 to 10) runParser(manyN(n+1, 'a'), "a"*n) shouldBe a [Failure]
    }
    it should "not care if more are present" in
    {
        for (n <- 0 to 10) runParser(manyN(n/2, 'a'), "a"*n) should be (Success(("a"*n).toList))
    }

    "skipManyN" must "ensure that n are parsed" in
    {
        for (n <- 0 to 10) runParser(skipManyN(n, 'a'), "a"*n) should be (Success(()))
        for (n <- 0 to 10) runParser(skipManyN(n+1, 'a'), "a"*n) shouldBe a [Failure]
    }
    it should "not care if more are present" in
    {
        for (n <- 0 to 10) runParser(skipManyN(n/2, 'a'), "a"*n) should be (Success(()))
    }

    "sepBy" must "accept empty input" in
    {
        runParser(sepBy('a', 'b'), "") should be (Success(Nil))
    }
    it must "not allow sep at the end of chain" in
    {
        runParser(sepBy('a', 'b'), "ab") shouldBe a [Failure]
    }
    it should "be able to parse 2 or more p" in
    {
        runParser(sepBy('a', 'b'), "aba") should be (Success(List('a', 'a')))
        runParser(sepBy('a', 'b'), "ababa") should be (Success(List('a', 'a', 'a')))
        runParser(sepBy('a', 'b'), "abababa") should be (Success(List('a', 'a', 'a', 'a')))
    }

    "sepBy1" must "require a p" in
    {
        runParser(sepBy1('a', 'b'), "a") should not be a [Failure]
        runParser(sepBy1('a', 'b'), input = "") shouldBe a [Failure]
    }

    "sepEndBy" must "accept empty input" in
    {
        runParser(sepEndBy('a', 'b'), "") should be (Success(Nil))
    }
    it should "not require sep at the end of chain" in
    {
        runParser(sepEndBy('a', 'b'), "a") should be (Success(List('a')))
    }
    it should "be able to parse 2 or more p" in
    {
        runParser(sepEndBy('a', 'b'), "aba") should be (Success(List('a', 'a')))
        runParser(sepEndBy('a', 'b'), "ababa") should be (Success(List('a', 'a', 'a')))
    }
    it should "be able to parse a final sep" in
    {
        runParser(sepEndBy('a', 'b'), "ab") should be (Success(List('a')))
        runParser(sepEndBy('a', 'b'), "abab") should be (Success(List('a', 'a')))
        runParser(sepEndBy('a', 'b'), "ababab") should be (Success(List('a', 'a', 'a')))
    }

    "sepEndBy1" must "require a p" in
    {
        runParser(sepEndBy1('a', 'b'), "a") should not be a [Failure]
        runParser(sepEndBy1('a', 'b'), input = "") shouldBe a [Failure]
    }

    "endBy" must "accept empty input" in
    {
        runParser(endBy('a', 'b'), "") should be (Success(Nil))
    }
    it must "require sep at end of chain" in
    {
        runParser(endBy('a', 'b'), "a") shouldBe a [Failure]
        runParser(endBy('a', 'b'), "ab") should be (Success(List('a')))
    }
    it should "be able to parse 2 or more p" in
    {
        runParser(endBy('a', 'b'), "abab") should be (Success(List('a', 'a')))
        runParser(endBy('a', 'b'), "ababab") should be (Success(List('a', 'a', 'a')))
    }

    "endBy1" must "require a p" in
    {
        runParser(endBy1('a', 'b'), "ab") should not be a [Failure]
        runParser(endBy1('a', 'b'), input = "") shouldBe a [Failure]
    }

    "eof" must "succeed at the end of input" in
    {
        runParser(eof, "") should not be a [Failure]
    }
    it must "fail if input remains" in
    {
        runParser(eof, "a") shouldBe a [Failure]
    }

    "notFollowedBy" must "succeed if p fails" in
    {
        runParser(notFollowedBy('a'), "") should be (Success(()))
    }
    it must "succeed even if p consumed input" in
    {
        runParser(notFollowedBy("aa"), "a") should be (Success(()))
    }
    it must "fail if p succeeds" in
    {
        runParser(notFollowedBy('a'), "a") shouldBe a [Failure]
    }

    "manyTill" must "require an end" in
    {
        runParser(manyTill('a', 'b'), "aa") shouldBe a [Failure]
        runParser(manyTill('a', 'b'), "ab") should be (Success(List('a')))
    }
    it should "parse the end without result" in
    {
        runParser(manyTill('a', 'b'), "b") should be (Success(Nil))
    }
    it should "parse p until that end is found" in
    {
        runParser(manyTill('a', 'b'), "aaaaaaaaaaaab") should not be a [Failure]
        runParser(manyTill("aa", 'b'), "ab") shouldBe a [Failure]
    }

    "many1Till" must "parse at least 1 p" in
    {
        runParser(many1Till('a', 'b'), "ab") should be (Success(List('a')))
        runParser(many1Till('a', 'b'), "b") shouldBe a [Failure]
    }
}
