package parsley

import parsley.Combinator._
import parsley.Parsley._
import parsley.Implicits.{charLift, stringLift}

import scala.language.implicitConversions

class CombinatorTests extends ParsleyTest {
    "choice" should "fail if given the empty list" in {
        choice().runParser("") shouldBe a [Failure]
    }
    it should "behave like p for List(p)" in {
        choice('a').runParser("") should equal ('a'.runParser(""))
        choice('a').runParser("a") should equal ('a'.runParser("a"))
    }
    it should "parse in order" in {
        choice("a", "b", "bc", "bcd").runParser("bcd") should be (Success("b"))
    }
    it should "fail if none of the parsers succeed" in {
        choice("a", "b", "bc", "bcd").runParser("c") shouldBe a [Failure]
    }

    "attemptChoice" should "correctly ensure the subparsers backtrack" in {
        attemptChoice("ac", "aba", "abc").runParser("abc") should be (Success("abc"))
    }

    "repeat" should "be pure(Nil) for n <= 0" in {
        repeat(0, 'a').runParser("a") should be (Success(Nil))
        repeat(-1, 'a').runParser("a") should be (Success(Nil))
    }
    it should "parse n times for n > 0" in {
        for (n <- 0 to 100) repeat(n, 'a').runParser("a"*n) should be (Success(("a" * n).toList))
    }
    it must "fail if n inputs are not present" in {
        repeat(2, 'a').runParser("a") shouldBe a [Failure]
    }

    "option" should "succeed with Some if p succeeds" in {
        option(pure(7)).runParser("") should be (Success(Some(7)))
    }
    it should "succeed with None if p fails without consumption" in {
        option('a').runParser("") should be (Success(None))
    }
    it should "fail if p fails with consumption" in {
        option("ab").runParser("a") shouldBe a [Failure]
    }

    "decide" must "succeed for Some" in {
        decide('a' <#> (Option(_))).runParser("a") should be (Success('a'))
    }
    it must "fail for None" in {
        decide(pure(None)).runParser("") shouldBe a [Failure]
    }
    it must "succeed for None with an alternative" in {
        decide(pure(None), pure(7)).runParser("") shouldBe Success(7)
    }
    it must "compose with option to become identity" in {
        decide(option(pure(7))).runParser("") should be (pure(7).runParser(""))
        decide(option('a')).runParser("") should be ('a'.runParser(""))
        decide(option("ab")).runParser("a") should be ("ab".runParser("a"))
    }

    "optional" must "succeed if p succeeds" in {
        optional('a').runParser("a") should be (Success(()))
    }
    it must "also succeed if p fails without consumption" in {
        optional('a').runParser("b") should be (Success(()))
    }
    it must "fail if p failed with consumption" in {
        optional("ab").runParser("a") shouldBe a [Failure]
    }

    "manyN" must "ensure that n are parsed" in {
        for (n <- 0 to 10) manyN(n, 'a').runParser("a"*n) should be (Success(("a"*n).toList))
        for (n <- 0 to 10) manyN(n+1, 'a').runParser("a"*n) shouldBe a [Failure]
    }
    it should "not care if more are present" in {
        for (n <- 0 to 10) manyN(n/2, 'a').runParser("a"*n) should be (Success(("a"*n).toList))
    }

    "skipManyN" must "ensure that n are parsed" in {
        for (n <- 0 to 10) skipManyN(n, 'a').runParser("a"*n) should be (Success(()))
        for (n <- 0 to 10) skipManyN(n+1, 'a').runParser("a"*n) shouldBe a [Failure]
    }
    it should "not care if more are present" in {
        for (n <- 0 to 10) skipManyN(n/2, 'a').runParser("a"*n) should be (Success(()))
    }

    "sepBy" must "accept empty input" in {
        sepBy('a', 'b').runParser("") should be (Success(Nil))
    }
    it must "not allow sep at the end of chain" in {
        sepBy('a', 'b').runParser("ab") shouldBe a [Failure]
    }
    it should "be able to parse 2 or more p" in {
        sepBy('a', 'b').runParser("aba") should be (Success(List('a', 'a')))
        sepBy('a', 'b').runParser("ababa") should be (Success(List('a', 'a', 'a')))
        sepBy('a', 'b').runParser("abababa") should be (Success(List('a', 'a', 'a', 'a')))
    }

    "sepBy1" must "require a p" in {
        sepBy1('a', 'b').runParser("a") should not be a [Failure]
        sepBy1('a', 'b').runParser(input = "") shouldBe a [Failure]
    }

    "sepEndBy" must "accept empty input" in {
        sepEndBy('a', 'b').runParser("") should be (Success(Nil))
    }
    it should "not require sep at the end of chain" in {
        sepEndBy('a', 'b').runParser("a") should be (Success(List('a')))
    }
    it should "be able to parse 2 or more p" in {
        sepEndBy('a', 'b').runParser("aba") should be (Success(List('a', 'a')))
        sepEndBy('a', 'b').runParser("ababa") should be (Success(List('a', 'a', 'a')))
    }
    it should "be able to parse a final sep" in {
        sepEndBy('a', 'b').runParser("ab") should be (Success(List('a')))
        sepEndBy('a', 'b').runParser("abab") should be (Success(List('a', 'a')))
        sepEndBy('a', 'b').runParser("ababab") should be (Success(List('a', 'a', 'a')))
    }
    it should "fail if p fails after consuming input" in {
        sepEndBy("aa", 'b').runParser("ab") shouldBe a [Failure]
    }
    it should "fail if sep fails after consuming input" in {
        sepEndBy('a', "bb").runParser("ab") shouldBe a [Failure]
    }
    it must "not corrupt the stack on sep hard-fail" in {
        ('c' <::> attempt(sepEndBy('a', "bb")).getOrElse(List('d'))).runParser("cab") should be (Success(List('c', 'd')))
    }

    "sepEndBy1" must "require a p" in {
        sepEndBy1('a', 'b').runParser("a") should not be a [Failure]
        sepEndBy1('a', 'b').runParser(input = "") shouldBe a [Failure]
    }

    "endBy" must "accept empty input" in {
        endBy('a', 'b').runParser("") should be (Success(Nil))
    }
    it must "require sep at end of chain" in {
        endBy('a', 'b').runParser("a") shouldBe a [Failure]
        endBy('a', 'b').runParser("ab") should be (Success(List('a')))
    }
    it should "be able to parse 2 or more p" in {
        endBy('a', 'b').runParser("abab") should be (Success(List('a', 'a')))
        endBy('a', 'b').runParser("ababab") should be (Success(List('a', 'a', 'a')))
    }

    "endBy1" must "require a p" in {
        endBy1('a', 'b').runParser("ab") should not be a [Failure]
        endBy1('a', 'b').runParser(input = "") shouldBe a [Failure]
    }

    "eof" must "succeed at the end of input" in {
        eof.runParser("") should not be a [Failure]
    }
    it must "fail if input remains" in {
        eof.runParser("a") shouldBe a [Failure]
    }

    "notFollowedBy" must "succeed if p fails" in {
        notFollowedBy('a').runParser("") should be (Success(()))
    }
    it must "succeed even if p consumed input" in {
        notFollowedBy("aa").runParser("a") should be (Success(()))
    }
    it must "fail if p succeeds" in {
        notFollowedBy('a').runParser("a") shouldBe a [Failure]
    }

    "manyUntil" must "require an end" in {
        manyUntil('a', 'b').runParser("aa") shouldBe a [Failure]
        manyUntil('a', 'b').runParser("ab") should be (Success(List('a')))
    }
    it should "parse the end without result" in {
        manyUntil('a', 'b').runParser("b") should be (Success(Nil))
    }
    it should "parse p until that end is found" in {
        manyUntil('a', 'b').runParser("aaaaaaaaaaaab") should not be a [Failure]
        manyUntil("aa", 'b').runParser("ab") shouldBe a [Failure]
    }

    "someUntil" must "parse at least 1 p" in {
        someUntil('a', 'b').runParser("ab") should be (Success(List('a')))
        someUntil('a', 'b').runParser("b") shouldBe a [Failure]
    }

    /*"forP" should "be able to parse context-sensitive grammars" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val abc = put(r1, 0) *>
                  many('a' *> modify[Int](r1, _ + 1)) *>
                  forP[Int](r2, get(r1), pure(_ != 0), pure(_ - 1), 'b') *>
                  forP[Int](r2, get(r1), pure(_ != 0), pure(_ - 1), 'c')
        abc.runParser("aaabbbccc") should be (Success(()))
        abc.runParser("aaaabc") shouldBe a [Failure]
    }*/
}
