package parsley

import parsley.combinator._
import parsley.Parsley._
import parsley.implicits.{charLift, stringLift}

import scala.language.implicitConversions

class CombinatorTests extends ParsleyTest {
    "choice" should "fail if given the empty list" in {
        choice().parse("") shouldBe a [Failure]
    }
    it should "behave like p for List(p)" in {
        choice('a').parse("") should equal ('a'.parse(""))
        choice('a').parse("a") should equal ('a'.parse("a"))
    }
    it should "parse in order" in {
        choice("a", "b", "bc", "bcd").parse("bcd") should be (Success("b"))
    }
    it should "fail if none of the parsers succeed" in {
        choice("a", "b", "bc", "bcd").parse("c") shouldBe a [Failure]
    }

    "attemptChoice" should "correctly ensure the subparsers backtrack" in {
        attemptChoice("ac", "aba", "abc").parse("abc") should be (Success("abc"))
    }

    "repeat" should "be pure(Nil) for n <= 0" in {
        repeat(0, 'a').parse("a") should be (Success(Nil))
        repeat(-1, 'a').parse("a") should be (Success(Nil))
    }
    it should "parse n times for n > 0" in {
        for (n <- 0 to 100) repeat(n, 'a').parse("a"*n) should be (Success(("a" * n).toList))
    }
    it must "fail if n inputs are not present" in {
        repeat(2, 'a').parse("a") shouldBe a [Failure]
    }

    "option" should "succeed with Some if p succeeds" in {
        option(pure(7)).parse("") should be (Success(Some(7)))
    }
    it should "succeed with None if p fails without consumption" in {
        option('a').parse("") should be (Success(None))
    }
    it should "fail if p fails with consumption" in {
        option("ab").parse("a") shouldBe a [Failure]
    }

    "decide" must "succeed for Some" in {
        decide('a'.map(Option(_))).parse("a") should be (Success('a'))
    }
    it must "fail for None" in {
        decide(pure(None)).parse("") shouldBe a [Failure]
    }
    it must "succeed for None with an alternative" in {
        decide(pure(None), pure(7)).parse("") shouldBe Success(7)
    }
    it must "compose with option to become identity" in {
        decide(option(pure(7))).parse("") should be (pure(7).parse(""))
        decide(option('a')).parse("") shouldBe a [Failure]
        'a'.parse("") shouldBe a [Failure]
        decide(option("ab")).parse("a") shouldBe a [Failure]
        "ab".parse("a") shouldBe a [Failure]
    }

    "optional" must "succeed if p succeeds" in {
        optional('a').parse("a") should be (Success(()))
    }
    it must "also succeed if p fails without consumption" in {
        optional('a').parse("b") should be (Success(()))
    }
    it must "fail if p failed with consumption" in {
        optional("ab").parse("a") shouldBe a [Failure]
    }

    "manyN" must "ensure that n are parsed" in {
        for (n <- 0 to 10) manyN(n, 'a').parse("a"*n) should be (Success(("a"*n).toList))
        for (n <- 0 to 10) manyN(n+1, 'a').parse("a"*n) shouldBe a [Failure]
    }
    it should "not care if more are present" in {
        for (n <- 0 to 10) manyN(n/2, 'a').parse("a"*n) should be (Success(("a"*n).toList))
    }

    "skipManyN" must "ensure that n are parsed" in {
        for (n <- 0 to 10) skipManyN(n, 'a').parse("a"*n) should be (Success(()))
        for (n <- 0 to 10) skipManyN(n+1, 'a').parse("a"*n) shouldBe a [Failure]
    }
    it should "not care if more are present" in {
        for (n <- 0 to 10) skipManyN(n/2, 'a').parse("a"*n) should be (Success(()))
    }

    "sepBy" must "accept empty input" in {
        sepBy('a', 'b').parse("") should be (Success(Nil))
    }
    it must "not allow sep at the end of chain" in {
        sepBy('a', 'b').parse("ab") shouldBe a [Failure]
    }
    it should "be able to parse 2 or more p" in {
        sepBy('a', 'b').parse("aba") should be (Success(List('a', 'a')))
        sepBy('a', 'b').parse("ababa") should be (Success(List('a', 'a', 'a')))
        sepBy('a', 'b').parse("abababa") should be (Success(List('a', 'a', 'a', 'a')))
    }

    "sepBy1" must "require a p" in {
        sepBy1('a', 'b').parse("a") should not be a [Failure]
        sepBy1('a', 'b').parse(input = "") shouldBe a [Failure]
    }

    "sepEndBy" must "accept empty input" in {
        sepEndBy('a', 'b').parse("") should be (Success(Nil))
    }
    it should "not require sep at the end of chain" in {
        sepEndBy('a', 'b').parse("a") should be (Success(List('a')))
    }
    it should "be able to parse 2 or more p" in {
        sepEndBy('a', 'b').parse("aba") should be (Success(List('a', 'a')))
        sepEndBy('a', 'b').parse("ababa") should be (Success(List('a', 'a', 'a')))
    }
    it should "be able to parse a final sep" in {
        sepEndBy('a', 'b').parse("ab") should be (Success(List('a')))
        sepEndBy('a', 'b').parse("abab") should be (Success(List('a', 'a')))
        sepEndBy('a', 'b').parse("ababab") should be (Success(List('a', 'a', 'a')))
    }
    it should "fail if p fails after consuming input" in {
        sepEndBy("aa", 'b').parse("ab") shouldBe a [Failure]
    }
    it should "fail if sep fails after consuming input" in {
        sepEndBy('a', "bb").parse("ab") shouldBe a [Failure]
    }
    it must "not corrupt the stack on sep hard-fail" in {
        ('c' <::> attempt(sepEndBy('a', "bb")).getOrElse(List('d'))).parse("cab") should be (Success(List('c', 'd')))
    }

    "sepEndBy1" must "require a p" in {
        sepEndBy1('a', 'b').parse("a") should not be a [Failure]
        sepEndBy1('a', 'b').parse(input = "") shouldBe a [Failure]
    }

    "endBy" must "accept empty input" in {
        endBy('a', 'b').parse("") should be (Success(Nil))
    }
    it must "require sep at end of chain" in {
        endBy('a', 'b').parse("a") shouldBe a [Failure]
        endBy('a', 'b').parse("ab") should be (Success(List('a')))
    }
    it should "be able to parse 2 or more p" in {
        endBy('a', 'b').parse("abab") should be (Success(List('a', 'a')))
        endBy('a', 'b').parse("ababab") should be (Success(List('a', 'a', 'a')))
    }

    "endBy1" must "require a p" in {
        endBy1('a', 'b').parse("ab") should not be a [Failure]
        endBy1('a', 'b').parse(input = "") shouldBe a [Failure]
    }

    "eof" must "succeed at the end of input" in {
        eof.parse("") should not be a [Failure]
    }
    it must "fail if input remains" in {
        eof.parse("a") shouldBe a [Failure]
    }

    "notFollowedBy" must "succeed if p fails" in {
        notFollowedBy('a').parse("") should be (Success(()))
    }
    it must "succeed even if p consumed input" in {
        notFollowedBy("aa").parse("a") should be (Success(()))
    }
    it must "fail if p succeeds" in {
        notFollowedBy('a').parse("a") shouldBe a [Failure]
    }

    "manyUntil" must "require an end" in {
        manyUntil('a', 'b').parse("aa") shouldBe a [Failure]
        manyUntil('a', 'b').parse("ab") should be (Success(List('a')))
    }
    it should "parse the end without result" in {
        manyUntil('a', 'b').parse("b") should be (Success(Nil))
    }
    it should "parse p until that end is found" in {
        manyUntil('a', 'b').parse("aaaaaaaaaaaab") should not be a [Failure]
        manyUntil("aa", 'b').parse("ab") shouldBe a [Failure]
    }

    "someUntil" must "parse at least 1 p" in {
        someUntil('a', 'b').parse("ab") should be (Success(List('a')))
        someUntil('a', 'b').parse("b") shouldBe a [Failure]
    }

    /*"forP" should "be able to parse context-sensitive grammars" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val abc = put(r1, 0) *>
                  many('a' *> modify[Int](r1, _ + 1)) *>
                  forP[Int](r2, get(r1), pure(_ != 0), pure(_ - 1), 'b') *>
                  forP[Int](r2, get(r1), pure(_ != 0), pure(_ - 1), 'c')
        abc.parse("aaabbbccc") should be (Success(()))
        abc.parse("aaaabc") shouldBe a [Failure]
    }*/
}
