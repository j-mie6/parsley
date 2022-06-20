/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.character.{letter, string, strings, stringOfMany, stringOfSome}
import parsley.implicits.character.{charLift, stringLift}
import parsley.Parsley._

import scala.language.implicitConversions

class StringTests extends ParsleyTest {
    private def stringPositionCheck(initialCol: Int, str: String) = {
        ((if (initialCol == 0) pure("") else string("." * initialCol)) *> string(str) *> pos).parse("." * initialCol + str)
    }

    /*private def badStrings(str0: String, strs: String*) = {
        attemptChoice((str0 +: strs).sorted(implicitly[Ordering[String]].reverse).map(string): _*)
    }*/

    "string" should "consume succeed if it is found at head" in {
        "abc".parse("abc") should not be a [Failure[_]]
    }
    it should "not consume input if it fails on first character" in {
        ("abc" <|> 'b').parse("b") should not be a [Failure[_]]
    }
    it should "consume input if it fails mid-string" in {
        ("abc" <|> "ab").parse("ab") shouldBe a [Failure[_]]
    }
    it should "not consume input if it fails mid-string when combined with attempt" in {
        (attempt("abc") <|> "ab").parse("ab") should not be a [Failure[_]]
    }
    it should "update positions correctly" in {
        stringPositionCheck(0, "abc") shouldBe Success((1, 4))
        stringPositionCheck(1, "\na") shouldBe Success((2, 2))
        stringPositionCheck(0, "a\t") shouldBe Success((1, 5))
        stringPositionCheck(0, "ab\t") shouldBe Success((1, 5))
        stringPositionCheck(0, "abc\t") shouldBe Success((1, 5))
        stringPositionCheck(0, "abcd\t") shouldBe Success((1, 9))
        stringPositionCheck(0, "\na\tb") shouldBe Success((2, 6))
        stringPositionCheck(2, "\t") shouldBe Success((1, 5))
    }
    it should "respect multiple tabs" in {
        stringPositionCheck(2, "\t\t") shouldBe Success((1, 9))
        stringPositionCheck(2, "\t\t\t") shouldBe Success((1, 13))
        stringPositionCheck(2, "\taaa\t") shouldBe Success((1, 9))
        stringPositionCheck(2, "\taa\taaa\t") shouldBe Success((1, 13))
        stringPositionCheck(2, "a\t\t") shouldBe Success((1, 9))
        stringPositionCheck(2, "aa\t") shouldBe Success((1, 9))
    }

    "strings" should "have longest match behaviour" in {
        val p = strings("hell", "hello", "h")
        p.parse("hello") shouldBe Success("hello")
        p.parse("hell") shouldBe Success("hell")
        p.parse("he") shouldBe Success("h")
    }
    it should "be extrinsically the same as a manual equivalent" in {
        val p = strings("hell", "hello", "abc", "g", "goodbye")
        val q = string("abc") <|> attempt("goodbye") <|> string("g") <|> attempt(string("hello")) <|> string("hell")
        info("parsing \"hello\"")
        p.parse("hello") shouldBe q.parse("hello")
        info("parsing \"hell\"")
        p.parse("hell") shouldBe q.parse("hell")
        info("parsing \"h\"")
        p.parse("h") shouldBe q.parse("h")
        info("parsing \"a\"")
        p.parse("a") shouldBe q.parse("a")
        info("parsing \"b\"")
        p.parse("b") shouldBe q.parse("b")
    }

    "stringOfMany" should "allow for no letters" in {
        val p = stringOfMany(letter)
        p.parse("") shouldBe Success("")
    }
    it should "consume as many letters as it can" in {
        val p = stringOfMany(letter)
        p.parse("abc") shouldBe Success("abc")
        p.parse("ab4c") shouldBe Success("ab")
    }
    it should "fail if pc fails having consumed input" in {
        val p = stringOfMany(string("ab") #> 'a')
        p.parse("ababab") shouldBe Success("aaa")
        p.parse("aba") shouldBe a [Failure[_]]
    }

    "stringOfSome" should "not allow for no letters" in {
        val p = stringOfSome(letter)
        p.parse("") shouldBe a [Failure[_]]
    }
    it should "consume as many letters as it can" in {
        val p = stringOfSome(letter)
        p.parse("abc") shouldBe Success("abc")
        p.parse("ab4c") shouldBe Success("ab")
    }
    it should "fail if pc fails having consumed input" in {
        val p = stringOfSome(string("ab") #> 'a')
        p.parse("ababab") shouldBe Success("aaa")
        p.parse("aba") shouldBe a [Failure[_]]
    }
}
