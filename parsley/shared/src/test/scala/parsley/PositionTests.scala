/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import Parsley.unit
import parsley.position._
import parsley.character.{char, string}

class PositionTests extends ParsleyTest {
    "line" should "start at 1" in {
        line.parse("") shouldBe Success(1)
    }
    it should "increment on newline" in {
        (char('\n') ~> line).parse("\n") shouldBe Success(2)
    }
    it should "not increment on tabs or other chars" in {
        (char('\t') ~> line).parse("\t") shouldBe Success(1)
        (char('a') ~> line).parse("a") shouldBe Success(1)
    }

    "col" should "start at 1" in {
        col.parse("") shouldBe Success(1)
    }
    it should "reset on newline" in {
        (char('a') ~> char('\n') ~> col).parse("a\n") shouldBe Success(1)
    }
    it should "go to tab boundaries" in {
        (char('\t') ~> col).parse("\t") shouldBe Success(5)
        (char('a') ~> char('\t') ~> col).parse("a\t") shouldBe Success(5)
        (char('a') ~> char('a') ~> char('a') ~> char('a') ~> char('\t') ~> col).parse("aaaa\t") shouldBe Success(9)
    }
    it should "increment on other chars" in {
        (char('a') ~> col).parse("a") shouldBe Success(2)
    }

    "offset" should "start at 0" in {
        offset.parse("") shouldBe Success(0)
    }
    it should "only increase by one regardless of character" in {
        (char('\n') ~> offset).parse("\n") shouldBe Success(1)
        (char('\t') ~> offset).parse("\t") shouldBe Success(1)
        (char('a') ~> offset).parse("a") shouldBe Success(1)
    }

    "withWidth" should "return 0 for pure things" in {
        withWidth(unit).parse("a") shouldBe Success(((), 0))
        (char('a') ~> withWidth(unit)).parse("a") shouldBe Success(((), 0))
    }
    it should "correctly span input consumption" in {
        withWidth(string("abc")).parse("abc") shouldBe Success(("abc", 3))
        (char('x') ~> withWidth(string("abc"))).parse("xabc") shouldBe Success(("abc", 3))
    }
}
