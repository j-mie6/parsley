package parsley.internal

import parsley.{ParsleyTest, Success}
import parsley.Parsley._
import parsley.character.{char, satisfy}
import parsley.implicits.charLift

import scala.language.implicitConversions

class InternalTests extends ParsleyTest {
    "subroutines" should "function correctly and be picked up" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = 'a' *> p <* 'b' <* p <* 'c'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 1
        q.internal.instrs.last should be (instructions.Return)
        q.runParser("a123b123c") should be (Success('3'))
    }

    they should "function correctly under error messages" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = p ? "err1" *> 'a' *> p ? "err1" <* 'b' <* p ? "err2" <* 'c' <* p ? "err2" <* 'd'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 2
        q.runParser("123a123b123c123d") should be (Success('3'))
    }

    they should "not appear when only referenced once with any given error message" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = 'a' *> p ? "err1" <* 'b' <* p ? "err2" <* 'c'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 0
        q.runParser("a123b123c") should be (Success('3'))
    }

    they should "not duplicate subroutines when error label is the same" in {
        val p = satisfy(_ => true) *> satisfy(_ => true) *> satisfy(_ => true)
        val q = 'a' *> p ? "err1" <* 'b' <* p ? "err1" <* 'c'
        q.internal.instrs.count(_ == instructions.Return) shouldBe 1
        q.runParser("a123b123c") should be (Success('3'))
    }
}