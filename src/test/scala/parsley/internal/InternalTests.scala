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
        q.internal.instrs.last should be (instructions.Return)
        q.runParser("a123b123c") should be (Success('3'))
    }
}