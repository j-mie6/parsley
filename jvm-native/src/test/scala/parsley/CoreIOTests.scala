package parsley

import parsley.combinator.manyUntil
import parsley.character.{item, string}
import parsley.implicits.character.{charLift, stringLift}
import parsley.io._

import scala.language.implicitConversions

import java.io.File

class CoreIOTests extends ParsleyTest {
    "parseFromFile" should "work" in {
        (manyUntil(item, "Jamie Willis") *> item).parseFromFile(new File("LICENSE")).get shouldBe Success('\n')
    }
    it should "fail with an error when file does not exist" in {
        Parsley.empty.parseFromFile(new File("foo.diuh")) shouldBe a [scala.util.Failure[_]]
    }

    "stack overflows" should "not occur" in {
        def repeat(n: Int, p: Parsley[Char]): Parsley[Char] = {
            if (n > 0) p *> repeat(n-1, p)
            else p
        }
        noException should be thrownBy repeat(4000, 'a').parse("a")
    }
}
