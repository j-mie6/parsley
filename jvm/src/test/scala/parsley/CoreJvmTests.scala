package parsley

import parsley.combinator.manyUntil
import parsley.character.{anyChar, string}
import parsley.implicits.character.{charLift, stringLift}
import parsley.io._

import scala.language.implicitConversions

import java.io.File

class CoreJvmTests extends ParsleyTest {
    "parseFromFile" should "work" in {
        (manyUntil(anyChar, "Jamie Willis") *> anyChar).parseFromFile(new File("LICENSE")).get shouldBe Success('\n')
    }

    "stack overflows" should "not occur" in {
        def repeat(n: Int, p: Parsley[Char]): Parsley[Char] = {
            if (n > 0) p *> repeat(n-1, p)
            else p
        }
        noException should be thrownBy repeat(4000, 'a').parse("a")
    }
}
