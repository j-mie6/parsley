import parsley.Parsley
import parsley.quick.*
import parsley.syntax.character.{charLift, stringLift}
import parsley.debug.combinator.*
import parsley.errors.combinator._

import parsley.debug.DillRemoteView

object Main {
    @main def main() = {
        lazy val hello: Parsley[String] = ("hel" ~> "lo").label("hehe")
        lazy val world: Parsley[String] = ("world")
        (hello ~> world).attach(DillRemoteView).parse("hello world")
    }
}
