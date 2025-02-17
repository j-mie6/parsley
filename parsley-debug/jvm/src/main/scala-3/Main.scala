import parsley.Parsley
import parsley.quick.*
import parsley.syntax.character.{charLift, stringLift}
import parsley.debug.combinator.*
import parsley.errors.combinator._

import parsley.debug.DillRemoteView

import parsley.debug.*
import parsley.debug.combinator.DebuggerOps

object Main {
    @main def main() = {
        lazy val hello: Parsley[String] = ("hel" ~> "lo").break(FullBreak).label("hehe")
        lazy val world: Parsley[String] = ("world")
        (hello ~> world).break(ExitBreak).attach(DillRemoteView).parse("hello world")
    }
}
