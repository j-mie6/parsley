package parsley.debugger

import org.scalatest.flatspec.AnyFlatSpec
import org.typelevel.scalaccompat.annotation.unused
import parsley.Parsley.fresh
import parsley.character.string
import parsley.combinator.many
import parsley.debugger.combinator.attachDebugger

class NodeCoverageSpec extends AnyFlatSpec {
    behavior of "the debugger runtime"

    it should "preserve the behaviour of parsers" in {
        var hit1 = false
        var hit2 = false
        var hit3 = false

        val parser = fresh { hit1 = true }.impure *> fresh { hit2 = true }.impure <* fresh { hit3 = true }
        val debugged = attachDebugger(parser)

        val _ = debugged._2.parse(""): @unused

        assert(hit1 && hit2 && hit3)
    }

    it should "factor out inputs of child parsers" in {
        val parser = many(string("abc"))
        val (treeF, debugged) = attachDebugger(parser)

        val _ = debugged.parse("abcabc"): @unused
        val tree = treeF()

        tree.parseResults match {
            case Some(result) => assertResult("{1}{2}")(result.rawInput)
            case None         => fail("No result recorded.")
        }
    }
}
