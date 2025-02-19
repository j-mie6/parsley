package parsley.debug

import parsley.Parsley
import parsley.Parsley.*
import parsley.character.string
import parsley.syntax.character.stringLift
import parsley.ParsleyTest
import parsley.debug.combinator.DebuggerOps
import org.scalatest.Assertions
import org.typelevel.scalaccompat.annotation.unused

private [debug] class MockedDebugView(private val exp: Iterator[Int]) extends DebugView.Reusable with DebugView.Pauseable {
  override private [debug] def render(@unused input: =>String, @unused tree: =>DebugTree): Unit = ()
  override private [debug] def renderWait(@unused input: =>String, @unused tree: =>DebugTree): Int = if (exp.hasNext) exp.next else Assertions.fail("Hit unexpected breakpoint")

  private [debug] def checkMetExpectations(): Unit = if (exp.hasNext) Assertions.fail(s"Did not hit all breakpoints. Still expecting: (${exp.mkString(", ")})")
}

class RemoteBreakSpec extends ParsleyTest {

    /* The test runner handling mocking functionality given a parser set with breakpoints, the input, and breakpoint return values */
    private def testExpecting(expectations: Int*)(p: Parsley[_], input: String): Unit = {
        val mock = new MockedDebugView(expectations.iterator)
        p.attach(mock).parse(input)
        mock.checkMetExpectations()
    }

    private def testExpectingNone = testExpecting() _

    behavior of "Remote breakpoints"

    it should "call renderWait after hitting a breakpoint" in {
        val p: Parsley[_] = "J"
        testExpecting(0)(p.break(EntryBreak), "J")
        testExpecting(0)(p.break(ExitBreak), "J")
    }

    it should "not break when given NoBreak" in {
        val p: Parsley[_] = string("A").break(NoBreak)
        testExpectingNone(p, "A")
    }

    it should "break twice when given FullBreak" in {
        val p: Parsley[_] = "M"
        testExpecting(0, 0)(p.break(FullBreak), "M")
    }

    it should "skip one breakpoint" in {
        val p: Parsley[_] = string("I").break(ExitBreak)
        testExpecting(1)(p ~> p, "II")
    }

    it should "skip many breakpoints" in {
        val p1: Parsley[_] = string("!").break(FullBreak)
        val p2: Parsley[_] = p1 ~> p1
        testExpecting(2, 2)(p2.break(FullBreak), "!!")
    }

    it should "never break if the parser wasn't reached" in {
        val p: Parsley[_] = "1" ~> string("2").break(FullBreak)
        testExpectingNone(p, "02")
    }

    it should "break many times with iterative combinators" in {
        val p: Parsley[_] = string("5").break(EntryBreak)
        testExpecting(0, 0, 0, 0)(many(p), "555")
    }
}
