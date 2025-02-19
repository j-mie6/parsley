package parsley.debug

import parsley.Parsley
import parsley.Parsley.*
import parsley.syntax.character.stringLift
import parsley.ParsleyTest
import parsley.debug.combinator.DebuggerOps
import org.scalatest.Assertions
import org.typelevel.scalaccompat.annotation.unused

private [debug] class MockedDebugView(private val exp: Iterator[Int]) extends DebugView.Reusable with DebugView.Pauseable {
  override private [debug] def render(@unused input: =>String, @unused tree: =>DebugTree): Unit = ()
  override private [debug] def renderWait(@unused input: =>String, @unused tree: =>DebugTree): Int = exp.nextOption().getOrElse(Assertions.fail("Hit unexpected breakpoint"))

  private [debug] def checkMetExpectations(): Unit = if (exp.hasNext) Assertions.fail(s"Did not hit all breakpoints. Still expecting: (${exp.mkString(", ")})")
}

class RemoteBreakSpec extends ParsleyTest {

    /* The test runner handling mocking functionality given a parser set with breakpoints, the input, and breakpoint return values */
    private def testExpecting(expectations: Int*)(p: Parsley[_], input: String): Unit = {
        val mock = new MockedDebugView(expectations.iterator)
        p.attach(mock).parse(input)
        mock.checkMetExpectations()
    }

    private def testExpectingNone = testExpecting()

    behavior of "Remote breakpoints"

    it should "call renderWait after hitting a breakpoint" in {
        val p: Parsley[_] = "J"
        testExpecting(0)(p.break(EntryBreak), "J")
        testExpecting(0)(p.break(ExitBreak), "J")
    }

    it should "not break when given NoBreak" in {
        val p: Parsley[_] = "A"
        testExpectingNone(p.break(NoBreak), "A")
    }

    it should "break twice when given FullBreak" in {
        val p: Parsley[_] = "M"
        testExpecting(0, 0)(p.break(FullBreak), "M")
    }

    it should "skip one breakpoint" in {
        val p1: Parsley[_] = "I"
        val p2: Parsley[_] = p1.break(ExitBreak) ~> p1.break(EntryBreak)
        testExpecting(1)(p2, "II")
    }

    it should "skip many breakpoints" in {
        val p1: Parsley[_] = "!"
        val p2: Parsley[_] = p1.break(FullBreak) ~> p1.break(FullBreak)
        testExpecting(2, 2)(p2.break(FullBreak), "!!")
    }

    it should "never break if the parser wasn't reached" in {
        val p1: Parsley[_] = "1"
        val p2: Parsley[_] = "2"
        testExpectingNone(p1 ~> p2.break(FullBreak), "02")
    }

    it should "break many times with iterative combinators" in {
        val p: Parsley[_] = "5"
        testExpecting(0, 0, 0, 0)(many(p.break(EntryBreak)), "555")
    }
}
