/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debug

import parsley.Parsley
import parsley.Parsley.*
import parsley.combinator.*
import parsley.character.*
import parsley.syntax.character.{charLift, stringLift}
import parsley.state.*
import parsley.debug.*
import parsley.debug.RefCodec.CodedRef
import parsley.debug.combinator.DebuggerOps
import parsley.ParsleyTest

import org.scalatest.Assertions
import org.typelevel.scalaccompat.annotation.unused


// Mock for breakpoint stepping
private [debug] class MockedPauseableView(exp: Iterator[Int]) extends DebugView.Reusable with DebugView.Pauseable {
  override private [debug] def render(@unused input: =>String, @unused tree: =>DebugTree): Unit = ()

  override private [debug] def renderWait(@unused input: =>String, @unused tree: =>DebugTree): Int = if (exp.hasNext) exp.next() else Assertions.fail("Hit unexpected breakpoint")

  private [debug] def checkMetExpectations(): Unit = if (exp.hasNext) Assertions.fail(s"Did not hit all breakpoints. Still expecting: (${exp.mkString(", ")})")
}

// Mock for modifying state
private [debug] class MockedManageableView(exp: Iterator[Seq[String]]) extends DebugView.Manageable {
  override private[debug] def render(input: => String, tree: => DebugTree): Unit = ()

  override private[debug] def renderWait(input: => String, tree: => DebugTree): Int = Assertions.fail("Should not have called renderWait")

  override private[debug] def renderManage(input: => String, tree: => DebugTree, state: (Int, String)*): (Int, Seq[CodedRef]) = {
    Assertions.assert(exp.hasNext, "Unexpected call to renderManage")

    val seq = exp.next();
    val newState = state.map(_._1).zip(seq)

    (0, newState)
  }

  private [debug] def checkMetExpectations(): Unit = if (exp.hasNext) Assertions.fail(s"Did not hit all breakpoints. Still expecting: ()")
}


class RemoteBreakSpec extends ParsleyTest {

    behavior of "Remote breakpoint"


    /** Tests for EntryBreak and ExitBreak consuming the right input
      * 
      */
      
    // TODO
    // it should "break before input is consumed on EntryBreak" in {
    //     val p: Parsley[_] = string("scala").break(EntryBreak)
    //     val mock = new MockedPauseableView2(Iterator())
    //     p.attach(mock).parse("scala")
    // }

    it should "break after input is consumed on ExitBreak" in {
        val p: Parsley[_] = string("scala").break(ExitBreak)
        val mock = new DebugView.Reusable with DebugView.Pauseable {
            private[debug] def render(input: => String, tree: => parsley.debug.DebugTree): Unit = ()
            private[debug] def renderWait(@unused input: => String, tree: => parsley.debug.DebugTree): Int = {
                val breakAttempt: ParseAttempt = tree.parseResults.getOrElse(Assertions.fail("RemoteBreak has no ParseAttempt"))
                Assertions.assert(breakAttempt.success, "RemoteBreak not marked as successful")

                tree.nodeChildren match {
                    case child :: Nil => {
                        val childAttempt = child.parseResults.getOrElse(throw new Exception("Internal error: Test is incorrect or Parsley is broken"))
                        Assertions.assertResult(childAttempt.rawInput)("scala")
                        0
                    }
                    case _ => Assertions.fail("RemoteBreak has more than one child")
                }
            }

        }
        val _ = p.attach(mock).parse("scala")
    }


    /** Tests for breakpoint stepping
      * 
      */

    // The test runner handling mocking functionality given a parser set with breakpoints, the input, and breakpoint return values
    private def testExpectingSkips(expectations: Int*)(p: Parsley[_], input: String): Unit = {
        val mock = new MockedPauseableView(expectations.iterator)
        p.attach(mock).parse(input)
        mock.checkMetExpectations()
    }

    private def testExpectingNoSkips = testExpectingSkips() _

    it should "call renderWait after hitting a breakpoint" in {
        val p: Parsley[_] = "J"
        testExpectingSkips(0)(p.break(EntryBreak), "J")
        testExpectingSkips(0)(p.break(ExitBreak), "J")
    }

    it should "not break when given NoBreak" in {
        val p: Parsley[_] = string("A").break(NoBreak)
        testExpectingNoSkips(p, "A")
    }

    it should "break twice when given FullBreak" in {
        val p: Parsley[_] = "M"
        testExpectingSkips(0, 0)(p.break(FullBreak), "M")
    }

    it should "skip one breakpoint" in {
        val p: Parsley[_] = string("I").break(ExitBreak)
        testExpectingSkips(1)(p ~> p, "II")
    }

    it should "skip breakpoints many times" in {
        val p1: Parsley[_] = string("E").break(FullBreak)
        val p2: Parsley[_] = p1 ~> p1 ~> p1
        testExpectingSkips(1, 1, 1)(p2, "EEE")
    }

    it should "skip many breakpoints at once" in {
        val p1: Parsley[_] = string("!").break(FullBreak)
        val p2: Parsley[_] = p1 ~> p1 ~> p1
        testExpectingSkips(7)(p2.break(FullBreak), "!!!")
    }

    it should "never break if the parser wasn't reached" in {
        val p: Parsley[_] = "1" ~> string("2").break(FullBreak)
        testExpectingNoSkips(p, "02")
    }

    it should "break many times with iterative combinators" in {
        val p: Parsley[_] = string("5").break(EntryBreak)
        testExpectingSkips(0, 0, 0, 0)(many(p), "555")
    }

    it should "stay silent after skipping more breakpoints than there are" in {
        val p: Parsley[_] = string(".").break(FullBreak)
        testExpectingSkips(10)(p ~> p, "..")
    }

    it should "skip indefinitely with the special case of -1" in {
        val p: Parsley[_] = string("#").break(EntryBreak)
        testExpectingSkips(-1)(many(p), "#####")
    }


    /** Tests for modifying state
      * 
      */

    // The test runner handling mocking functionality given a parser set with breakpoints, the input, and the return values ofencoded string 
    private def testExpectingRefs[B](expectations: Seq[String]*)(leftTagInner: Parsley[B], refCodec: Codec[B], mkParser: B => Parsley[B])(input: String, shouldSucceed: Boolean): Unit = {        
        val leftTag: Parsley[B] = (atomic('<' <~ notFollowedBy('/'))) ~> leftTagInner <~ '>'
        val p = leftTag.fillRef { r => {
            object TestRefCodec extends RefCodec {
                type A = B

                val ref: Ref[B] = r
                val codec: Codec[B] = refCodec
            }
            char(' ').break(ExitBreak, TestRefCodec) <~ (string("</") ~> r.get.flatMap(mkParser) <~ ">")
        }}

        val mock = new MockedManageableView(expectations.iterator)
        (p.attach(mock).parse(input), shouldSucceed) match {
            case (res: parsley.Failure[_], true) => Assertions.fail(f"Parser should've succeeded, failed with ${res}")
            case (res: parsley.Success[_], false) => Assertions.fail(f"Parser should've failed, succeeded with ${res}")
            case _ => mock.checkMetExpectations()
        }
    }

    // Parsers for standard expressions
    val trueParser = "true" as true
    val falseParser = "false" as false
    val boolParser = choice(trueParser, falseParser)
    val shortParser = digit.foldLeft1[Short](0)((n, d) => (n * 10 + d.asDigit).toShort)
    val intParser = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
    val longParser = digit.foldLeft1[Long](0)((n, d) => n * 10 + d.asDigit)

    // Making numerical parsers given a reference
    def myNumeric[A](x: A) = parsley.character.string(x.toString) as x
    val myShort = myNumeric[Short] _
    val myInt = myNumeric[Int] _
    val myLong = myNumeric[Long] _
    val myBool = { b: Boolean => ifS(pure(b), trueParser, falseParser) }

    // it should "preserve references that aren't passed to break" in {
    //     val p = leftTagLetters.fillRef { name => char(' ').break(ExitBreak). <~ ("</" ~> refString(name) <~ ">") }
    //     testExpectingRefs(Seq.empty)(p, "<hi> </hi>", true)
    // }
    
    it should "preserve references that are passed in and not returned" in {
        testExpectingRefs(Seq.empty)(stringOfSome(letter), StringCodec, string)("<hello> </hi>", false)
    }

    it should "modify Ref[String]" in {
        testExpectingRefs(Seq("hi"))(stringOfSome(letter), StringCodec, string)("<hello> </hi>", true)
    }

    it should "modify Ref[Char]" in {
        testExpectingRefs(Seq("z"))(letter, CharCodec, char)("<a> </z>", true)
    }

    it should "modify Ref[Boolean]" in {
        testExpectingRefs(Seq("true"))(choice(trueParser, falseParser), BooleanCodec, myBool)("<false> </true>", true)
    }

    it should "modify Ref[Short]" in {
        testExpectingRefs(Seq("127"))(shortParser, ShortCodec, myShort)("<0> </127>", true)
    }
    
    it should "modify Ref[Int]" in {
        testExpectingRefs(Seq("256"))(intParser, IntCodec, myInt)("<255> </256>", true)
    }

    it should "modify Ref[Long]" in {
        testExpectingRefs(Seq("2147483648"))(longParser, LongCodec, myLong)("<2147483647> </2147483648>", true)
    }



    // it should "modify references many times" in {
    //     testExpectingRefs(Seq("bye"), Seq("hi"))(stringOfSome(letter), StringCodec, string)("<hello> </hi>", true)
    // }
}
