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
    private def testExpectingRefs(expectations: Seq[String]*)(p: Parsley[_], input: String, shouldSucceed: Boolean): Unit = {
        val mock = new MockedManageableView(expectations.iterator)
        (p.attach(mock).parse(input), shouldSucceed) match {
            case (res: parsley.Failure[_], true) => Assertions.fail(f"Parser should've succeeded, failed with ${res}")
            case (res: parsley.Success[_], false) => Assertions.fail(f"Parser should've failed, succeeded with ${res}")
            case _ => mock.checkMetExpectations()
        }
    }

    /** Some common test utils */

    // Parsers for standard expressions
    val trueParser = "true" as true
    val falseParser = "false" as false
    val intParser = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)

    // Making a parser given a reference
    def refParser[A, B](mkParser: A => Parsley[B])(r: Ref[A]): Parsley[B] = r.get.flatMap(mkParser)
    val refChar = refParser(parsley.character.char) _
    val refString = refParser(parsley.character.string) _
    val refBool = refParser(if (_ : Boolean) trueParser else falseParser) _ // TODO use ifP
    val refInt = refParser { (x : Int) => parsley.character.string(x.toString) as x } _

    // Parsing different types in open tags
    def leftTag[A](p: Parsley[A]): Parsley[A] = (atomic('<' <~ notFollowedBy('/'))) ~> p <~ '>'
    val leftTagLetter = leftTag(letter)
    val leftTagLetters = leftTag(stringOfSome(letter))
    val leftTagBool = leftTag(choice(trueParser, falseParser))
    val leftTagInt = leftTag(intParser)

    it should "preserve references that aren't passed to break" in {
        val p = leftTagLetters.fillRef { name => char(' ').break(ExitBreak). <~ ("</" ~> refString(name) <~ ">") }
        testExpectingRefs(Seq.empty)(p, "<hi> </hi>", true)
    }
    
    it should "preserve references that are passed in and not returned" in {
        val p = leftTagLetters.fillRef { name => {
            class NameRefCodec extends RefCodec {
                type A = String

                val ref: Ref[A] = name
                val codec: Codec[A] = StringCodec
            }

            char(' ').break(ExitBreak, new NameRefCodec) <~ ("</" ~> refString(name) <~ ">") 
        }}
            
        testExpectingRefs(Seq.empty)(p, "<hello> </hi>", false)
    }

    it should "modify references that are passed in and returned" in {
        val p = leftTagLetters.fillRef { name => {
            class NameRefCodec extends RefCodec {
                type A = String

                val ref: Ref[A] = name
                val codec: Codec[A] = StringCodec
            }
            char(' ').break(ExitBreak, new NameRefCodec) <~ ("</" ~> refString(name) <~ ">") 
        }}
        
        testExpectingRefs(Seq("hi"))(p, "<hello> </hi>", true)
    }

    it should "modify references many times after passing them in and returning" in {
        val p = leftTagLetters.fillRef { name => {
            class NameRefCodec extends RefCodec {
                type A = String

                val ref: Ref[A] = name
                val codec: Codec[A] = StringCodec
            }
            char(' ').break(ExitBreak, new NameRefCodec) <~ (string("</").break(ExitBreak, new NameRefCodec) ~> refString(name) <~ ">") 
        }}
        
        testExpectingRefs(Seq("bye"), Seq("hi"))(p, "<hello> </hi>", true)
    }

    it should "modify Ref[Char]" in {
        val p = leftTagLetter.fillRef { name => {
            class NameRefCodec extends RefCodec {
                type A = Char

                val ref: Ref[A] = name
                val codec: Codec[A] = CharCodec
            }
            char(' ').break(ExitBreak, new NameRefCodec) <~ (string("</") ~> refChar(name) <~ ">") 
        }}
        
        testExpectingRefs(Seq("z"))(p, "<a> </z>", true)
    }

    it should "modify Ref[Boolean]" in {
        val p = leftTagBool.fillRef { name => {
            class NameRefCodec extends RefCodec {
                type A = Boolean

                val ref: Ref[A] = name
                val codec: Codec[A] = BooleanCodec
            }
            char(' ').break(ExitBreak, new NameRefCodec) <~ (string("</") ~> refBool(name) <~ ">") 
        }}
        
        testExpectingRefs(Seq("true"))(p, "<false> </true>", true)
    }

    it should "modify Ref[Int]" in {
        val p = leftTagInt.fillRef { name => {
            class NameRefCodec extends RefCodec {
                type A = Int

                val ref: Ref[A] = name
                val codec: Codec[A] = IntCodec
            }
            char(' ').break(ExitBreak, new NameRefCodec) <~ (string("</") ~> refInt(name) <~ ">")
        }}
        
        testExpectingRefs(Seq("256"))(p, "<255> </256>", true)
    }
}
