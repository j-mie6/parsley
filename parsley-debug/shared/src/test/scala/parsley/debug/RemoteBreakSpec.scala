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
import parsley.token.*
import parsley.token.descriptions.LexicalDesc
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
      
    it should "break before input is consumed on EntryBreak" in {
        val p: Parsley[_] = "cat " ~> string("gifs").break(EntryBreak)
        val mock = new DebugView.Reusable with DebugView.Pauseable {
            private[debug] def render(input: => String, tree: => parsley.debug.DebugTree): Unit = ()
            private[debug] def renderWait(@unused input: => String, tree: => parsley.debug.DebugTree): Int = {
                val breakAttempt: ParseAttempt = tree.parseResults.getOrElse(Assertions.fail("RemoteBreak has no ParseAttempt"))
                
                Assertions.assert(breakAttempt.success, "RemoteBreak not marked as successful")
                tree.nodeChildren match {
                    case child :: Nil => {
                        Assertions.assert(child.parseResults.isEmpty, s"Child has consumed input: ${child.parseResults}")
                        0
                    }
                    case children => Assertions.fail(f"RemoteBreak does not have one child (${children.map(_.internalName)})")
                }
            }

        }
        val _ = p.attach(mock).parse("cat gifs")
    }

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
                    case children => Assertions.fail(f"RemoteBreak does not have one child (${children.map(_.internalName)})")
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

                val ref: Ref[A] = r
                val codec: Codec[A] = refCodec
            }
            char(' ').break(ExitBreak, TestRefCodec) <~ (string("</") ~> r.get.flatMap(mkParser) <~ ">")
        }}

        val mock = new MockedManageableView(expectations.iterator)
        runManageableTest(p, mock, input, shouldSucceed)
    }

    private def runManageableTest(p: Parsley[_], mock: MockedManageableView, input: String, shouldSucceed: Boolean): Unit = {
        (p.attach(mock).parse(input), shouldSucceed) match {
            case (res: parsley.Failure[_], true) => Assertions.fail(f"Parser should've succeeded, failed with ${res}")
            case (res: parsley.Success[_], false) => Assertions.fail(f"Parser should've failed, succeeded with ${res}")
            case _ => mock.checkMetExpectations()
        }
    }

    // Parsers for standard expressions
    val lexer = new Lexer(LexicalDesc.plain)
    val trueParser = "true" as true
    val falseParser = "false" as false
    val boolParser = choice(trueParser, falseParser)
    val byteParser = digit.foldLeft1[Byte](0)((n, d) => (n * 10 + d.asDigit).toByte)
    val shortParser = digit.foldLeft1[Short](0)((n, d) => (n * 10 + d.asDigit).toShort)
    val intParser = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
    val longParser = digit.foldLeft1[Long](0)((n, d) => n * 10 + d.asDigit)
    val floatParser = lexer.lexeme.real.exactFloat
    val doubleParser = lexer.lexeme.real.exactDouble

    // Making numerical parsers given a reference
    def myNumeric[A](x: A) = parsley.character.string(x.toString) as x
    val myByte = myNumeric[Byte] _
    val myShort = myNumeric[Short] _
    val myInt = myNumeric[Int] _
    val myLong = myNumeric[Long] _
    val myFloat = myNumeric[Float] _
    val myDouble = myNumeric[Double] _
    val myBool = { (b: Boolean) => if (b) trueParser else falseParser }

    it should "preserve references that aren't passed to break" in {
        val leftTag = (atomic('<' <~ notFollowedBy('/'))) ~> "hello" <~ '>'
        val p = leftTag.fillRef { r => char(' ').break(ExitBreak) <~ (string("</") ~> r.get.flatMap(string) <~ ">")}
        val mock = new MockedManageableView(Iterator(Seq.empty))
        runManageableTest(p, mock, "<hello> </hello>", true)
    }
    
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

    it should "modify Ref[Byte]" in {
        testExpectingRefs(Seq("1"))(byteParser, ByteCodec, myByte)("<0> </1>", true)
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

    it should "modify Ref[Float]" in {
        testExpectingRefs(Seq("0.03125"))(floatParser, FloatCodec, myFloat)("<0.0> </0.03125>", true)
    }

    it should "modify Ref[Double]" in {
        testExpectingRefs(Seq("0.00390625"))(doubleParser, DoubleCodec, myDouble)("<0.0> </0.00390625>", true)
    }

    it should "modify the same reference many times" in {
        val leftTag = (atomic('<' <~ notFollowedBy('/'))) ~> "hello"
        val p = leftTag.fillRef { r => 
            object TestRefCodec extends RefCodec {
                type A = String

                val ref: Ref[A] = r
                val codec: Codec[A] = StringCodec
            }
            (char('>').break(ExitBreak, TestRefCodec) ~> r.get.flatMap(string).break(ExitBreak, TestRefCodec)) <~ (string("</").break(ExitBreak, TestRefCodec) ~> r.get.flatMap(string) <~ ">")
        }
        val mock = new MockedManageableView(Iterator(Seq("hey"), Seq("bye"), Seq("hi")))
        runManageableTest(p, mock, "<hello>hey</hi>", true)
    }

    it should "modify multiple references at the same time of different types" in {
        val openTag = (atomic('<' <~ notFollowedBy('/')))

        val aTagLeft = openTag ~> "a" <~ '>'
        val bTagLeft = openTag ~> 'b' <~ '>'

        val p = aTagLeft.fillRef { r1 => 
            object Ref1Codec extends RefCodec {
                type A = String
                val ref: Ref[A] = r1
                val codec: Codec[A] = StringCodec
            }
            bTagLeft.fillRef { r2 => 
                object Ref2Codec extends RefCodec {
                    type A = Char
                    val ref: Ref[A] = r2
                    val codec: Codec[A] = CharCodec
                }
                char(' ').break(ExitBreak, Ref1Codec, Ref2Codec) ~> ("</" ~> r2.get.flatMap(char) <~ '>')
            } <~> "</" ~> r1.get.flatMap(string) <~ '>'
        }
        val mock = new MockedManageableView(Iterator(Seq("A", "B")))
        runManageableTest(p, mock, "<a><b> </B></A>", true)
    }

    it should "modify correctly with EntryBreak" in {
        val leftTag = (atomic('<' <~ notFollowedBy('/'))) ~> "hello" <~ '>'
        val p = leftTag.fillRef { r => 
            object TestRefCodec extends RefCodec {
                type A = String

                val ref: Ref[A] = r
                val codec: Codec[A] = StringCodec
            }
            (string("</") ~> r.get.flatMap(string).break(EntryBreak, TestRefCodec) <~ ">")}
        val mock = new MockedManageableView(Iterator(Seq("world")))
        runManageableTest(p, mock, "<hello></world>", true)
    }
}
