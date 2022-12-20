/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.annotation.nowarn

class ResultTests extends ParsleyTest {
    "Success" should "return true for isSuccess" in {
        Success(7).isSuccess shouldBe true
    }
    it should "return false for isFailure" in {
        Success(7).isFailure shouldBe false
    }
    it should "not throw an exception on get" in {
        noException should be thrownBy (Success(4).get)
    }

    "Failure" should "return false for isSuccess" in {
        Failure("oops").isSuccess shouldBe false
    }
    it should "return true for isFailure" in {
        Failure("oops").isFailure shouldBe true
    }
    it should "throw an exception on get" in {
        a [NoSuchElementException] should be thrownBy (Failure("oops").get)
    }

    "Result[A]" should "behave like Either[String, A] on success" in {
        val rx = for {
            y <- Success(5)
            x <- Success(y + 4)
        } yield x
        val ex = for {
            y <- Right(5)
            x <- Right(y + 4)
        } yield x
        rx.toEither should equal (ex)
    }
    it should "behave like Either[String, A] on failure" in {
        val rx = for {
            y <- Failure("oops"): Result[String, Int]
            x <- Success((y: Int) + 4)
        } yield x
        val ex = for {
            y <- Left("oops"): Either[String, Int]
            x <- Right((y: Int) + 4)
        } yield x
        rx.toEither should equal (ex)
        val rx2 = for {
            _ <- Success(5)
            x <- Failure("oops"): Result[Any, Any]
        } yield x
        val ex2 = for {
            _ <- Right(5)
            x <- Left("oops"): Either[Any, Any]
        } yield x
        rx2.toEither should equal (ex2)
    }
    it should "behave like Option[A] when extracting elements" in {
        Success(7).getOrElse(5) should equal (Some(7).getOrElse(5))
        Failure("oops").getOrElse(5) should equal (None.getOrElse(5))
        Success(7).toOption should equal (Some(7))
        Failure("oops").toOption should equal (None)
    }
    it should "throw an exception when it fails and converted to Try" in {
        noException should be thrownBy (Success(7).toTry.get)
        an [Exception] should be thrownBy (Failure("oops").toTry.get)
    }
    it should "be convertible to a possibly one element sequence" in {
        Success(4).toSeq should have size 1
        Failure("oops").toSeq shouldBe empty
    }
    it should "support contains" in {
        Success(5).contains(5) shouldBe true
        Success(4).contains(5) shouldBe false
        Failure("oops").contains(5) shouldBe false
    }
    it should "support forall" in {
        Success(5).forall((x: Int) => x % 2 == 0) shouldBe false
        Success(4).forall((x: Int) => x % 2 == 0) shouldBe true
        Failure("msg").forall((x: Int) => x % 2 == 0) shouldBe true
    }
    it should "support foreach" in {
        var x = 0
        Failure("msg").foreach(y => x = y): @nowarn
        x shouldBe 0
        for (y <- Success(3)) x = y
        x shouldBe 3
    }
    it should "flatten correctly" in {
        Success(Success(4)).flatten.isSuccess shouldBe true
        Success(Failure("ops")).flatten.isFailure shouldBe true
        Failure("msg").flatten.isFailure shouldBe true
    }
    it should "be filterable" in {
        Success(5).filterOrElse(_ == 5, "???") shouldBe Success(5)
        Success(5).filterOrElse(_ == 4, "should be 4!") shouldBe Failure("should be 4!")
        Failure("oops").filterOrElse(_ == 4: @nowarn, "should be 4!") shouldBe Failure("oops")
    }
    it should "be foldable" in {
        (Success(4).fold(_ => 0, _+1): @nowarn) shouldBe 5
        Failure("oops").fold(_ => 0, (x: Int) => x + 1) shouldBe 0
    }
}
