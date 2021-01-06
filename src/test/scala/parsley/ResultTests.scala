package parsley

import scala.language.implicitConversions

class ResultTests extends ParsleyTest {
    "Success" should "return true for isSuccess" in {
        Success(7).isSuccess shouldBe true
    }
    it should "return false for isFailure" in {
        Success(7).isFailure shouldBe false
    }

    "Failure" should "return false for isSuccess" in {
        Failure("oops").isSuccess shouldBe false
    }
    it should "return true for isFailure" in {
        Failure("oops").isFailure shouldBe true
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
            y <- Failure("oops")
            x <- Success((y: Int) + 4)
        } yield x
        val ex = for {
            y <- Left("oops")
            x <- Right((y: Int) + 4)
        } yield x
        rx.toEither should equal (ex)
        val rx2 = for {
            y <- Success(5)
            x <- Failure("oops")
        } yield x
        val ex2 = for {
            y <- Right(5)
            x <- Left("oops")
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
}