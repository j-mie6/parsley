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

    "Result[A]" should "behave like Either[String, A]" in {
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
}