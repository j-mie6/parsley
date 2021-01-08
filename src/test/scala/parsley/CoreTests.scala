package parsley

import parsley.Parsley._
import parsley.Char.{char, satisfy}
import parsley.Implicits.{charLift, stringLift}

import scala.language.implicitConversions

class CoreTests extends ParsleyTest {
    private val add: (Int, Int) => Int = _+_
    private val add1 = (x: Int) => x + 1
    private val mult5 = (x: Int) => x * 5
    private val toUpper = (c: Char) => c.toUpper

    "Character parsers" should "fail on no input" in {
        'a'.runParser("") should not be a [Success[_]]
    }

    they should "fail when given incorrect input" in {
        'a'.runParser("b") should not be a [Success[_]]
    }

    they should "succeed when given correct input" in {
        'a'.runParser("a") should be (Success('a'))
    }

    they must "only consume a single character of input at most" in {
        var res = (satisfy(_ == 'a') *> 'b').runParser("aaaaaa")
        res shouldBe a [Failure]
        res match {
            case Failure(err) => err should startWith ("(line 1, column 2)")
            case _ =>
        }
        res = ('a' *> 'b').runParser("bc")
        res shouldBe a [Failure]
        res match {
            case Failure(err) => err should startWith ("(line 1, column 1)")
            case _ =>
        }
    }

    "Pure parsers" should "not require input" in {
        unit.runParser("") should not be a [Failure]
    }

    they must "result in their correct value" in {
        pure(42).runParser("") should be (Success(42))
        pure("hello world").runParser("") should be (Success("hello world"))
        pure(add).runParser("") should be (Success(add))
    }

    they must "not consume any input" in {
        (pure('a') *> 'a').runParser("a") should be (Success('a'))
    }

    // APPLICATIVE LAWS
    they must "obey the homomorphism law: pure f <*> pure x = pure (f x)" in {
        (pure(add1) <*> pure(42)).runParser("") should be (Success(43))
    }

    they must "obey the fmap law: pure f <*> p = f <$> p" in {
        (pure(toUpper) <*> 'a').runParser("a") should equal ((toUpper <#> 'a').runParser("a"))
        (pure(toUpper) <*> ('a' <|> 'b')).runParser("a") should equal ((toUpper <#> ('a' <|> 'b')).runParser("a"))
        (pure(toUpper) <*> ('a' <|> 'b')).runParser("b") should equal ((toUpper <#> ('a' <|> 'b')).runParser("b"))
    }

    they must "obey the interchange law: u <*> pure x = ($x) <$> u" in {
        (('a' #> add1) <*> pure(41)).runParser("a") should equal ((((f: Int => Int) => f(41)) <#> ('a' #> add1)).runParser("a"))
        ((('a' <|> 'b') #> add1) <*> pure(41)).runParser("a") should equal ((((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1)).runParser("a"))
        ((('a' <|> 'b') #> add1) <*> pure(41)).runParser("b") should equal ((((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1)).runParser("b"))
    }

    they must "obey the composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" in {
        val u: Parsley[Int => Int] = 'a' #> add1
        val v: Parsley[Int => Int] = 'b' #> mult5
        val w: Parsley[Int] = 'c' #> 7
        val compose: (Int => Int) => (Int => Int) => Int => Int = f => g => f.compose(g)
        (pure(compose) <*> u <*> v <*> w).runParser("abc") should equal ((u <*> (v <*> w)).runParser("abc"))
    }

    // SELECTIVE LAWS
    they must "obey the selective left-branch law" in {
        branch[Int, Int, Int](pure(Left(7)), pure(_+1), pure(_-1)).runParser("") shouldBe Success(8)
    }
    they must "obey the selective right-branch law" in {
        select(pure(Right(7)), Parsley.empty).runParser("") shouldBe Success(7)
    }

    // MONAD LAWS
    they must "obey the left identity law: pure x >>= f = f x" in {
        (pure('a') >>= char).runParser("a") should equal ('a'.runParser("a"))
    }
    "Parsers" must "obey the right identity law: m >>= pure = m" in {
        ('a' >>= pure).runParser("a") should equal ('a'.runParser("a"))
    }
    they must "obey the associativity law: (m >>= f) >>= g = m >>= (x => f x >>= g)" in {
        val f: Int => Parsley[Int] = x => pure(x + 1)
        val g: Int => Parsley[Int] = x => pure(x/3)
        val m = '1' #> 4
        ((m >>= f) >>= g).runParser("1") should equal ((m >>= (x => f(x) >>= g)).runParser("1"))
    }
    they must "allow for flattening" in {
        join(pure(char('a'))).runParser("a") shouldBe Success('a')
        join(Parsley.empty).runParser("") shouldBe a [Failure]
    }

    "mzero parsers" should "always fail" in {
        (Parsley.empty >> 'a').runParser("a") shouldBe a [Failure]
        (Parsley.fail("") >> 'a').runParser("a") shouldBe a [Failure]
        (Parsley.unexpected("") *> 'a').runParser("a") shouldBe a [Failure]
        (('a' ! (_ => "")) *> 'b').runParser("ab") shouldBe a [Failure]
        ('a'.unexpected(_ => "") *> 'b').runParser("ab") shouldBe a [Failure]
    }

    "<|>" should "not try the second alternative if the first succeeded" in {
        ('a' <|> Parsley.fail("wrong!")).runParser("a") should not be a [Failure]
    }
    it should "only try second alternative if the first failed without consuming input" in {
        ('a' <|> 'b').runParser("b") should not be a [Failure]
    }
    it should "not try the second alternative if the first failed after consuming input" in {
        ("ab" <|> "ac").runParser("ac") shouldBe a [Failure]
    }

    "attempt" should "cause <|> to try second alternative even if input consumed" in {
        attempt("ab").orElse("ac").runParser("ac") should not be a [Failure]
    }

    "lookAhead" should "consume no input on success" in {
        lookAhead('a').runParser("a") should not be a [Failure]
        (lookAhead('a') *> 'b').runParser("ab") should be (Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected \"b\""))
    }
    it must "fail when input is consumed, and input is consumed" in {
        lookAhead("ab").runParser("ac") shouldBe a [Failure]
    }
    /*it should "not affect the state of the registers on success" in {
        val r1 = Reg.make[Int]
        (put(r1, 5) *> lookAhead(put(r1, 7) *> 'a') *> get(r1)).runParser("a") should be {
            Success(5)
        }
        (put(r1, 5) *> (lookAhead(put(r1, 7) *> 'a') <|> 'b') *> get(r1)).runParser("b") should be {
            Success(7)
        }
    }*/

    "many" should "crash when given a parser that does not consume input" in {
       an [Exception] should be thrownBy many(pure(5)).runParser("")
    }

    "stateful parsers" should "allow for persistent state" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val p = (put(r1, 5)
              *> put(r2, 7)
              *> put(r1, lift2[Int, Int, Int](_+_, get(r1), get(r2)))
              *> (get(r1) <~> gets(r2, (x: Int) => x+1)))
        p.runParser("") should be (Success((12, 8)))
    }
    they should "be modifiable" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 5) *> modify[Int](r1, _+1) *> get(r1)
        p.runParser("") should be (Success(6))
    }
    they should "provide localised context" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 5) *> (local(r1, (x: Int) => x+1, get(r1)) <~> get(r1))
        val q = put(r1, 5) *> (local(r1, 6, get(r1)) <~> get(r1))
        p.runParser("") should be (Success((6, 5)))
        q.runParser("") should be (Success((6, 5)))
    }
    they should "be correctly allocated when found inside recursion" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[String]
        lazy val rec: Parsley[Unit] = char('a') *> put(r1, 1) *> rec <|> unit
        val p = put(r2, "hello :)") *> rec *> get(r2)
        p.runParser("a") shouldBe Success("hello :)")
    }
    they should "be correctly allocated when found inside sub-routines" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[String]
        val q = char('a') *> put(r1, 1)
        val p = put(r2, "hello :)") *> q *> q *> get(r2)
        p.runParser("aa") shouldBe Success("hello :)")
    }
    they should "be preserved by callee-save in flatMap" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val r3 = Reg.make[String]
        val p = (put(r3, "hello world") *> put(r1, 6)).flatMap(_ => put(r3, "hi") *> put(r2, 4)) *> (get(r1) <~> get(r3))
        p.runParser("") shouldBe Success((6, "hi"))
    }
    they should "be preserved by callee-save in flatMap even when it fails" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val r3 = Reg.make[String]
        val p = put(r3, "hello world") *>
                put(r1, 6) *>
                Combinator.optional(unit.flatMap(_ => put(r3, "hi") *> put(r2, 4) *> Parsley.empty)) *>
                (get(r1) <~> get(r3))
        p.runParser("") shouldBe Success((6, "hi"))
    }
    they should "be able to be rolled back if they fail softly" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 3) *> (rollback(r1, put(r1, 2) *> Parsley.empty) <|> unit) *> get(r1)
        p.runParser("") shouldBe Success(3)
    }
    they should "but not roll back if they hard fail" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 3) *> (rollback(r1, 'a' *> put(r1, 2) *> Parsley.empty) <\> unit) *> get(r1)
        p.runParser("a") shouldBe Success(2)
    }
    they should "not rollback if successful" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 3) *> rollback(r1, put(r1, 2)) *> get(r1)
        p.runParser("") shouldBe Success(2)
    }

    "ternary parsers" should "function correctly" in {
        val p = pure(true)
        (p ?: ('a', 'b')).runParser("a") should be (Success('a'))
    }

    "filtered parsers" should "function correctly" in {
        val p = Char.anyChar.filterNot(_.isLower)
        p.runParser("a") shouldBe a [Failure]
        p.runParser("A") shouldBe Success('A')

        val q = Char.anyChar.guardNot(_.isLower, "letter was not uppercase")
        q.runParser("a") shouldBe Failure("(line 1, column 2):\n  letter was not uppercase")
        q.runParser("A") shouldBe Success('A')

        val r = Char.anyChar.guardNot(_.isLower, c => s"'$c' is not uppercase")
        r.runParser("a") shouldBe Failure("(line 1, column 2):\n  'a' is not uppercase")
        r.runParser("A") shouldBe Success('A')

        val s = Char.anyChar >?> (_.isUpper, "letter was not uppercase")
        s.runParser("a") shouldBe Failure("(line 1, column 2):\n  letter was not uppercase")
        s.runParser("A") shouldBe Success('A')

        val t = Char.anyChar >?> (_.isUpper, c => s"'$c' is not uppercase")
        t.runParser("a") shouldBe Failure("(line 1, column 2):\n  'a' is not uppercase")
        t.runParser("A") shouldBe Success('A')
    }

    "the collect combinator" should "act like a filter then a map" in {
        val p = Char.anyChar.collect {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        p.runParser("+") shouldBe Success(0)
        p.runParser("C") shouldBe Success(3)
        p.runParser("a") shouldBe a [Failure]
    }

    "the cast combinator" should "allow for casts to valid types" in {
        val p = pure[Any](7)
        p.cast[Int].runParser("") shouldBe Success(7)
    }
    it should "reject invalid casts by failing" in {
        val p = pure[Any](7)
        p.cast[String].runParser("") shouldBe a [Failure]
    }

    "stack overflows" should "not occur" in {
        def repeat(n: Int, p: Parsley[Char]): Parsley[Char] = {
            if (n > 0) p *> repeat(n-1, p)
            else p
        }
        noException should be thrownBy repeat(4000, 'a').runParser("a")
    }
    they should "not be thrown by recursive parsers" in {
        lazy val p: Parsley[Int] = p.map((x: Int) => x+1)
        def many_[A](p: Parsley[A]): Parsley[List[A]] = {
            lazy val manyp: Parsley[List[A]] = (p <::> manyp) </> Nil
            manyp
        }
        noException should be thrownBy many_('a' *> p).runParser("")
    }
    they should "not be caused by bind optimisation" in {
        lazy val uhoh: Parsley[Unit] = 'a' >>= (_ => uhoh)
        noException should be thrownBy uhoh.runParser("a")
    }

    "parsers" should "be thread safe when ran correctly" ignore {
        import scala.concurrent.{Future, ExecutionContext, Await, duration}, ExecutionContext.global, duration._
        implicit val ec = global
        lazy val p: Parsley[List[List[Char]]] = (('a' *> many('b' *> 'b' *> 'b')) <::> p) </> Nil
        val as = 1000
        val bs = 2000
        val input = ("a" + "b" * (bs * 3)) * as
        val output = Success(List.fill[Char](as, bs)('b'))

        val futs = Future.traverse(1 to 16)(_ => Future(p.runParser(input)))
        for (out <- Await.result(futs, 10.second)) {
            out should be (output)
        }
    }
}
