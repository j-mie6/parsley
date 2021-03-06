package parsley

import parsley.Parsley._
import parsley.combinator.{many, manyUntil}
import parsley.lift._
import parsley.character.{char, satisfy, digit, anyChar, string}
import parsley.implicits.{charLift, stringLift, Lift1}
import parsley.registers._
import parsley.io._

import scala.language.implicitConversions

import java.io.File

class CoreTests extends ParsleyTest {
    private val add: (Int, Int) => Int = _+_
    private val add1 = (x: Int) => x + 1
    private val mult5 = (x: Int) => x * 5
    private val toUpper = (c: Char) => c.toUpper

    "Character parsers" should "fail on no input" in {
        'a'.parse("") should not be a [Success[_]]
    }

    they should "fail when given incorrect input" in {
        'a'.parse("b") should not be a [Success[_]]
    }

    they should "succeed when given correct input" in {
        'a'.parse("a") should be (Success('a'))
    }

    they must "only consume a single character of input at most" in {
        var res = (satisfy(_ == 'a') *> 'b').parse("aaaaaa")
        res shouldBe a [Failure]
        res match {
            case Failure(err) => err should startWith ("(line 1, column 2)")
            case _ =>
        }
        res = ('a' ~> 'b').parse("bc")
        res shouldBe a [Failure]
        res match {
            case Failure(err) => err should startWith ("(line 1, column 1)")
            case _ =>
        }
    }

    "Pure parsers" should "not require input" in {
        unit.parse("") should not be a [Failure]
    }

    they must "result in their correct value" in {
        pure(42).parse("") should be (Success(42))
        pure("hello world").parse("") should be (Success("hello world"))
        pure(add).parse("") should be (Success(add))
    }

    they must "not consume any input" in {
        (pure('a') <~ 'a').parse("a") should be (Success('a'))
    }

    // APPLICATIVE LAWS
    they must "obey the homomorphism law: pure f <*> pure x = pure (f x)" in {
        (pure(add1) <*> pure(42)).parse("") should be (Success(43))
        (pure(42) <**> pure(add1)).parse("") should be (Success(43))
    }

    they must "obey the fmap law: pure f <*> p = f <$> p" in {
        (pure(toUpper) <*> 'a').parse("a") should equal ((toUpper.lift('a')).parse("a"))
        (pure(toUpper) <*> ('a' <|> 'b')).parse("a") should equal ((toUpper <#> ('a' <|> 'b')).parse("a"))
        (pure(toUpper) <*> ('a' <|> 'b')).parse("b") should equal ((toUpper <#> ('a' <|> 'b')).parse("b"))
    }

    they must "obey the interchange law: u <*> pure x = ($x) <$> u" in {
        (('a' #> add1) <*> pure(41)).parse("a") should equal ((((f: Int => Int) => f(41)) <#> ('a' #> add1)).parse("a"))
        ((('a' <|> 'b') #> add1) <*> pure(41)).parse("a") should equal ((((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1)).parse("a"))
        ((('a' <|> 'b') #> add1) <*> pure(41)).parse("b") should equal ((((f: Int => Int) => f(41)) <#> (('a' <|> 'b') #> add1)).parse("b"))
    }

    they must "obey the composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" in {
        val u: Parsley[Int => Int] = 'a' #> add1
        val v: Parsley[Int => Int] = 'b' #> mult5
        val w: Parsley[Int] = 'c' #> 7
        val compose: (Int => Int) => (Int => Int) => Int => Int = f => g => f.compose(g)
        (pure(compose) <*> u <*> v <*> w).parse("abc") should equal ((u <*> (v <*> w)).parse("abc"))
    }

    "lift22" must "work correctly (and by extension 21-3)" in {
        val p = lift.lift22[Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char,
                            Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, String](
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => s"$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o$p$q$r$s$t$u$v",
            anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar,
            anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar, anyChar)
        p.parse("abcdefghijklmnopqrstuv") shouldBe Success("abcdefghijklmnopqrstuv")
    }

    // SELECTIVE LAWS
    they must "obey the selective left-branch law" in {
        branch[Int, Int, Int](pure(Left(7)), pure(_+1), pure(_-1)).parse("") shouldBe Success(8)
    }
    they must "obey the selective right-branch law" in {
        select(pure(Right(7)), Parsley.empty).parse("") shouldBe Success(7)
    }

    // MONAD LAWS
    they must "obey the left identity law: pure x >>= f = f x" in {
        (pure('a') >>= char).parse("a") should equal ('a'.parse("a"))
    }
    "Parsers" must "obey the right identity law: m >>= pure = m" in {
        ('a' >>= pure).parse("a") should equal ('a'.parse("a"))
    }
    they must "obey the associativity law: (m >>= f) >>= g = m >>= (x => f x >>= g)" in {
        val f: Int => Parsley[Int] = x => pure(x + 1)
        val g: Int => Parsley[Int] = x => pure(x/3)
        val m = '1' #> 4
        ((m >>= f) >>= g).parse("1") should equal ((m >>= (x => f(x) >>= g)).parse("1"))
    }
    they must "allow for flattening" in {
        join(pure(char('a'))).parse("a") shouldBe Success('a')
        join(Parsley.empty).parse("") shouldBe a [Failure]
    }

    "mzero parsers" should "always fail" in {
        (Parsley.empty ~> 'a').parse("a") shouldBe a [Failure]
        (Parsley.fail("") ~> 'a').parse("a") shouldBe a [Failure]
        (Parsley.unexpected("") *> 'a').parse("a") shouldBe a [Failure]
        (('a' ! (_ => "")) *> 'b').parse("ab") shouldBe a [Failure]
        ('a'.unexpected(_ => "") *> 'b').parse("ab") shouldBe a [Failure]
    }

    "<|>" should "not try the second alternative if the first succeeded" in {
        ('a' <|> Parsley.fail("wrong!")).parse("a") should not be a [Failure]
    }
    it should "only try second alternative if the first failed without consuming input" in {
        ('a' <|> 'b').parse("b") should not be a [Failure]
    }
    it should "not try the second alternative if the first failed after consuming input" in {
        ("ab" <|> "ac").parse("ac") shouldBe a [Failure]
    }
    it should "not be affected by an empty on the left" in {
        (Parsley.empty <|> 'a').parse("b") shouldBe Failure("(line 1, column 1):\n  unexpected \"b\"\n  expected \"a\"\n  >b\n  >^")
    }

    "attempt" should "cause <|> to try second alternative even if input consumed" in {
        attempt("ab").orElse("ac").parse("ac") should not be a [Failure]
    }

    "lookAhead" should "consume no input on success" in {
        lookAhead('a').parse("a") should not be a [Failure]
        (lookAhead('a') *> 'b').parse("ab") should be (Failure("(line 1, column 1):\n  unexpected \"a\"\n  expected \"b\"\n  >ab\n  >^"))
    }
    it must "fail when input is consumed, and input is consumed" in {
        lookAhead("ab").parse("ac") shouldBe a [Failure]
    }
    /*it should "not affect the state of the registers on success" in {
        val r1 = Reg.make[Int]
        (put(r1, 5) *> lookAhead(put(r1, 7) *> 'a') *> get(r1)).parse("a") should be {
            Success(5)
        }
        (put(r1, 5) *> (lookAhead(put(r1, 7) *> 'a') <|> 'b') *> get(r1)).parse("b") should be {
            Success(7)
        }
    }*/

    "many" should "crash when given a parser that does not consume input" in {
       an [Exception] should be thrownBy many(pure(5)).parse("")
    }

    "stateful parsers" should "allow for persistent state" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val p = (put(r1, 5)
              *> put(r2, 7)
              *> put(r1, lift2[Int, Int, Int](_+_, get(r1), get(r2)))
              *> (get(r1) zip gets(r2, (x: Int) => x+1)))
        p.parse("") should be (Success((12, 8)))
    }
    they should "be modifiable" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 5) *> modify[Int](r1, _+1) *> get(r1)
        p.parse("") should be (Success(6))
    }
    they should "provide localised context" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 5) *> (local(r1, (x: Int) => x+1, get(r1)) zip get(r1))
        val q = put(r1, 5) *> (local(r1, 6, get(r1)) <~> get(r1))
        p.parse("") should be (Success((6, 5)))
        q.parse("") should be (Success((6, 5)))
    }
    they should "be correctly allocated when found inside recursion" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[String]
        lazy val rec: Parsley[Unit] = char('a') *> put(r1, 1) *> rec <|> unit
        val p = put(r2, "hello :)") *> rec *> get(r2)
        p.parse("a") shouldBe Success("hello :)")
    }
    they should "be correctly allocated when found inside sub-routines" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[String]
        val q = char('a') *> put(r1, 1)
        val p = put(r2, "hello :)") *> q *> q *> get(r2)
        p.parse("aa") shouldBe Success("hello :)")
    }
    they should "be preserved by callee-save in flatMap" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val r3 = Reg.make[String]
        val p = (put(r3, "hello world") *> put(r1, 6)).flatMap(_ => put(r3, "hi") *> put(r2, 4)) *> (get(r1) <~> get(r3))
        p.parse("") shouldBe Success((6, "hi"))
    }
    they should "be preserved by callee-save in flatMap even when it fails" in {
        val r1 = Reg.make[Int]
        val r2 = Reg.make[Int]
        val r3 = Reg.make[String]
        val p = put(r3, "hello world") *>
                put(r1, 6) *>
                combinator.optional(unit.flatMap(_ => put(r3, "hi") *> put(r2, 4) *> Parsley.empty)) *>
                (get(r1) zip get(r3))
        p.parse("") shouldBe Success((6, "hi"))
    }
    they should "be able to be rolled back if they fail softly" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 3) *> (rollback(r1, put(r1, 2) *> Parsley.empty) <|> unit) *> get(r1)
        p.parse("") shouldBe Success(3)
    }
    they should "but not roll back if they hard fail" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 3) *> (rollback(r1, 'a' *> put(r1, 2) *> Parsley.empty) <\> unit) *> get(r1)
        p.parse("a") shouldBe Success(2)
    }
    they should "not rollback if successful" in {
        val r1 = Reg.make[Int]
        val p = put(r1, 3) *> rollback(r1, put(r1, 2)) *> get(r1)
        p.parse("") shouldBe Success(2)
    }

    "ternary parsers" should "function correctly" in {
        val p = pure(true)
        (p ?: ('a', 'b')).parse("a") should be (Success('a'))
    }

    "filtered parsers" should "function correctly" in {
        val p = anyChar.filterNot(_.isLower)
        p.parse("a") shouldBe a [Failure]
        p.parse("A") shouldBe Success('A')

        val q = anyChar.filterOut {
            case c if c.isLower => s"'$c' should have been uppercase"
        }
        q.parse("a") shouldBe Failure("(line 1, column 2):\n  'a' should have been uppercase\n  >a\n  > ^")
        q.parse("A") shouldBe Success('A')

        val r = anyChar.guardAgainst {
            case c if c.isLower => s"'$c' is not uppercase"
        }
        r.parse("a") shouldBe Failure("(line 1, column 2):\n  'a' is not uppercase\n  >a\n  > ^")
        r.parse("A") shouldBe Success('A')
    }

    // Issue #70
    "filterOut" should "not corrupt the stack under a handler" in {
        val p = attempt(anyChar.filterOut {
            case c if c.isLower => "no lowercase!"
        })
        p.parse("a") shouldBe a [Failure]
    }

    "the collect combinator" should "act like a filter then a map" in {
        val p = anyChar.collect[Int] {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        p.parse("+") shouldBe Success(0)
        p.parse("C") shouldBe Success(3)
        p.parse("a") shouldBe a [Failure]

        val q = anyChar.collectMsg("oops") {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        q.parse("+") shouldBe Success(0)
        q.parse("C") shouldBe Success(3)
        q.parse("a") shouldBe Failure("(line 1, column 2):\n  oops\n  >a\n  > ^")

        val r = anyChar.collectMsg(c => s"$c is not appropriate") {
            case '+' => 0
            case c if c.isUpper => c - 'A' + 1
        }
        r.parse("+") shouldBe Success(0)
        r.parse("C") shouldBe Success(3)
        r.parse("a") shouldBe Failure("(line 1, column 2):\n  a is not appropriate\n  >a\n  > ^")
    }

    "the cast combinator" should "allow for casts to valid types" in {
        val p = pure[Any](7)
        p.cast[Int].parse("") shouldBe Success(7)
    }
    it should "reject invalid casts by failing" in {
        val p = pure[Any](7)
        p.cast[String].parse("") shouldBe a [Failure]
    }

    "foldRight" should "work correctly" in {
        val p = 'a'.foldRight[List[Char]](Nil)(_::_)

        p.parse("") should be (Success(Nil))
        p.parse("aaa") should be (Success(List('a', 'a', 'a')))
    }
    "foldRight1" should "work correctly" in {
        val p = 'a'.foldRight1[List[Char]](Nil)(_::_)
        p.parse("") shouldBe a [Failure]
        p.parse("aaa") should be (Success(List('a', 'a', 'a')))
    }

    "foldLeft" should "work correctly" in {
        val p = digit.foldLeft(0)((x, d) => x * 10 + d.asDigit)

        p.parse("") should be (Success(0))
        p.parse("123") should be (Success(123))
    }
    "foldLeft1" should "work correctly" in {
        val p = digit.foldLeft1(0)((x, d) => x * 10 + d.asDigit)
        p.parse("") shouldBe a [Failure]
        p.parse("123") should be (Success(123))
    }
    "reduceRightOption" should "return Some on success" in {
        val p = string("a").reduceRightOption((s1, s2) => s"$s1($s2)")
        p.parse("aaaaa") shouldBe Success(Some("a(a(a(a(a))))"))
    }
    it should "return None if there is no p" in {
        val p = string("a").reduceRightOption((s1, s2) => s"$s1($s2)")
        p.parse("") shouldBe Success(None)
    }
    "reduceLeftOption" should "return Some on success" in {
        val p = string("a").reduceLeftOption((s1, s2) => s"($s1)$s2")
        p.parse("aaaaa") shouldBe Success(Some("((((a)a)a)a)a"))
    }
    it should "return None if there is no p" in {
        val p = string("a").reduceLeftOption((s1, s2) => s"($s1)$s2")
        p.parse("") shouldBe Success(None)
    }

    "stack overflows" should "not occur" in {
        def repeat(n: Int, p: Parsley[Char]): Parsley[Char] = {
            if (n > 0) p *> repeat(n-1, p)
            else p
        }
        noException should be thrownBy repeat(4000, 'a').parse("a")
    }
    they should "not be thrown by recursive parsers" in {
        lazy val p: Parsley[Int] = p.map((x: Int) => x+1)
        def many_[A](p: Parsley[A]): Parsley[List[A]] = {
            lazy val manyp: Parsley[List[A]] = (p <::> manyp) </> Nil
            manyp
        }
        noException should be thrownBy many_('a' *> p).parse("")
    }
    they should "not be caused by bind optimisation" in {
        lazy val uhoh: Parsley[Unit] = 'a' >>= (_ => uhoh)
        noException should be thrownBy uhoh.parse("a")
    }

    "parseFromFile" should "work" in {
        (manyUntil(anyChar, "Jamie Willis") *> anyChar).parseFromFile(new File("LICENSE")).get shouldBe Success('\n')
    }
}
