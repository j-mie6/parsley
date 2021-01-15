package parsley

import parsley.character.digit
import parsley.implicits.{charLift, stringLift}
import parsley.Combinator.{chainPost, chainPre, chainl1, chainr1, chainl, chainr}
import parsley.expr.{precedence, Ops, GOps, Levels, Level, InfixL, InfixR, Prefix, Postfix}
import parsley.Parsley._
import parsley._

import scala.language.implicitConversions

class ExpressionParserTests extends ParsleyTest {
    "chainPost" must "require an initial value" in {
        chainPost('1' #> 1, '+' #> ((x: Int) => x + 1)).runParser("1") should be (Success(1))
    }
    it must "parse all operators that follow" in {
        chainPost('1' #> 1, '+' #> ((x: Int) => x + 1)).runParser("1++++++++++++++") should not be a [Failure]
    }
    it must "apply the functions" in {
        chainPost('1' #> 1, '+' #> ((x: Int) => x + 1)).runParser("1++++++++++++++") should be (Success(15))
    }
    it must "fail if an operator fails after consuming input" in {
        chainPost('1' #> 1, "++" #> ((x: Int) => x + 1)).runParser("1+++++++++++++") shouldBe a [Failure]
    }
    it must "not leave the stack in an inconsistent state on failure" in {
        val p = chainPost[Int]('1' #> 1, (col.#>[Int => Int](_ + 1)) <* '+')
        val q = chainl1[Int, Int](p, '*' #> (_ * _))
        noException should be thrownBy q.runParser("1+*1+")
    }

    "chainPre" must "parse an operatorless value" in {
        chainPre('+' #> ((x: Int) => x + 1), '1' #> 1).runParser("1") should be (Success(1))
    }
    it must "parse all operators that precede a value" in {
        chainPre('+' #> ((x: Int) => x + 1), '1' #> 1).runParser("+++++++++++1") should not be a [Failure]
    }
    it must "fail if the final value is absent" in {
        chainPre('+' #> ((x: Int) => x + 1), '1' #> 1).runParser("+++++++++++") shouldBe a [Failure]
    }
    it must "apply the functions" in {
        chainPre('+' #> ((x: Int) => x + 1), '1' #> 1).runParser("+++++++++++1") should be (Success(12))
    }

    "chainr1" must "require an initial value" in {
        chainr1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("11") should be (Success(1))
        chainr1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("1") shouldBe a [Failure]
        chainr1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("2") shouldBe a [Failure]
    }
    it must "parse all operators and values that follow" in {
        chainr1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in {
        chainr1(digit.map(_.asDigit), '%' #> ((x: Int, y: Int) => x % y)).runParser("6%5%2%7") should be (Success(0))
    }
    it must "fail if an operator or p fails after consuming input" in {
        chainr1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).runParser("11+11+11+11+11") shouldBe a [Failure]
        chainr1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).runParser("11++11++11++1++11") shouldBe a [Failure]
    }
    it must "correctly accept the use of a wrapping function" in {
        sealed trait Expr
        case class Add(x: Int, y: Expr) extends Expr
        case class Num(x: Int) extends Expr
        val p = chainr1[Int, Expr]("1" #> 1, "+" #> Add.apply)(Num)
        p.runParser("1+1+1") should be (Success(Add(1, Add(1, Num(1)))))
        p.runParser("1") should be (Success(Num(1)))
    }
    "chainr" must "allow for no initial value" in {
        chainr("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).runParser("2") shouldBe Success(0)
        chainr("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).runParser("1") shouldBe a [Failure]
    }

    "chainl1" must "require an initial value" in {
        chainl1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("11") should be (Success(1))
        chainl1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("1") shouldBe a [Failure]
        chainl1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("2") shouldBe a [Failure]
    }
    it must "parse all operators and values that follow" in {
        chainl1("11" #> 1, '+' #> ((x: Int, y: Int) => x + y)).runParser("11+11+11+11+11") should be (Success(5))
    }
    it must "apply the functions with correct associativity" in {
        chainl1(digit.map(_.asDigit), '%' #> ((x: Int, y: Int) => x % y)).runParser("6%5%2%7") should be (Success(1))
    }
    it must "fail if an operator fails after consuming input" in {
        chainl1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).runParser("11+11+11+11+11") shouldBe a [Failure]
        chainl1("11" #> 1, "++" #> ((x: Int, y: Int) => x + y)).runParser("11++11++11++1++11") shouldBe a [Failure]
    }
    it must "not leave the stack in an inconsistent state on failure" in {
        val p = chainl1[Int, Int]('1' #> 1, (col.#>[(Int, Int) => Int](_ + _)) <* '+')
        val q = chainl1[Int, Int](p, '*' #> (_ * _))
        noException should be thrownBy q.runParser("1+1*1+1")
    }
    it must "correctly accept the use of a wrapping function" in {
        sealed trait Expr
        case class Add(x: Expr, y: Int) extends Expr
        case class Num(x: Int) extends Expr
        chainl1[Int, Expr]("1" #> 1, "+" #> Add.apply)(Num).runParser("1+1+1") should be (Success(Add(Add(Num(1), 1), 1)))
    }
    "chainl" must "allow for no initial value" in {
        chainl("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).runParser("11") should be (Success(1))
        chainl("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).runParser("1") shouldBe a [Failure]
        chainl("11" #> 1, '+' #> ((x: Int, y: Int) => x + y), 0).runParser("2") shouldBe Success(0)
    }

    "expression parsers" should "result in correct precedence" in {
        val expr = precedence[Int](digit.map(_.asDigit), Ops(InfixL)('*' #> (_*_)),
                                                         Ops(InfixL)('+' #> (_+_)))
        expr.runParser("1+2*3+4") should be (Success(11))
        expr.runParser("1*2+3*4") should be (Success(14))
    }
    they should "work for multiple operators at the same level" in {
        val expr = precedence[Int](digit.map(_.asDigit), Ops(InfixL)('+' #> (_+_), '-' #> (_-_)))
        expr.runParser("1+2-3+4") should be (Success(4))
        expr.runParser("1-2+3-4") should be (Success(-2))
    }
    they should "work for mixed associativity operators" in {
        val expr = precedence[Int](digit.map(_.asDigit), Ops(InfixL)('*' #> (_*_)),
                                                         Ops(InfixR)('+' #> (_+_)))
        expr.runParser("1+2*3+4") should be (Success(11))
        expr.runParser("1*2+3*4") should be (Success(14))
    }
    they should "parse mathematical expressions" in {
        lazy val expr: Parsley[Int] = precedence[Int](atom,
            Ops(Prefix)('-' #> (x => -x)),
            Ops(InfixL)('/' #> (_/_)),
            Ops(InfixR)('*' #> (_*_)),
            Ops(InfixL)('+' #> (_+_), '-' #> (_-_)))
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr <* ')')
        expr.runParser("(2+3)*8") should be (Success(40))
        expr.runParser("-3+4") should be (Success(1))
        expr.runParser("-(3+4)") should be (Success(-7))
        expr.runParser("(3+-7)*(-2--4)/2") should be (Success(-4))
    }
    they should "parse prefix operators mixed with infix operators" in {
        lazy val expr = precedence[Int](atom, Ops(Prefix)('-' #> (x => -x)),
                                              Ops(InfixL)('-' #> (_-_)))
        lazy val atom: Parsley[Int] = digit.map(_.asDigit) <|> ('(' *> expr <* ')')
        expr.runParser("-1") should be (Success(-1))
        expr.runParser("2-1") should be (Success(1))
        expr.runParser("-2-1") should be (Success(-3))
        expr.runParser("-(2-1)") should be (Success(-1))
        expr.runParser("(-0)-1") should be (Success(-1))
    }
    they should "be able to parse prefix operators weaker than an infix" in {
        sealed trait Expr
        case class Lt(x: Expr, y: Expr) extends Expr
        case class Inc(x: Expr) extends Expr
        case class Num(x: Int) extends Expr
        val expr = precedence[Expr](digit.map(_.asDigit).map(Num), Ops(InfixL)('<' #> Lt),
                                                                   Ops(Prefix)("++" #> Inc))
        expr.runParser("++1<2") should be (Success(Inc(Lt(Num(1), Num(2)))))
    }
    they should "generalise to non-monolithic structures" in {
        sealed trait Expr
        case class Add(x: Expr, y: Term) extends Expr
        case class ExprOf(x: Term) extends Expr
        sealed trait Term
        case class Mul(x: Atom, y: Term) extends Term
        case class TermOf(x: Atom) extends Term
        sealed trait Atom
        case class Parens(x: Expr) extends Atom
        case class Num(x: Int) extends Atom
        lazy val atom: Parsley[Atom] = digit.map(_.asDigit).map(Num) <|> ('(' *> expr.map(Parens) <* ')')
        lazy val expr = precedence[Atom, Expr](atom,
            GOps[Atom, Term](InfixR)('*' #> Mul.apply)(TermOf) +:
            GOps[Term, Expr](InfixL)('+' #> Add.apply)(ExprOf) +:
            Levels.empty[Expr])
        expr.runParser("(7+8)*2+3+6*2") should be (Success(Add(Add(ExprOf(Mul(Parens(Add(ExprOf(TermOf(Num(7))), TermOf(Num(8)))), TermOf(Num(2)))), TermOf(Num(3))), Mul(Num(6), TermOf(Num(2))))))
    }

    "mixed expressions" should "also be parsable" in {
        val lang = token.LanguageDef.plain.copy(
            identStart = token.Predicate(_.isLetter),
            identLetter = token.Predicate(_.isLetter)
        )

        sealed trait Expr
        case class Binary(l: Expr, r: Expr) extends Expr
        case class Unary(c: Expr) extends Expr
        case class Constant(x: String) extends Expr

        val tok = new token.Lexer(lang)

        lazy val ops: List[Ops[Expr, Expr]] = List(
            Ops(Postfix)(tok.parens(expr </> Constant("")) <#> (e1 => (e2: Expr) => Binary(e2, e1))),
            Ops(InfixL)('.' #> Binary),
            Ops(InfixR)(".=" #> Binary),
            Ops(InfixR)(',' #> Binary)
        )

        lazy val atom: Parsley[Expr] = tok.identifier.map(Constant)
        lazy val expr: Parsley[Expr] = precedence(atom, ops: _*)

        expr.runParser("o.f()") shouldBe a [Success[_]]
        expr.runParser("o.f(x,y)") shouldBe a [Success[_]]
    }
}
