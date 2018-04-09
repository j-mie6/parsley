package parsley

import scala.annotation.tailrec

private [parsley] object ParsleyBench
{
    import parsley.Parsley._
    import parsley.Combinator._
    import parsley.Char._
    val liftab: Parsley[String] = lift2[Char, Char, String]((x, y) => x.toString + y.toString, 'a', 'b')
    println(liftab.pretty)
    val aconsb: Parsley[List[Char]] = 'a' <::> ('b' #> Nil)
    println(aconsb.pretty)
    val athenb: Parsley[String] = 'a' *> 'b' #> "ab"
    println(athenb.pretty)
    val manya: Parsley[List[Char]] = many('a') <* 'b'
    println(manya.pretty)
    def chain: Parsley[Int] = chainl1('1' <#> (_.toInt), '+' #> ((x: Int, y: Int) => x + y))
    println(chain.pretty)
    
    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp
    
    // This is an optimisation for the logic inside. Since this is the last in a chain of ors
    // it doesn't need to account for the other symbols (just needs to not accidentally consume ])
    private val whitespaceBF = satisfy(_ != ']')
    
    def brainfuck: Parsley[List[BrainFuckOp]] = 
    {
        lazy val bf: Parsley[List[BrainFuckOp]] = 
            many('>' #> Some(RightPointer)
             <|> '<' #> Some(LeftPointer)
             <|> '+' #> Some(Increment)
             <|> '-' #> Some(Decrement)
             <|> '.' #> Some(Output)
             <|> ',' #> Some(Input)
             <|> between('[', ']' <|> fail("unclosed loop"), bf.map(p => Some(Loop(p))))
             <|> (whitespaceBF #> None)).map(_.flatten)
        attempt(bf <* eof) <|> fail("\"]\" closes a loop, but there isn't one open")
    }
    println(brainfuck.pretty)
}

/*private [parsley] object BenchParser extends scala.util.parsing.combinator.Parsers
{
    import scala.util.parsing.input.{NoPosition, Reader}
    override type Elem = Char
    private val elem: Parser[Int] = accept("1", {case '1' => '1'.toInt})
    private val op: Parser[(Int, Int) => Int] = accept("+", {case '+' => _ + _})
    val bench = chainl1(elem, op)

    private class BenchReader(tokens: String) extends Reader[Elem]
    {
        override def first = tokens.head
        override def atEnd = tokens.isEmpty
        override def pos = NoPosition
        override def rest = new BenchReader(tokens.tail)
    }

    def apply(input: String) = bench(new BenchReader(input))
}*/

/*
// TODO: Test out https://github.com/djspiewak/parseback
object Parseback
{
    ??? 
}
*/

private [parsley] object Native
{
    val recursiveDescent: String => Either[String, Int] = (input: String) => expr(input, 0)._1
    def expr(input: String, index: Int): (Either[String, Int], Int) =
    {
        one(input, index) match
        {
            case (Right(x), index_) => plus(input, index_) match
            {
                case (Right(op), index__) => expr(input, index__) match
                {
                    case (Right(y), index___) => (Right(op(x)(y)), index___)
                    case (err, index___) => (err, index___)
                }
                case (_, index__) => (Right(x), index__)
            }
            case (err, index_) => (err, index_)
        }
    }
    def exprl(input: String, index: Int): (Either[String, Int], Int) =
    {
        one(input, index) match
        {
            case (Right(x), index_) =>
                val (ops, index__) = rep(plusone)(input, index_)
                (Right(ops.foldLeft(x)((acc, op) => op(acc))), index__)
            case err => err
        }
    }
    @tailrec def rep[A](p: (String, Int) => (Either[String, A], Int))
                       (input: String, index: Int, acc: List[A] = Nil): (List[A], Int) = p(input, index) match
    {
        case (Right(x), index_) => rep(p)(input, index_, x::acc)
        case (_, index_) => (acc.reverse, index_)
    }
    def one(input: String, index: Int): (Either[String, Int], Int) =
    {
        if (index < input.length && input(index) == '1') (Right('1'.toInt), index + 1)
        else (Left(s"$index: Expected 1, got ${if (index < input.length) input(index) else "end of input"}"), index)
    }
    def plus(input: String, index: Int): (Either[String, Int => Int => Int], Int) =
    {
        if (index < input.length && input(index) == '+') (Right((x: Int) => (y: Int) => x + y), index + 1)
        else (Left(s"$index: Expected +, got ${if (index < input.length) input(index) else "end of input"}"), index)
    }
    def plusone(input: String, index: Int): (Either[String, Int => Int], Int) =
    {
        plus(input, index) match
        {
            case (Right(op), index_) => one(input, index_) match
            {
                case (Right(y), index__) => (Right((z: Int) => op(z)(y)), index__)
                case (Left(err), index__) => (Left(err), index__)
            }
            case (Left(err), index__) => (Left(err), index__)
        }
    }

    val parseTail: String => Int = (input: String) => parseTail_(input, 0, 0)
    @tailrec def parseTail_(input: String, index: Int, sum: Int): Int =
    {
        if (index >= input.length) sum
        else input(index) match
        {
            case c@'1' => parseTail_(input, index + 1, sum + c)
            case '+' => parseTail_(input, index + 1, sum)
        }
    }
}

/*private [parsley] object FastParser
{
    import fastparse.all._
    val x = P("1").!.map(_(0).toInt)
    val y = P("+").!.map(_ => (x: Int) => (y: Int) => x + y)
    def chainlf[A](p: Parser[A], op: Parser[A => A => A]): Parser[A] =
    {
        for (x <- p;
             fs <- (for (f <- op;
                         y <- p)
                 yield ((x: A) => f(x)(y))).rep)
            yield fs.foldLeft(x)((y, f) => f(y))
        //val ops = (op ~ p).map{case (f, x) => (y: A) => f(y)(x)}
        //(p ~ ops.rep).map{case (x, (xs: Seq[A=>A])) => xs.foldLeft(x)((y, f) => f(y))}
    }
    val z = chainlf(x, y)
    def repeat[A](p: Parser[A], n: Int): Parser[A] =
    {
        if (n > 0) for (_ <- p; x <- repeat(p, n-1)) yield x
        else p
    }
    val big = repeat(P("1"), 5000)

    trait BrainFuckOp
    case object RightPointer extends BrainFuckOp
    case object LeftPointer extends BrainFuckOp
    case object Increment extends BrainFuckOp
    case object Decrement extends BrainFuckOp
    case object Output extends BrainFuckOp
    case object Input extends BrainFuckOp
    case class Loop(p: List[BrainFuckOp]) extends BrainFuckOp

    // This is an optimisation for the logic inside. Since this is the last in a chain of ors
    // it doesn't need to account for the other symbols (just needs to not accidentally consume ])
    private val whitespaceBF = P(CharPred(_ != ']'))

    def brainfuck: Parser[List[BrainFuckOp]] =
    {
        lazy val bf: Parser[List[BrainFuckOp]] =
            (P(">").map(_ => Some(RightPointer))
           | P("<").map(_ => Some(LeftPointer))
           | P("+").map(_ => Some(Increment))
           | P("-").map(_ => Some(Decrement))
           | P(".").map(_ => Some(Output))
           | P(",").map(_ => Some(Input))
           | P("[" ~ bf ~ "]").map(p => Some(Loop(p)))
           | whitespaceBF.map(_ => None)).rep.map(_.flatten.toList)
        bf ~ End
    }
}*/

private [parsley] object Benchmark
{
    def main(args: Array[String]): Unit =
    {
        //Console.in.read()
        val p = ParsleyBench.brainfuck
        val input1 = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"
        val input2 = "[+++++++<[<<>>>>]..hahah this is brainfuck.,,,,,-[---]-++]"
        val input = input2
        //println(runParser(p, "aaaab"))
        //println(runParser(p, "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"))
        println(runParserFastUnsafe(p, input))
        val start = System.currentTimeMillis()
        //println(runParser(p, input))
        for (_ <- 0 to 1000000)
            runParserFastUnsafe(p, input)
            //p(input)
            //p.parse(input)
        println(System.currentTimeMillis() - start)
    }
}