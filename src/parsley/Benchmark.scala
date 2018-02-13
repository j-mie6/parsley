package parsley

object ParsleyBench
{
    import parsley.Parsley._
    val liftab: Parsley[String] = lift2_[Char, Char, String]((x, y) => x.toString + y.toString, 'a', 'b')
    println(liftab)
    reset()
    val aconsb: Parsley[List[Char]] = 'a' <::> ('b' #> Nil)
    println(aconsb)
    reset()
    val athenb: Parsley[String] = 'a' *> 'b' #> "ab"
    println(athenb)
    reset()
    val manya: Parsley[List[Char]] = many('a') <* 'b'
    println(manya)
    reset()
    val chain: Parsley[Int] = chainl1('1' <#> (_.toInt), '+' #> ((x: Int) => (y: Int) => x + y))
    println(chain)
    reset()
}

/*object BenchParser extends scala.util.parsing.combinator.Parsers
{
    import scala.util.parsing.input.{NoPosition, Reader}
    override type Elem = Char
    private val elem: Parser[Int] = accept("1", {case '1' => '1'.toInt})
    private val op: Parser[(Int, Int) => Int] = accept("+", {case '+' => _ + _})
    val bench = chainl1(elem, op)

    private class BenchReader(tokens: List[Elem]) extends Reader[Elem]
    {
        override def first = tokens.head
        override def atEnd = tokens.isEmpty
        override def pos = NoPosition
        override def rest = new BenchReader(tokens.tail)
    }

    def apply(input: List[Elem]) = bench(new BenchReader(input))
}

object FastParser
{
    import fastparse.all._
    val x = P("1").!.map(_(0).toInt)
    val y = P("+").!.map(_ => ((x: Int) => (y: Int) => x + y))
    def chainlf[A](p: Parser[A], op: Parser[A => A => A]): Parser[A] =
    {
        for (x <- p;
             fs <- (for (f <- op;
                         y <- p)
                 yield ((x: A) => f(x)(y))).rep)
            yield fs.foldLeft(x)((y, f) => f(y))
    }
    val z = chainlf(x, y)
}*/

object Benchmark
{
    def main(args: Array[String]): Unit =
    {
        //Console.in.read()
        val p = ParsleyBench.chain
        val input1 = "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"
        //val input2 = "aaaab"
        val input = input1
        println(runParser(p, "aaaab"))
        println(runParser(p, "1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1"))
        val start = System.currentTimeMillis()
        for (_ <- 0 to 10000000)
            runParser(p, input)
            //p(input_)
            //p.parse(input)
        println(System.currentTimeMillis() - start)
    }
}