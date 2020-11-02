package parsley.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.typelevel.jawn.ast._
import parsley._, Combinator._, Parsley._
import scala.io.Source

/* Based on https://github.com/typelevel/jawn/blob/v1.0.0/benchmark/src/main/scala/jawn/JmhBenchmarks.scala */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class JmhBenchmarks(name: String) {
  val text: String =
    Source.fromResource(name, getClass.getClassLoader).getLines().mkString("\n")

  @Benchmark
  def parsleyParseCold(): JValue =
    runParserThreadSafe(JsonBench.coldJson, text).toOption.get

  @Benchmark
  def parsleyParseHot(): JValue =
    parsley.runParser(JsonBench.hotJson, text) match {
      case Success(x) => x
      case Failure(e) => sys.error(e.toString)
    }
}

object JsonBench {
  def coldJson: Parsley[JValue] = {
    val jsontoks = LanguageDef.plain.copy(space = Predicate(Char.isWhitespace))
    val tok = new TokenParser(jsontoks)
    lazy val obj: Parsley[JValue] = tok.braces(tok.commaSep(tok.stringLiteral <~> tok.colon *> value).map(pairs => JObject.fromSeq(pairs)))
    lazy val array: Parsley[JValue] = tok.brackets(tok.commaSep(value)).map(list => JArray.fromSeq(list))
    lazy val value: Parsley[JValue] =
      (tok.stringLiteral.map(JString.apply)
        <|> tok.symbol("true") #> JTrue
        <|> tok.symbol("false") #> JFalse
        <|> tok.symbol("null") #> JNull
        <|> array
        <|> attempt(tok.float).map(JNum.apply)
        <|> tok.integer.map(i => JNum.apply(i.toLong))
        <|> obj)

    tok.whiteSpace *> (obj <|> array) <* eof
  }

  // Stable instance to warm up
  val hotJson: Parsley[JValue] = {
    val p = coldJson
    coldJson.force()
    p
  }
}

class BarBench extends JmhBenchmarks("bar.json")
class Qux2Bench extends JmhBenchmarks("qux2.json")
class Bla25Bench extends JmhBenchmarks("bla25.json")
class CountriesBench extends JmhBenchmarks("countries.geo.json")
class Ugh10kBench extends JmhBenchmarks("ugh10k.json")
