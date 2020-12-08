package parsley

/**
* Result of a parser. Either a `Success[A]` or a `Failure`
* @tparam A The type of expected success result
*/
//TODO: Make these a bit more full fledged
sealed abstract class Result[+A]
{
    def toOption: Option[A]
    def toEither: Either[String, A]
}

/**
* Returned when a parser succeeded.
* @param x The result value of the successful parse
* @tparam A The type of expected success result
*/
case class Success[A] private [parsley] (x: A) extends Result[A]
{
    override def toOption: Option[A] = Some(x)
    override def toEither: Either[String, A] = Right(x)
}

/**
* Returned on parsing failure
* @param msg The error message reported by the parser
*/
case class Failure private [parsley] (msg: String) extends Result[Nothing]
{
    override def toOption: Option[Nothing] = None
    override def toEither: Either[String, Nothing] = Left(msg)
}