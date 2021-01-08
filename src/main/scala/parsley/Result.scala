package parsley

import scala.util.{Try, Success => TSuccess, Failure => TFailure}

/**
* Result of a parser. Either a `Success[A]` or a `Failure`
* @tparam A The type of expected success result
*/
sealed abstract class Result[+A]
{
    /** Applies `fa` if this is a `Failure` or `fb` if this is a `Success`.
    *
    *  @param ferr the function to apply if this is a `Failure`
    *  @param fa the function to apply if this is a `Success`
    *  @return the results of applying the function
    */
    def fold[B](ferr: String => B, fa: A => B): B = this match {
        case Success(x)   => fa(x)
        case Failure(msg) => ferr(msg)
    }

    /** Executes the given side-effecting function if this is a `Success`.
    *
    *  @param f The side-effecting function to execute.
    */
    def foreach[U](f: A => U): Unit = this match {
        case Success(x) => f(x)
        case _          =>
    }

    /** Returns the results's value.
    *
    *  @note The result must not be a failure.
    *  @throws java.util.NoSuchElementException if the result is a failure.
    */
    def get: A

    /** Returns the value from this `Success` or the given argument if this is a `Failure`.  */
    def getOrElse[B >: A](or: =>B): B = orElse(Success(or)).get

    /** Returns this `Success` or the given argument if this is a `Failure`. */
    def orElse[B >: A](or: =>Result[B]): Result[B] = this match {
        case Success(_) => this
        case _          => or
    }

    /** Returns `true` if this is a `Success` and its value is equal to `elem` (as determined by `==`),
    *  returns `false` otherwise.
    *
    *  @param elem    the element to test.
    *  @return `true` if this is a `Success` value equal to `elem`.
    */
    final def contains[B >: A](elem: B): Boolean = exists(_ == elem)

    /** Returns `true` if `Failure` or returns the result of the application of
    *  the given predicate to the `Success` value.
    */
    def forall(f: A => Boolean): Boolean = this match {
        case Success(x) => f(x)
        case _          => true
    }

    /** Returns `false` if `Failure` or returns the result of the application of
    *  the given predicate to the `Success` value.
    */
    def exists(p: A => Boolean): Boolean = this match {
        case Success(x) => p(x)
        case _          => false
    }

    /** Binds the given function across `Success`.
    *
    *  @param f The function to bind across `Success`.
    */
    def flatMap[B](f: A => Result[B]): Result[B] = this match {
        case Success(x) => f(x)
        case _          => this.asInstanceOf[Result[B]]
    }

    /** Returns the right value if this is right
        * or this value if this is left
        *
        * Equivalent to `flatMap(id => id)`
        */
    def flatten[B](implicit ev: A <:< Result[B]): Result[B] = flatMap(ev)

    /** The given function is applied if this is a `Success`. */
    def map[B](f: A => B): Result[B] = this match {
        case Success(x) => Success(f(x))
        case _          => this.asInstanceOf[Result[B]]
    }

    /** Returns `Success` with the existing value of `Success` if this is a `Success`
    *  and the given predicate `p` holds for the right value,
    *  or `Failure(msg)` if this is a `Success` and the given predicate `p` does not hold for the right value,
    *  or `Failure` with the existing value of `Failure` if this is a `Failure`.
    */
    def filterOrElse(p: A => Boolean, msg: =>String): Result[A] = this match {
        case Success(x) if !p(x) => Failure(msg)
        case _                   => this
    }

    /** Returns a `Seq` containing the `Success` value if
    *  it exists or an empty `Seq` if this is a `Failure`.
    */
    def toSeq: collection.immutable.Seq[A] = this match {
        case Success(x) => collection.immutable.Seq(x)
        case _          => collection.immutable.Seq.empty
    }

    /** Returns a `Some` containing the `Success` value
    *  if it exists or a `None` if this is a `Failure`.
    */
    def toOption: Option[A] = this match {
        case Success(x) => Some(x)
        case _          => None
    }

    /** Converts the `Result` into a `Try` where `Failure` maps to a plain `Exception` */
    def toTry: Try[A] = this match {
        case Success(x)   => TSuccess(x)
        case Failure(msg) => TFailure(new Exception(s"ParseError: $msg"))
    }

    def toEither: Either[String, A] = this match {
        case Success(x)   => Right(x)
        case Failure(msg) => Left(msg)
    }

    /** Returns `true` if this is a `Success`, `false` otherwise. */
    def isSuccess: Boolean

    /** Returns `true` if this is a `Failure`, `false` otherwise. */
    def isFailure: Boolean
}

/**
* Returned when a parser succeeded.
* @param x The result value of the successful parse
* @tparam A The type of expected success result
*/
case class Success[A] private [parsley] (x: A) extends Result[A]
{
    override def isSuccess: Boolean = true
    override def isFailure: Boolean = false
    override def get: A = x
}

/**
* Returned on parsing failure
* @param msg The error message reported by the parser
*/
case class Failure private [parsley] (msg: String) extends Result[Nothing]
{
    override def isSuccess: Boolean = false
    override def isFailure: Boolean = true
    override def get: Nothing = throw new NoSuchElementException("get called on Failure")
}