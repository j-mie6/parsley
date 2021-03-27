package parsley

import scala.util.{Try, Success => TSuccess, Failure => TFailure}
import scala.util.hashing.MurmurHash3

/**
* Result of a parser. Either a `Success[A]` or a `Failure`
* @tparam A The type of expected success result
*/
sealed trait Result[+Err, +A]
{
    /** Applies `fa` if this is a `Failure` or `fb` if this is a `Success`.
      *
      * @param ferr the function to apply if this is a `Failure`
      * @param fa the function to apply if this is a `Success`
      * @return the results of applying the function
      * @since 1.7.0
      */
    def fold[B](ferr: Err => B, fa: A => B): B = this match {
        case Success(x)   => fa(x)
        case Failure(msg) => ferr(msg)
    }

    /** Executes the given side-effecting function if this is a `Success`.
      *
      * @param f The side-effecting function to execute.
      * @since 1.7.0
      */
    def foreach[U](f: A => U): Unit = this match {
        case Success(x) => f(x)
        case _          =>
    }

    /** Returns the results's value.
      *
      * @note The result must not be a failure.
      * @throws java.util.NoSuchElementException if the result is a failure.
      * @since 1.7.0
      */
    def get: A

    /** Returns the value from this `Success` or the given argument if this is a `Failure`.
      * @since 1.7.0
      */
    def getOrElse[B >: A](or: =>B): B = orElse(Success(or)).get

    /** Returns this `Success` or the given argument if this is a `Failure`.
      * @since 1.7.0
      */
    def orElse[B >: A, Err_ >: Err](or: =>Result[Err_, B]): Result[Err_, B] = this match {
        case Success(_) => this
        case _          => or
    }

    /** Returns `true` if this is a `Success` and its value is equal to `elem` (as determined by `==`),
      * returns `false` otherwise.
      *
      * @param elem    the element to test.
      * @return `true` if this is a `Success` value equal to `elem`.
      * @since 1.7.0
      */
    final def contains[B >: A](elem: B): Boolean = exists(_ == elem)

    /** Returns `true` if `Failure` or returns the result of the application of
      * the given predicate to the `Success` value.
      * @since 1.7.0
      */
    def forall(f: A => Boolean): Boolean = this match {
        case Success(x) => f(x)
        case _          => true
    }

    /** Returns `false` if `Failure` or returns the result of the application of
      * the given predicate to the `Success` value.
      * @since 1.7.0
      */
    def exists(p: A => Boolean): Boolean = this match {
        case Success(x) => p(x)
        case _          => false
    }

    /** Binds the given function across `Success`.
      *
      * @param f The function to bind across `Success`.
      * @since 1.7.0
      */
    def flatMap[B, Err_ >: Err](f: A => Result[Err_, B]): Result[Err_, B] = this match {
        case Success(x) => f(x)
        case _          => this.asInstanceOf[Result[Err, B]]
    }

    /** Returns the right value if this is right
      * or this value if this is left
      *
      * Equivalent to `flatMap(id => id)`
      * @since 1.7.0
      */
    def flatten[B, Err_ >: Err](implicit ev: A <:< Result[Err_, B]): Result[Err_, B] = flatMap(ev)

    /** The given function is applied if this is a `Success`.
      * @since 1.7.0
      */
    def map[B](f: A => B): Result[Err, B] = this match {
        case Success(x) => Success(f(x))
        case _          => this.asInstanceOf[Result[Err, B]]
    }

    /** Returns `Success` with the existing value of `Success` if this is a `Success`
      * and the given predicate `p` holds for the right value,
      * or `Failure(msg)` if this is a `Success` and the given predicate `p` does not hold for the right value,
      * or `Failure` with the existing value of `Failure` if this is a `Failure`.
      * @since 1.7.0
      */
    def filterOrElse[Err_ >: Err](p: A => Boolean, msg: =>Err_): Result[Err_, A] = this match {
        case Success(x) if !p(x) => Failure(msg)
        case _                   => this
    }

    /** Returns a `Seq` containing the `Success` value if
      * it exists or an empty `Seq` if this is a `Failure`.
      * @since 1.7.0
      */
    def toSeq: collection.immutable.Seq[A] = this match {
        case Success(x) => collection.immutable.Seq(x)
        case _          => collection.immutable.Seq.empty
    }

    /** Returns a `Some` containing the `Success` value
      * if it exists or a `None` if this is a `Failure`.
      * @since 1.7.0
      */
    def toOption: Option[A] = this match {
        case Success(x) => Some(x)
        case _          => None
    }

    /** Converts the `Result` into a `Try` where `Failure` maps to a plain `Exception`
      * @since 1.7.0
      */
    def toTry: Try[A] = this match {
        case Success(x)   => TSuccess(x)
        case Failure(msg) => TFailure(new Exception(s"ParseError: $msg"))
    }

    /** Converts the `Result` into a `Either` where `Failure` maps to a `Left[Err]`
      * @since 1.7.0
      */
    def toEither: Either[Err, A] = this match {
        case Success(x)   => Right(x)
        case Failure(msg) => Left(msg)
    }

    /** Returns `true` if this is a `Success`, `false` otherwise.
      * @since 1.7.0
      */
    def isSuccess: Boolean

    /** Returns `true` if this is a `Failure`, `false` otherwise.
      * @since 1.7.0
      */
    def isFailure: Boolean
}

/**
  * Returned when a parser succeeded.
  * @param x The result value of the successful parse
  * @tparam A The type of expected success result
  */
case class Success[A] private [parsley] (x: A) extends Result[Nothing, A]
{
    override def isSuccess: Boolean = true
    override def isFailure: Boolean = false
    override def get: A = x
}

/**
  * Returned on parsing failure
  * @param msg The error message reported by the parser
  */
//case class Failure private [parsley] (msg: String) extends Result[Nothing]
class Failure[Err] private [parsley] (_msg: =>Err) extends Result[Err, Nothing] with Product with Serializable
{
    lazy val msg: Err = _msg
    override def isSuccess: Boolean = false
    override def isFailure: Boolean = true
    override def get: Nothing = throw new NoSuchElementException("get called on Failure")
    // We are normally given everything below, but ideally we want to make error generation lazy
    // $COVERAGE-OFF$
    override def toString: String = s"Failure($msg)"
    override def hashCode: Int = MurmurHash3.productHash(this)
    override def canEqual(x: Any): Boolean = x.isInstanceOf[Failure[_]]
    override def productPrefix: String = "Failure"
    override def productArity: Int = 1
    override def productElement(idx: Int): Any = {
        if (idx != 0) throw new IndexOutOfBoundsException("Failure only has arity 1") else msg
    }
    override def equals(x: Any): Boolean = x != null && (x match {
        case x: Failure[_] => x.msg == msg
    })
    def copy(msg: =>Err = this.msg): Failure[Err] = new Failure(msg)
    // $COVERAGE-ON$
}
// $COVERAGE-OFF$
object Failure {
    def apply[Err](msg: =>Err): Failure[Err] = new Failure(msg)
    def unapply[Err](x: Failure[Err]): Some[Err] = Some(x.msg)
    def andThen[A, Err](f: Failure[Err] => A): Err => A = msg => f(Failure(msg))
    def compose[A, Err](f: A => Err): A => Failure[Err] = msg => Failure(f(msg))
}
// $COVERAGE-ON$