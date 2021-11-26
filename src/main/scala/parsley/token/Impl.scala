package parsley.token

import parsley.Parsley
import scala.language.higherKinds

/**
  * The Impl trait is used to provide implementation of the parser requirements from `LanguageDef`
  * @since 2.2.0
  */
sealed trait Impl

/**
  * The implementation provided is a parser which parses the required token.
  * @param p The parser which will parse the token
  * @since 2.2.0
  */
final case class Parser(p: Parsley[_]) extends Impl

/**
  * The implementation provided is a function which matches on the input streams characters
  * @param f The predicate that input tokens are tested against
  * @since 2.2.0
  */
final case class Predicate(f: Char => Boolean) extends Impl

/**
  * This implementation states that the required functionality is not required. If it is used it will raise an error
  * at parse-time
  * @since 2.2.0
  */
case object NotRequired extends Impl

private [parsley] final case class BitSetImpl(cs: Char => Boolean) extends Impl

/**
  * This implementation uses a set of valid tokens. It is converted to a high-performance BitSet.
  * @since 2.2.0
  */
object CharSet
{
    /**
      * @param cs The set to convert
      * @since 2.2.0
      */
    def apply(cs: Set[Char]): Impl = BitSetImpl(new BitSet(cs))
    def apply(cs: Char*): Impl = apply(Set(cs: _*))
}

private [parsley] object Static {
    def unapply(impl: Impl): Option[Char => Boolean] = impl match {
        case Predicate(f)   => Some(f)
        case BitSetImpl(cs) => Some(cs)
        case NotRequired    => Some(_ => false)
        case _              => None
    }
}