package parsley.token.errors

import parsley.Parsley, Parsley.pure
import parsley.errors.combinator, combinator.ErrorMethods
import parsley.position

private [parsley] object FilterOps {
    def amendThenDislodge[A](full: Boolean)(p: Parsley[A]): Parsley[A] = {
        if (full) combinator.amendThenDislodge(p)
        else p
    }
}

/** This trait, and its subclasses, can be used to configure how filters should be used within the `Lexer`.
  * @since 4.1.0
  * @group filters
  */
trait FilterConfig[A] {
    private [parsley] def filter(p: Parsley[A])(f: A => Boolean): Parsley[A]
    private [parsley] def collect[B](p: Parsley[A])(f: PartialFunction[A, B]): Parsley[B] = this.filter(p)(f.isDefinedAt).map(f)
    private [parsley] def injectLeft[B]: FilterConfig[Either[A, B]]
    private [parsley] def injectRight[B]: FilterConfig[Either[B, A]]
}

/** This subtrait of `FilterConfig` specifies that only filters generating ''specialised'' errors may be used.
  * @since 4.1.0
  * @group filters
  */
trait SpecialisedFilterConfig[A] extends FilterConfig[A]
/** This subtrait of `FilterConfig` specifies that only filters generating ''vanilla'' errors may be used.
  * @since 4.1.0
  * @group filters
  */
trait VanillaFilterConfig[A] extends FilterConfig[A]

/** This class ensures that the filter will generate ''specialised'' messages for the given failing parse.
  * @since 4.1.0
  * @param fullAmend filters usually have partial amend semantics: should this instead do a full amend?
  * @group filters
  */
abstract class SpecialisedMessage[A](fullAmend: Boolean) extends SpecialisedFilterConfig[A] { self =>
    /** This method produces the messages for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def message(x: A): Seq[String]

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = FilterOps.amendThenDislodge(fullAmend) {
        p.guardAgainst {
            case x if !f(x) => message(x)
        }
    }
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) =  FilterOps.amendThenDislodge(fullAmend) {
        p.collectMsg(message(_))(f)
    }
    private [parsley] final override def injectLeft[B] = new SpecialisedMessage[Either[A, B]](fullAmend) {
        def message(xy: Either[A, B]) = {
            val Left(x) = xy
            self.message(x)
        }
    }
    private [parsley] final override def injectRight[B] = new SpecialisedMessage[Either[B, A]](fullAmend) {
        def message(xy: Either[B, A]) = {
            val Right(y) = xy
            self.message(y)
        }
    }
}

/** This class ensures that the filter will generate a ''vanilla'' unexpected item for the given failing parse.
  * @since 4.1.0
  * @param fullAmend filters usually have partial amend semantics: should this instead do a full amend?
  * @group filters
  */
abstract class Unexpected[A](fullAmend: Boolean) extends VanillaFilterConfig[A] { self =>
    /** This method produces the unexpected label for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def unexpected(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = FilterOps.amendThenDislodge(fullAmend) {
        p.unexpectedWhen {
            case x if !f(x) => unexpected(x)
        }
    }
    private [parsley] final override def injectLeft[B] = new Unexpected[Either[A, B]](fullAmend) {
        def unexpected(xy: Either[A, B]) = {
            val Left(x) = xy
            self.unexpected(x)
        }
    }
    private [parsley] final override def injectRight[B] = new Unexpected[Either[B, A]](fullAmend) {
        def unexpected(xy: Either[B, A]) = {
            val Right(y) = xy
            self.unexpected(y)
        }
    }
}

/** This class ensures that the filter will generate a ''vanilla'' reason for the given failing parse.
  * @since 4.1.0
  * @param fullAmend filters usually have partial amend semantics: should this instead do a full amend?
  * @group filters
  */
abstract class Because[A](fullAmend: Boolean) extends VanillaFilterConfig[A] { self =>
    /** This method produces the reason for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def reason(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = FilterOps.amendThenDislodge(fullAmend) {
        p.filterOut {
            case x if !f(x) => reason(x)
        }
    }
    private [parsley] final override def injectLeft[B] = new Because[Either[A, B]](fullAmend) {
        def reason(xy: Either[A, B]) = {
            val Left(x) = xy
            self.reason(x)
        }
    }
    private [parsley] final override def injectRight[B] = new Because[Either[B, A]](fullAmend) {
        def reason(xy: Either[B, A]) = {
            val Right(y) = xy
            self.reason(y)
        }
    }
}

/** This class ensures that the filter will generate a ''vanilla'' unexpected item and a reason for the given failing parse.
  * @since 4.1.0
  * @param fullAmend filters usually have partial amend semantics: should this instead do a full amend?
  * @group filters
  */
abstract class UnexpectedBecause[A](fullAmend: Boolean) extends VanillaFilterConfig[A] { self =>
    /** This method produces the unexpected label for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def unexpected(x: A): String
    /** This method produces the reason for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def reason(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = FilterOps.amendThenDislodge(fullAmend) {
        combinator.amendThenDislodge {
            position.internalOffsetSpan(combinator.entrench(p)).flatMap { case (os, x, oe) =>
                if (f(x)) combinator.unexpected(oe - os, this.unexpected(x)).explain(reason(x))
                else pure(x)
            }
        }
    }
    private [parsley] final override def injectLeft[B] = new UnexpectedBecause[Either[A, B]](fullAmend) {
        def unexpected(xy: Either[A, B]) = {
            val Left(x) = xy
            self.unexpected(x)
        }
        def reason(xy: Either[A, B]) = {
            val Left(x) = xy
            self.reason(x)
        }
    }
    private [parsley] final override def injectRight[B] = new UnexpectedBecause[Either[B, A]](fullAmend) {
        def unexpected(xy: Either[B, A]) = {
            val Right(y) = xy
            self.unexpected(y)
        }
        def reason(xy: Either[B, A]) = {
            val Right(x) = xy
            self.reason(x)
        }
    }
}

/** This class can be used to not specify an error configuration for the filter, a regular `filter` is used instead.
  * @since 4.1.0
  * @group filters
  */
final class BasicFilter[A] extends SpecialisedFilterConfig[A] with VanillaFilterConfig[A] {
    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.filter(f)
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) = p.collect(f)
    private [parsley] final override def injectLeft[B] = new BasicFilter[Either[A, B]]
    private [parsley] final override def injectRight[B] = new BasicFilter[Either[B, A]]
}
