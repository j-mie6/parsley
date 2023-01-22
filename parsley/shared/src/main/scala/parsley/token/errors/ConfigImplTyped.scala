/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.XCompat.unused
import parsley.Parsley
import parsley.errors.combinator, combinator.ErrorMethods

/** This trait, and its subclasses, can be used to configure how filters should be used within the `Lexer`.
  * @since 4.1.0
  * @group filters
  */
trait FilterConfig[A] {
    private [parsley] def filter(p: Parsley[A])(f: A => Boolean): Parsley[A]
    // $COVERAGE-OFF$
    private [parsley] def collect[B](p: Parsley[A])(f: PartialFunction[A, B]): Parsley[B] = this.filter(p)(f.isDefinedAt).map(f)
    private [parsley] def injectLeft[B]: FilterConfig[Either[A, B]]
    private [parsley] def injectRight[B]: FilterConfig[Either[B, A]]
    // $COVERAGE-ON$
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
  * @group filters
  */
abstract class SpecialisedMessage[A] extends SpecialisedFilterConfig[A] { self =>
    @deprecated("filters do not have partial amend semantics, so this does nothing", "4.1.0")  def this(@unused fullAmend: Boolean) = this()
    /** This method produces the messages for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def message(x: A): Seq[String]

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.guardAgainst {
        case x if !f(x) => message(x)
    }

    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) = p.collectMsg(message(_))(f)
    // $COVERAGE-OFF$
    private [parsley] final override def injectLeft[B] = new SpecialisedMessage[Either[A, B]] {
        def message(xy: Either[A, B]) = {
            val Left(x) = xy
            self.message(x)
        }
    }
    private [parsley] final override def injectRight[B] = new SpecialisedMessage[Either[B, A]] {
        def message(xy: Either[B, A]) = {
            val Right(y) = xy
            self.message(y)
        }
    }
    // $COVERAGE-ON$
}

/** This class ensures that the filter will generate a ''vanilla'' unexpected item for the given failing parse.
  * @since 4.1.0
  * @group filters
  */
abstract class Unexpected[A] extends VanillaFilterConfig[A] { self =>
    @deprecated("filters do not have partial amend semantics, so this does nothing", "4.1.0")  def this(@unused fullAmend: Boolean) = this()
    /** This method produces the unexpected label for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def unexpected(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.unexpectedWhen {
        case x if !f(x) => unexpected(x)
    }
    // $COVERAGE-OFF$
    private [parsley] final override def injectLeft[B] = new Unexpected[Either[A, B]] {
        def unexpected(xy: Either[A, B]) = {
            val Left(x) = xy
            self.unexpected(x)
        }
    }
    private [parsley] final override def injectRight[B] = new Unexpected[Either[B, A]] {
        def unexpected(xy: Either[B, A]) = {
            val Right(y) = xy
            self.unexpected(y)
        }
    }
    // $COVERAGE-ON$
}

/** This class ensures that the filter will generate a ''vanilla'' reason for the given failing parse.
  * @since 4.1.0
  * @group filters
  */
abstract class Because[A] extends VanillaFilterConfig[A] { self =>
    @deprecated("filters do not have partial amend semantics, so this does nothing", "4.1.0")  def this(@unused fullAmend: Boolean) = this()
    /** This method produces the reason for the given value.
      * @since 4.1.0
      * @group badchar
      */
    def reason(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.filterOut {
        case x if !f(x) => reason(x)
    }
    // $COVERAGE-OFF$
    private [parsley] final override def injectLeft[B] = new Because[Either[A, B]] {
        def reason(xy: Either[A, B]) = {
            val Left(x) = xy
            self.reason(x)
        }
    }
    private [parsley] final override def injectRight[B] = new Because[Either[B, A]] {
        def reason(xy: Either[B, A]) = {
            val Right(y) = xy
            self.reason(y)
        }
    }
    // $COVERAGE-ON$
}

/** This class ensures that the filter will generate a ''vanilla'' unexpected item and a reason for the given failing parse.
  * @since 4.1.0
  * @group filters
  */
abstract class UnexpectedBecause[A] extends VanillaFilterConfig[A] { self =>
    @deprecated("filters do not have partial amend semantics, so this does nothing", "4.1.0")  def this(@unused fullAmend: Boolean) = this()
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

    // TODO: factor this combinator out with the "Great Move" in 4.2
    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.unexpectedWithReasonWhen {
        case x if !f(x) => (unexpected(x), reason(x))
    }
    // $COVERAGE-OFF$
    private [parsley] final override def injectLeft[B] = new UnexpectedBecause[Either[A, B]] {
        def unexpected(xy: Either[A, B]) = {
            val Left(x) = xy
            self.unexpected(x)
        }
        def reason(xy: Either[A, B]) = {
            val Left(x) = xy
            self.reason(x)
        }
    }
    private [parsley] final override def injectRight[B] = new UnexpectedBecause[Either[B, A]] {
        def unexpected(xy: Either[B, A]) = {
            val Right(y) = xy
            self.unexpected(y)
        }
        def reason(xy: Either[B, A]) = {
            val Right(x) = xy
            self.reason(x)
        }
    }
    // $COVERAGE-ON$
}

/** This class can be used to not specify an error configuration for the filter, a regular `filter` is used instead.
  * @since 4.1.0
  * @group filters
  */
final class BasicFilter[A] extends SpecialisedFilterConfig[A] with VanillaFilterConfig[A] {
    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.filter(f)
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) = p.collect(f)
    // $COVERAGE-OFF$
    private [parsley] final override def injectLeft[B] = new BasicFilter[Either[A, B]]
    private [parsley] final override def injectRight[B] = new BasicFilter[Either[B, A]]
    // $COVERAGE-ON$
}
