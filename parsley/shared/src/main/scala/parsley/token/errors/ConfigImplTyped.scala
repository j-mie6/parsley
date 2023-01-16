package parsley.token.errors

import parsley.Parsley, Parsley.pure
import parsley.errors.combinator, combinator.ErrorMethods
import parsley.position

private [parsley] sealed trait ConfigImplTyped[A]
private [parsley] object ConfigImplTyped {
    def amendThenDislodge[A](full: Boolean)(p: Parsley[A]): Parsley[A] = {
        if (full) combinator.amendThenDislodge(p)
        else p
    }
}

trait FilterOps[A] {
    private [parsley] def filter(p: Parsley[A])(f: A => Boolean): Parsley[A]
    private [parsley] def collect[B](p: Parsley[A])(f: PartialFunction[A, B]): Parsley[B] = this.filter(p)(f.isDefinedAt).map(f)
    private [parsley] def injectLeft[B]: FilterOps[Either[A, B]]
    private [parsley] def injectRight[B]: FilterOps[Either[B, A]]
}

trait FilterConfig[A] extends ConfigImplTyped[A] with FilterOps[A]

trait SpecialisedFilterConfig[A] extends FilterConfig[A]
trait VanillaFilterConfig[A] extends FilterConfig[A]

abstract class SpecialisedMessage[A](fullAmend: Boolean) extends SpecialisedFilterConfig[A] { self =>
    def message(x: A): Seq[String]

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = ConfigImplTyped.amendThenDislodge(fullAmend) {
        p.guardAgainst {
            case x if !f(x) => message(x)
        }
    }
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) =  ConfigImplTyped.amendThenDislodge(fullAmend) {
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

abstract class Unexpected[A](fullAmend: Boolean) extends VanillaFilterConfig[A] { self =>
    def unexpected(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = ConfigImplTyped.amendThenDislodge(fullAmend) {
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

abstract class Because[A](fullAmend: Boolean) extends VanillaFilterConfig[A] { self =>
    def reason(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = ConfigImplTyped.amendThenDislodge(fullAmend) {
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

abstract class UnexpectedBecause[A](fullAmend: Boolean) extends VanillaFilterConfig[A] { self =>
    def unexpected(x: A): String
    def reason(x: A): String

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = ConfigImplTyped.amendThenDislodge(fullAmend) {
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

final class BasicFilter[A] extends SpecialisedFilterConfig[A] with VanillaFilterConfig[A] {
    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.filter(f)
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) = p.collect(f)
    private [parsley] final override def injectLeft[B] = new BasicFilter[Either[A, B]]
    private [parsley] final override def injectRight[B] = new BasicFilter[Either[B, A]]
}
