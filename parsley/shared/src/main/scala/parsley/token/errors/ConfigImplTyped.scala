package parsley.token.errors

import parsley.Parsley
import parsley.errors.combinator._

private [parsley] sealed trait ConfigImplTyped[A] {
    private [parsley] def filter(p: Parsley[A])(f: A => Boolean): Parsley[A]
    private [parsley] def collect[B](p: Parsley[A])(f: PartialFunction[A, B]): Parsley[B]
    private [parsley] def injectLeft[B]: ConfigImplTyped[Either[A, B]]
    private [parsley] def injectRight[B]: ConfigImplTyped[Either[B, A]]
}
private [parsley] object ConfigImplTyped {
    def amendThenDislodge[A](full: Boolean)(p: Parsley[A]): Parsley[A] = {
        if (full) parsley.errors.combinator.amendThenDislodge(p)
        else p
    }
}

trait SpecialisedFilterConfig[A] extends ConfigImplTyped[A]

trait VanillaFilterConfig[A] extends ConfigImplTyped[A]

abstract class SpecialisedMessage[A](fullAmend: Boolean) extends SpecialisedFilterConfig[A] { self =>
    def apply(x: A): Seq[String]

    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = ConfigImplTyped.amendThenDislodge(fullAmend) {
        p.guardAgainst {
            case x if !f(x) => apply(x)
        }
    }
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) =  ConfigImplTyped.amendThenDislodge(fullAmend) {
        p.collectMsg(apply(_))(f)
    }
    private [parsley] final override def injectLeft[B] = new SpecialisedMessage[Either[A, B]](fullAmend) {
        def apply(xy: Either[A, B]) = {
            val Left(x) = xy
            self.apply(x)
        }
    }
    private [parsley] final override def injectRight[B] = new SpecialisedMessage[Either[B, A]](fullAmend) {
        def apply(xy: Either[B, A]) = {
            val Right(y) = xy
            self.apply(y)
        }
    }
}

final class BasicFilter[A] extends SpecialisedFilterConfig[A] with VanillaFilterConfig[A] {
    private [parsley] final override def filter(p: Parsley[A])(f: A => Boolean) = p.filter(f)
    private [parsley] final override def collect[B](p: Parsley[A])(f: PartialFunction[A, B]) = p.collect(f)
    private [parsley] final override def injectLeft[B] = new BasicFilter[Either[A, B]]
    private [parsley] final override def injectRight[B] = new BasicFilter[Either[B, A]]
}
