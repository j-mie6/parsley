package parsley.debug.internal

import scala.util.Try

private [debug] trait Codec[A] {
    private [debug] def decode(s: String): Try[A]
    private [debug] def encode(a: A): String
}
