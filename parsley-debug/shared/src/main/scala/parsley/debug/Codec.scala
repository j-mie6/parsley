package parsley.debug

import scala.util.Try

trait Codec[A] {
    def decode(s: String): Try[A]
    def encode(a: A): String
}

object BooleanCodec extends Codec[Boolean] {
    def encode(b: Boolean): String = b.toString

    def decode(s: String): Try[Boolean] = Try(s.toBoolean)
}

object ByteCodec extends Codec[Byte] {
    def encode(x: Byte): String = x.toString

    def decode(s: String): Try[Byte] = Try(s.toByte)
}

object ShortCodec extends Codec[Short] {
    def encode(x: Short): String = x.toString

    def decode(s: String): Try[Short] = Try(s.toShort)
}

object IntCodec extends Codec[Int] {
    def encode(x: Int): String = x.toString

    def decode(s: String): Try[Int] = Try(s.toInt)
}

object LongCodec extends Codec[Long] {
    def encode(x: Long): String = x.toString

    def decode(s: String): Try[Long] = Try(s.toLong)
}

object FloatCodec extends Codec[Float] {
    def encode(x: Float): String = x.toString

    def decode(s: String): Try[Float] = Try(s.toFloat)
}

object DoubleCodec extends Codec[Double] {
    def encode(x: Double): String = x.toString

    def decode(s: String): Try[Double] = Try(s.toDouble)
}

object CharCodec extends Codec[Char] {
    def encode(c: Char): String = c.toString

    def decode(s: String): Try[Char] = Try(if (s.length == 1) s.charAt(0) else throw new Exception)
}

object StringCodec extends Codec[String] {
    def encode(s: String): String = s

    def decode(s: String): Try[String] = Try(s)
}
