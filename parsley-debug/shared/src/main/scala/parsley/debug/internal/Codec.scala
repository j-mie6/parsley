package parsley.debug.internal

import scala.util.Try

private [debug] trait Codec[A] {
    private [debug] def decode(s: String): Try[A]
    private [debug] def encode(a: A): String
}

private [debug] object BooleanCodec extends Codec[Boolean] {
    override private[debug] def encode(b: Boolean): String = b.toString

    override private[debug] def decode(s: String): Try[Boolean] = Try(s.toBoolean)
}

private [debug] object ByteCodec extends Codec[Byte] {
    override private[debug] def encode(x: Byte): String = x.toString

    override private[debug] def decode(s: String): Try[Byte] = Try(s.toByte)
}

private [debug] object ShortCodec extends Codec[Short] {
    override private[debug] def encode(x: Short): String = x.toString

    override private[debug] def decode(s: String): Try[Short] = Try(s.toShort)
}

private [debug] object IntCodec extends Codec[Int] {
    override private[debug] def encode(x: Int): String = x.toString

    override private[debug] def decode(s: String): Try[Int] = Try(s.toInt)
}

private [debug] object LongCodec extends Codec[Long] {
    override private[debug] def encode(x: Long): String = x.toString

    override private[debug] def decode(s: String): Try[Long] = Try(s.toLong)
}

private [debug] object FloatCodec extends Codec[Float] {
    override private[debug] def encode(x: Float): String = x.toString

    override private[debug] def decode(s: String): Try[Float] = Try(s.toFloat)
}

private [debug] object DoubleCodec extends Codec[Double] {
    override private[debug] def encode(x: Double): String = x.toString

    override private[debug] def decode(s: String): Try[Double] = Try(s.toDouble)
}

private [debug] object CharCodec extends Codec[Char] {
    override private[debug] def encode(c: Char): String = c.toString

    override private[debug] def decode(s: String): Try[Char] = Try(if (s.length == 1) s.charAt(0) else throw new Exception)
}

private [debug] object StringCodec extends Codec[String] {
    override private[debug] def encode(s: String): String = s

    override private[debug] def decode(s: String): Try[String] = Try(s)
}
