/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debug

import scala.util.Try
import parsley.state.Ref

abstract class RefCodec {
    type A

    val ref: Ref[A]
    val codec: Codec[A]
}

object RefCodec {
    
    // Ref address and String value passed to RemoteView
    type CodedRef = (Int, String)

}


/** An encoder-decoder for `A` and a string representation
  * 
  * @tparam A the type being encoded and decoded
  */
trait Codec[A] {
    /** Encode `a` into a string
      * 
      * @param a the value to encode
      * @return the string encoding of `a`
      */
    def encode(a: A): String

    /** Attempt to decode a string into type `A`
      *
      * @param s the string to decode
      * @return a `Option[A]` containing the decoded value if successful
      */
    def decode(s: String): Option[A]
}

object BooleanCodec extends Codec[Boolean] {
    def encode(b: Boolean): String = b.toString

    def decode(s: String): Option[Boolean] = Try(s.toBoolean).toOption
}

object ByteCodec extends Codec[Byte] {
    def encode(x: Byte): String = x.toString

    def decode(s: String): Option[Byte] = Try(s.toByte).toOption
}

object ShortCodec extends Codec[Short] {
    def encode(x: Short): String = x.toString

    def decode(s: String): Option[Short] = Try(s.toShort).toOption
}

object IntCodec extends Codec[Int] {
    def encode(x: Int): String = x.toString

    def decode(s: String): Option[Int] = Try(s.toInt).toOption
}

object LongCodec extends Codec[Long] {
    def encode(x: Long): String = x.toString

    def decode(s: String): Option[Long] = Try(s.toLong).toOption
}

object FloatCodec extends Codec[Float] {
    def encode(x: Float): String = x.toString

    def decode(s: String): Option[Float] = Try(s.toFloat).toOption
}

object DoubleCodec extends Codec[Double] {
    def encode(x: Double): String = x.toString

    def decode(s: String): Option[Double] = Try(s.toDouble).toOption
}

object CharCodec extends Codec[Char] {
    def encode(c: Char): String = c.toString

    def decode(s: String): Option[Char] = if (s.length() == 1) Some(s.charAt(0)) else None
}

object StringCodec extends Codec[String] {
    def encode(s: String): String = s

    def decode(s: String): Option[String] = Some(s)
}
