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

object Codec {

    /** Implicitly converts a reference to an encodable object
      *
      * @param r The reference
      * @param c The string encoder-decoder for type `B`
      * @tparam B the type to be contained in this reference during runtime
      * @return The encodable object storing `r`
      */
    implicit def encodable[B](r: Ref[B])(implicit c: Codec[B]) = new RefCodec {
        type A = B
        override val ref: Ref[A] = r
        override val codec: Codec[A] = c
    }
    

    implicit val booleanCodec: Codec[Boolean] = new Codec[Boolean] {
        def encode(b: Boolean): String = b.toString

        def decode(s: String): Option[Boolean] = Try(s.toBoolean).toOption
    }   

    implicit val byteCodec: Codec[Byte] = new Codec[Byte] {
        def encode(x: Byte): String = x.toString

        def decode(s: String): Option[Byte] = Try(s.toByte).toOption
    }

    implicit val shortCodec: Codec[Short] = new Codec[Short] {
        def encode(x: Short): String = x.toString

        def decode(s: String): Option[Short] = Try(s.toShort).toOption
    }

    implicit val intCodec: Codec[Int] = new Codec[Int] {
        def encode(x: Int): String = x.toString

        def decode(s: String): Option[Int] = Try(s.toInt).toOption
    }

    implicit val longCodec: Codec[Long] = new Codec[Long] {
        def encode(x: Long): String = x.toString

        def decode(s: String): Option[Long] = Try(s.toLong).toOption
    }

    implicit val floatCodec: Codec[Float] = new Codec[Float] {
        def encode(x: Float): String = x.toString

        def decode(s: String): Option[Float] = Try(s.toFloat).toOption
    }

    implicit val doubleCodec: Codec[Double] = new Codec[Double] {
        def encode(x: Double): String = x.toString

        def decode(s: String): Option[Double] = Try(s.toDouble).toOption
    }

    implicit val charCodec: Codec[Char] = new Codec[Char] {
        def encode(c: Char): String = c.toString

        def decode(s: String): Option[Char] = if (s.length() == 1) Some(s.charAt(0)) else None
    }

    implicit val stringCodec: Codec[String]  = new Codec[String] {
        def encode(s: String): String = s

        def decode(s: String): Option[String] = Some(s)
    }
}
