package parsley.token.numeric

import scala.annotation.implicitNotFound

private [numeric] sealed abstract class Bits {
    private [numeric] type self <: Bits
    private [numeric] def upperSigned: BigInt
    private [numeric] def lowerSigned: BigInt
    private [numeric] def upperUnsigned: BigInt
}
private [numeric] object _8 extends Bits {
    private [numeric] type self = _8.type
    private [numeric] val upperSigned = Byte.MaxValue
    private [numeric] val lowerSigned = Byte.MinValue
    private [numeric] val upperUnsigned = 0xff
}
private [numeric] object _16 extends Bits {
    private [numeric] type self = _16.type
    private [numeric] val upperSigned = Short.MaxValue
    private [numeric] val lowerSigned = Short.MaxValue
    private [numeric] val upperUnsigned = 0xffff
}
private [numeric] object _32 extends Bits {
    private [numeric] type self = _32.type
    private [numeric] val upperSigned = Int.MaxValue
    private [numeric] val lowerSigned = Int.MinValue
    private [numeric] val upperUnsigned = 0xffffffffL
}
private [numeric] object _64 extends Bits {
    private [numeric] type self = _64.type
    private [numeric] val upperSigned = Long.MaxValue
    private [numeric] val lowerSigned = Long.MinValue
    private [numeric] val upperUnsigned = BigInt("ffffffffffffffff", 16)
}

private [numeric] sealed abstract class CanHold[N <: Bits, T] {
    private [numeric] def fromBigInt(x: BigInt): T
}

/** This class is used to provide "low-priority" implicits
  * for the types defined within [[CanHold$ `CanHold`]].
  *
  * These implicits are not favoured equally to the implicits
  * defined within [[CanHold$ `CanHold`]] and this means
  * that Scala will not have any ambiguous implicit errors.
  *
  * @since 4.0.0
  */
abstract class LowPriorityImplicits private[numeric] {
    import CanHold.can_hold_64_bits
    // this being here means that Scala will look for it last, which allows default to Long for 64-bit
    /** Evidence that `BigInt` can store (at least) 64 bits of data.
      *
      * @note [[CanHold$.long_64 `long_64`]] is prioritised for implicit selection over this.
      * @since 4.0.0
      */
    implicit val big_64: can_hold_64_bits[BigInt] = new CanHold[_64.type, BigInt] {
        def fromBigInt(x: BigInt): BigInt = x
    }
}

/** This object contains the definitions of several types that help enforce that
  * parsers of bounded precision only return types that can losslessly accomodate
  * that precision.
  *
  * Note that, on the JVM, there is no such thing as an unsigned value ''natively''.
  * Instead, the JVM provides a guarantee that overflow is well-defined, and, as such
  * supports operations that work on numbers ''as if'' they were unsigned. For this
  * reason, `parsley` makes no distinction between unsigned and signed numbers.
  *
  * @since 4.0.0
  */
object CanHold extends LowPriorityImplicits {
    /** This type-constraint requires that the given type has enough bit-width
      * to store 8 bits of data.
      *
      * @tparam T the type that can accomodate 8 bits.
      * @since 4.0.0
      */
    @implicitNotFound("The type ${T} cannot hold an 8-bit number without loss")
    type can_hold_8_bits[T] = CanHold[_8.type, T]
    /** This type-constraint requires that the given type has enough bit-width
      * to store 16 bits of data.
      *
      * @tparam T the type that can accomodate 16 bits.
      * @since 4.0.0
      */
    @implicitNotFound("The type ${T} cannot hold a 16-bit number without loss")
    type can_hold_16_bits[T] = CanHold[_16.type, T]
    /** This type-constraint requires that the given type has enough bit-width
      * to store 32 bits of data.
      *
      * @tparam T the type that can accomodate 32 bits.
      * @since 4.0.0
      */
    @implicitNotFound("The type ${T} cannot hold a 32-bit number without loss")
    type can_hold_32_bits[T] = CanHold[_32.type, T]
    /** This type-constraint requires that the given type has enough bit-width
      * to store 64 bits of data.
      *
      * @tparam T the type that can accomodate 64 bits.
      * @since 4.0.0
      */
    @implicitNotFound("The type ${T} cannot hold a 64-bit number without loss")
    type can_hold_64_bits[T] = CanHold[_64.type, T]

    /** Provides evidence that a type that can store 16 bits can also store 8 bits.
      *
      * @since 4.0.0
      */
    implicit def fits_8_16[T: can_hold_16_bits]: can_hold_8_bits[T] = implicitly[can_hold_16_bits[T]].asInstanceOf[can_hold_8_bits[T]]
    /** Provides evidence that a type that can store 32 bits can also store 16 bits.
      *
      * @since 4.0.0
      */
    implicit def fits_16_32[T: can_hold_32_bits]: can_hold_16_bits[T] = implicitly[can_hold_32_bits[T]].asInstanceOf[can_hold_16_bits[T]]
    /** Provides evidence that a type that can store 64 bits can also store 32 bits.
      *
      * @since 4.0.0
      */
    implicit def fits_32_64[T: can_hold_64_bits]: can_hold_32_bits[T] = implicitly[can_hold_64_bits[T]].asInstanceOf[can_hold_32_bits[T]]

    /** Evidence that `Byte` can store 8 bits of data.
      *
      * @since 4.0.0
      */
    implicit val byte_8: can_hold_8_bits[Byte] = new CanHold[_8.type, Byte] {
        def fromBigInt(x: BigInt): Byte = x.toByte
    }
    /** Evidence that `Short` can store 16 bits of data.
      *
      * @since 4.0.0
      */
    implicit val short_16: can_hold_16_bits[Short] = new CanHold[_16.type, Short] {
        def fromBigInt(x: BigInt): Short = x.toShort
    }
    /** Evidence that `Int` can store 32 bits of data.
      *
      * @since 4.0.0
      */
    implicit val int_32: can_hold_32_bits[Int] = new CanHold[_32.type, Int] {
        def fromBigInt(x: BigInt): Int = x.toInt
    }
    /** Evidence that `Long` can store 64 bits of data.
      *
      * @since 4.0.0
      */
    implicit val long_64: can_hold_64_bits[Long] = new CanHold[_64.type, Long] {
        def fromBigInt(x: BigInt): Long = x.toLong
    }
}
