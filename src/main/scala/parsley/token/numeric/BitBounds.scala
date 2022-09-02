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

sealed abstract class CanHold[N <: Bits, T] private[numeric] {
    private [numeric] def fromBigInt(x: BigInt): T
}
abstract class LowPriorityImplicits private[numeric] {
    import CanHold.can_hold_64_bits
    // this being here means that Scala will look for it last, which allows default to Long for 64-bit
    implicit val big_64: can_hold_64_bits[BigInt] = new CanHold[_64.type, BigInt] {
        def fromBigInt(x: BigInt): BigInt = x
    }
}

object CanHold extends LowPriorityImplicits {
    @implicitNotFound("The type ${T} cannot hold an 8-bit number without loss")
    type can_hold_8_bits[T] = CanHold[_8.type, T]
    @implicitNotFound("The type ${T} cannot hold a 16-bit number without loss")
    type can_hold_16_bits[T] = CanHold[_16.type, T]
    @implicitNotFound("The type ${T} cannot hold a 32-bit number without loss")
    type can_hold_32_bits[T] = CanHold[_32.type, T]
    @implicitNotFound("The type ${T} cannot hold a 64-bit number without loss")
    type can_hold_64_bits[T] = CanHold[_64.type, T]

    implicit def fits_8_16[T: can_hold_16_bits]: can_hold_8_bits[T] = implicitly[can_hold_16_bits[T]].asInstanceOf[can_hold_8_bits[T]]
    implicit def fits_16_32[T: can_hold_32_bits]: can_hold_16_bits[T] = implicitly[can_hold_32_bits[T]].asInstanceOf[can_hold_16_bits[T]]
    implicit def fits_32_64[T: can_hold_64_bits]: can_hold_32_bits[T] = implicitly[can_hold_64_bits[T]].asInstanceOf[can_hold_32_bits[T]]

    implicit val byte_8: can_hold_8_bits[Byte] = new CanHold[_8.type, Byte] {
        def fromBigInt(x: BigInt): Byte = x.toByte
    }
    implicit val short_16: can_hold_16_bits[Short] = new CanHold[_16.type, Short] {
        def fromBigInt(x: BigInt): Short = x.toShort
    }
    implicit val int_32: can_hold_32_bits[Int] = new CanHold[_32.type, Int] {
        def fromBigInt(x: BigInt): Int = x.toInt
    }
    implicit val long_64: can_hold_64_bits[Long] = new CanHold[_64.type, Long] {
        def fromBigInt(x: BigInt): Long = x.toLong
    }
}
