package parsley.token

import scala.annotation.implicitNotFound

private [token] sealed abstract class Bits {
    private [token] type self <: Bits
    private [token] def upperSigned: BigInt
    private [token] def lowerSigned: BigInt
    private [token] def upperUnsigned: BigInt
}
private [token] object _8 extends Bits {
    private [token] type self = _8.type
    private [token] val upperSigned = 0xfe
    private [token] val lowerSigned = -0xff
    private [token] val upperUnsigned = 0xff
}
private [token] object _16 extends Bits {
    private [token] type self = _16.type
    private [token] val upperSigned = 0xfffe
    private [token] val lowerSigned = -0xffff
    private [token] val upperUnsigned = 0xffff
}
private [token] object _32 extends Bits {
    private [token] type self = _32.type
    private [token] val upperSigned = 0xfffffffeL
    private [token] val lowerSigned = -0xffffffffL
    private [token] val upperUnsigned = 0xffffffffL
}
private [token] object _64 extends Bits {
    private [token] type self = _64.type
    private [token] val upperSigned = BigInt("fffffffffffffffe", 16)
    private [token] val lowerSigned = BigInt("-ffffffffffffffff", 16)
    private [token] val upperUnsigned = BigInt("ffffffffffffffff", 16)
}

sealed abstract class CanHold[N <: Bits, T] private[token] {
    def fromBigInt(x: BigInt): T
}
abstract class LowPriorityImplicits {
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

private [token] sealed trait Precision
object Precision {
    private [token] final class Single extends Precision
    private [token] final class Double extends Precision
}

final class AsPreciseAs[T, P <: Precision] private[AsPreciseAs] /*for scala 3...*/ {}
object AsPreciseAs {
    @implicitNotFound("The type ${T} is not precise enough to contain the full range of an IEEE 754 single-precision float")
    type f32[T] = AsPreciseAs[T, Precision.Single]
    @implicitNotFound("The type ${T} is not precise enough to contain the full range of an IEEE 754 double-precision float")
    type f64[T] = AsPreciseAs[T, Precision.Double]

    implicit def as_precise_32_64[T](implicit ev: AsPreciseAs[T, Precision.Double]): AsPreciseAs[T, Precision.Single] = new AsPreciseAs

    implicit val float_f32: AsPreciseAs[Float, Precision.Single] = new AsPreciseAs
    implicit val double_f64: AsPreciseAs[Double, Precision.Double] = new AsPreciseAs
    implicit val big_f64: AsPreciseAs[BigDecimal, Precision.Double] = new AsPreciseAs

}
