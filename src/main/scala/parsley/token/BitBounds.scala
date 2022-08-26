package parsley.token

import scala.annotation.implicitNotFound

private [token] sealed trait Bits {
    type self <: Bits
}
private [token] final object _8 extends Bits {
    type self = _8.type
}
private [token] final object _16 extends Bits {
    type self = _16.type
}
private [token] final object _32 extends Bits {
    type self = _32.type
}
private [token] final object _64 extends Bits {
    type self = _64.type
}

/*sealed trait SafeConvertInt[T] {
    def fromBigInt(x: BigInt): T
    val minValue: BigInt
    val maxValue: BigInt
}
object SafeConvertInt {
    implicit object fromByte extends SafeConvertInt[Byte] {
        def fromBigInt(x: BigInt): Byte = x.toByte
        val minValue = Byte.MinValue
        val maxValue = Byte.MaxValue
    }

    implicit object fromShort extends SafeConvertInt[Short] {
        def fromBigInt(x: BigInt): Short = x.toShort
        val minValue = Short.MinValue
        val maxValue = Short.MaxValue
    }

    implicit object fromInt extends SafeConvertInt[Int] {
        def fromBigInt(x: BigInt): Int = x.toInt
        val minValue = Int.MinValue
        val maxValue = Int.MaxValue
    }

    implicit object fromLong extends SafeConvertInt[Long] {
        def fromBigInt(x: BigInt): Long = x.toLong
        val minValue = Long.MinValue
        val maxValue = Long.MaxValue
    }

    implicit object fromBigInt extends SafeConvertInt[BigInt] {
        def fromBigInt(x: BigInt): Long = x
        val minValue =
        val maxValue = Long.MaxValue
    }
}*/

final class CanHold[N <: Bits, T] private[CanHold]
object CanHold {
  @implicitNotFound("The type ${T} cannot hold an 8-bit number without loss")
  type can_hold_8_bits[T] = CanHold[_8.type, T]
  @implicitNotFound("The type ${T} cannot hold a 16-bit number without loss")
  type can_hold_16_bits[T] = CanHold[_16.type, T]
  @implicitNotFound("The type ${T} cannot hold a 32-bit number without loss")
  type can_hold_32_bits[T] = CanHold[_32.type, T]
  @implicitNotFound("The type ${T} cannot hold a 64-bit number without loss")
  type can_hold_64_bits[T] = CanHold[_64.type, T]

  implicit def fits_8_16[T: can_hold_16_bits]: can_hold_8_bits[T] = new CanHold
  implicit def fits_16_32[T: can_hold_32_bits]: can_hold_16_bits[T] = new CanHold
  implicit def fits_32_64[T: can_hold_64_bits]: can_hold_32_bits[T] = new CanHold

  implicit val byte_8: can_hold_8_bits[Byte] = new CanHold
  implicit val short_16: can_hold_16_bits[Short] = new CanHold
  implicit val int_32: can_hold_32_bits[Int] = new CanHold
  implicit val long_64: can_hold_64_bits[Long] = new CanHold
  implicit val big_64: can_hold_64_bits[BigInt] = new CanHold
}

private [token] sealed trait Precision
object Precision {
    private [token] final class Single extends Precision
    private [token] final class Double extends Precision
}

final class AsPreciseAs[T, P <: Precision] private[AsPreciseAs]
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
