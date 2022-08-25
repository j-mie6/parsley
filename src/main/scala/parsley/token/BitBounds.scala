package parsley.token

import scala.annotation.implicitNotFound

private [token] sealed trait Signedness
private [token] final class Signed extends Signedness
private [token] final class Unsigned extends Signedness

private [token] sealed trait Bits
private [token] final class _8 extends Bits
private [token] final class _16 extends Bits
private [token] final class _32 extends Bits
private [token] final class _64 extends Bits

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

final class CanFitAll[T, S <: Signedness, N <: Bits] private[CanFitAll] {}
object CanFitAll {
  @implicitNotFound("The type ${T} cannot hold an signed 8-bit number without loss")
  type s8[T] = CanFitAll[T, Signed, _8]
  @implicitNotFound("The type ${T} cannot hold an unsigned 8-bit number without loss")
  type u8[T] = CanFitAll[T, Unsigned, _8]
  @implicitNotFound("The type ${T} cannot hold an signed 16-bit number without loss")
  type s16[T] = CanFitAll[T, Signed, _16]
  @implicitNotFound("The type ${T} cannot hold an unsigned 16-bit number without loss")
  type u16[T] = CanFitAll[T, Unsigned, _16]
  @implicitNotFound("The type ${T} cannot hold an signed 32-bit number without loss")
  type s32[T] = CanFitAll[T, Signed, _32]
  @implicitNotFound("The type ${T} cannot hold an unsigned 32-bit number without loss")
  type u32[T] = CanFitAll[T, Unsigned, _32]
  @implicitNotFound("The type ${T} cannot hold an signed 64-bit number without loss")
  type s64[T] = CanFitAll[T, Signed, _64]
  @implicitNotFound("The type ${T} cannot hold an unsigned 64-bit number without loss")
  type u64[T] = CanFitAll[T, Unsigned, _64]

  implicit def fits_8_16[T, S <: Signedness](implicit ev: CanFitAll[T, _, _16]): CanFitAll[T, S, _8] = new CanFitAll
  implicit def fits_16_32[T, S <: Signedness](implicit ev: CanFitAll[T, _, _32]): CanFitAll[T, S, _16] = new CanFitAll
  implicit def fits_32_64[T, S <: Signedness](implicit ev: CanFitAll[T, _, _64]): CanFitAll[T, S, _32] = new CanFitAll

  implicit val byte_s8: CanFitAll[Byte, Signed, _8] = new CanFitAll
  implicit val short_s16: CanFitAll[Short, Signed, _16] = new CanFitAll
  implicit val int_s32: CanFitAll[Int, Signed, _32] = new CanFitAll
  implicit val long_s64: CanFitAll[Long, Signed, _64] = new CanFitAll
  implicit val big_s64: CanFitAll[BigInt, Signed, _64] = new CanFitAll
  implicit val big_u64: CanFitAll[BigInt, Unsigned, _64] = new CanFitAll
}

private [token] sealed trait Precision
object Precision {
    private [token] final class Single extends Precision
    private [token] final class Double extends Precision
}

final class AsPreciseAs[T, P <: Precision] private[AsPreciseAs] {}
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