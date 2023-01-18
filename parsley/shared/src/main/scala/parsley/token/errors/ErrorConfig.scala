/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.XCompat.unused

/** This class is used to specify how errors should be produced by the
  * [[parsley.token.Lexer `Lexer`]] class.
  *
  * The [[parsley.token.Lexer `Lexer`]] is set up to produce a variety of different
  * errors via `label`-ing, `explain`-ing, and `filter`-ing, and some applications of
  * the ''Verified'' and ''Preventative'' error patterns. The exact content of those
  * errors can be configured here. Errors can be suppressed or specified with different
  * levels of detail, or even switching between ''vanilla'' or ''specialised'' errors.
  *
  * This class should be used by extending it and overriding the relevant parts: all
  * methods here are non-abstract and their default is documented inside.
  *
  * @since 4.1.0
  * @group errconfig
  *
  * @groupprio numeric 0
  * @groupname numeric Numeric Errors
  * @groupdesc numeric These control the errors generated with the `numeric` component of the `Lexer`.
  *
  * @groupprio text 0
  * @groupname text Text Errors
  * @groupdesc text These control the errors generated with the `text` component of the `Lexer`.
  *
  * @groupprio names 0
  * @groupname names Name Errors
  * @groupdesc names These control the errors generated with the `names` component of the `Lexer`.
  *
  * @groupprio symbol 0
  * @groupname symbol Symbol Errors
  * @groupdesc symbol These control the errors generated with the `symbol` component of the `Lexer`.
  *
  * @groupprio space 0
  * @groupname space Space Errors
  * @groupdesc space These control the errors generated with the `space` component of the `Lexer`.
  */
class ErrorConfig {
    // numeric
    /** How should a numeric break character (like `_`) be referred to or explained within an error.
      * @since 4.1.0
      * @group numeric
      */
    def labelNumericBreakChar: LabelWithExplainConfig = NotConfigured

    /** How should unsigned decimal integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber:* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedDecimal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** How should unsigned hexadecimal integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber:* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedHexadecimal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** How should unsigned octal integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber:* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedOctal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** How should unsigned binary integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber:* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedBinary: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** How should generic unsigned integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelIntegerUnsignedNumber: LabelWithExplainConfig = NotConfigured
    /** How should unsigned decimal integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber(bits:Int):* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedDecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedDecimal
    /** How should unsigned hexadecimal integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber(bits:Int):* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedHexadecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedHexadecimal
    /** How should unsigned octal integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber(bits:Int):* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedOctal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedOctal
    /** How should unsigned binary integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerUnsignedNumber(bits:Int):* `labelIntegerUnsignedNumber`]]
      * @group numeric
      */
    def labelIntegerUnsignedBinary(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedBinary
    /** How should generic unsigned integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelIntegerUnsignedNumber(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedNumber

    /** How should signed decimal integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedNumber:* `labelIntegerSignedNumber`]]
      * @group numeric
      */
    def labelIntegerSignedDecimal: LabelWithExplainConfig = labelIntegerSignedNumber
    /** How should signed hexadecimal integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedNumber:* `labelIntegerSignedNumber`]]
      * @group numeric
      */
    def labelIntegerSignedHexadecimal: LabelWithExplainConfig = labelIntegerSignedNumber
    /** How should signed octal integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedNumber:* `labelIntegerSignedNumber`]]
      * @group numeric
      */
    def labelIntegerSignedOctal: LabelWithExplainConfig = labelIntegerSignedNumber
    /** How should signed binary integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedNumber:* `labelIntegerSignedNumber`]]
      * @group numeric
      */
    def labelIntegerSignedBinary: LabelWithExplainConfig = labelIntegerSignedNumber
    /** How should generic signed integers be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelIntegerSignedNumber: LabelWithExplainConfig = NotConfigured
    /** How should signed decimal integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedDecimal(bits:Int):* `labelIntegerSignedDecimal`]]
      * @group numeric
      */
    def labelIntegerSignedDecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedDecimal
    /** How should signed hexadecimal integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedHexadecimal(bits:Int):* `labelIntegerSignedHexadecimal`]]
      * @group numeric
      */
    def labelIntegerSignedHexadecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedHexadecimal
    /** How should signed octal integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedOctal(bits:Int):* `labelIntegerSignedOctal`]]
      * @group numeric
      */
    def labelIntegerSignedOctal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedOctal
    /** How should signed binary integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[parsley.token.errors.ErrorConfig.labelIntegerSignedBinary(bits:Int):* `labelIntegerSignedBinary`]]
      * @group numeric
      */
    def labelIntegerSignedBinary(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedBinary
    /** How should generic signed integers of a given bit-width be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelIntegerSignedNumber(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedNumber

    /** How should the fact that the end of a decimal integer literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelIntegerNumberEnd `labelIntegerNumberEnd`]]
      * @group numeric
      */
    def labelIntegerDecimalEnd: LabelConfig = labelIntegerNumberEnd
    /** How should the fact that the end of a hexadecimal integer literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelIntegerNumberEnd `labelIntegerNumberEnd`]]
      * @group numeric
      */
    def labelIntegerHexadecimalEnd: LabelConfig = labelIntegerNumberEnd
    /** How should the fact that the end of an octal integer literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelIntegerNumberEnd `labelIntegerNumberEnd`]]
      * @group numeric
      */
    def labelIntegerOctalEnd: LabelConfig = labelIntegerNumberEnd
    /** How should the fact that the end of a binary integer literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelIntegerNumberEnd `labelIntegerNumberEnd`]]
      * @group numeric
      */
    def labelIntegerBinaryEnd: LabelConfig = labelIntegerNumberEnd
    /** How should the fact that the end of a generic integer literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelIntegerNumberEnd: LabelConfig = NotConfigured

    /** How should decimal reals should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumber `labelRealNumber`]]
      * @group numeric
      */
    def labelRealDecimal: LabelWithExplainConfig = labelRealNumber
    /** How should hexadecimal reals should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumber `labelRealNumber`]]
      * @group numeric
      */
    def labelRealHexadecimal: LabelWithExplainConfig = labelRealNumber
    /** How should octal reals should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumber `labelRealNumber`]]
      * @group numeric
      */
    def labelRealOctal: LabelWithExplainConfig = labelRealNumber
    /** How should binary reals should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumber `labelRealNumber`]]
      * @group numeric
      */
    def labelRealBinary: LabelWithExplainConfig = labelRealNumber
    /** How should generic reals should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelRealNumber: LabelWithExplainConfig = NotConfigured
    /** How should decimal floats should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealDecimal `labelRealDecimal`]]
      * @group numeric
      */
    def labelRealFloatDecimal: LabelWithExplainConfig = labelRealDecimal
    /** How should hexadecimal floats should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealHexadecimal `labelRealHexadecimal`]]
      * @group numeric
      */
    def labelRealFloatHexadecimal: LabelWithExplainConfig = labelRealHexadecimal
    /** How should octal floats should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealOctal `labelRealOctal`]]
      * @group numeric
      */
    def labelRealFloatOctal: LabelWithExplainConfig = labelRealOctal
    /** How should binary floats should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealBinary `labelRealBinary`]]
      * @group numeric
      */
    def labelRealFloatBinary: LabelWithExplainConfig = labelRealBinary
    /** How should generic floats should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumber `labelRealNumber`]]
      * @group numeric
      */
    def labelRealFloatNumber: LabelWithExplainConfig = labelRealNumber
    /** How should decimal doubles should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealDecimal `labelRealDecimal`]]
      * @group numeric
      */
    def labelRealDoubleDecimal: LabelWithExplainConfig = labelRealDecimal
    /** How should hexadecimal doubles should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealHexadecimal `labelRealHexadecimal`]]
      * @group numeric
      */
    def labelRealDoubleHexadecimal: LabelWithExplainConfig = labelRealHexadecimal
    /** How should octal doubles should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealOctal `labelRealOctal`]]
      * @group numeric
      */
    def labelRealDoubleOctal: LabelWithExplainConfig = labelRealOctal
    /** How should binary doubles should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealBinary `labelRealBinary`]]
      * @group numeric
      */
    def labelRealDoubleBinary: LabelWithExplainConfig = labelRealBinary
    /** How should generic doubles should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumber `labelRealNumber`]]
      * @group numeric
      */
    def labelRealDoubleNumber: LabelWithExplainConfig = labelRealNumber

    /** How should the fact that the end of a decimal real literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumberEnd `labelRealNumberEnd`]]
      * @group numeric
      */
    def labelRealDecimalEnd: LabelConfig = labelRealNumberEnd
    /** How should the fact that the end of a hexadecimal real literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumberEnd `labelRealNumberEnd`]]
      * @group numeric
      */
    def labelRealHexadecimalEnd: LabelConfig = labelRealNumberEnd
    /** How should the fact that the end of an octal real literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumberEnd `labelRealNumberEnd`]]
      * @group numeric
      */
    def labelRealOctalEnd: LabelConfig = labelRealNumberEnd
    /** How should the fact that the end of a binary real literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[labelRealNumberEnd `labelRealNumberEnd`]]
      * @group numeric
      */
    def labelRealBinaryEnd: LabelConfig = labelRealNumberEnd
    /** How should the fact that the end of a generic real literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelRealNumberEnd: LabelConfig = NotConfigured

    /** How should the "dot" that separates the integer and fractional part of a real number should be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelRealDot: LabelWithExplainConfig = NotConfigured
    /** How should the trailing exponents of a real number be referred to or explained within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelRealExponent: LabelWithExplainConfig = NotConfigured
    /** How should the fact that the end of an exponent part of a real literal is expected be referred to within an error.
      * @since 4.1.0
      * @note defaults to [[NotConfigured `NotConfigured`]]
      * @group numeric
      */
    def labelRealExponentEnd: LabelConfig = NotConfigured

    /** Even if leading and trailing zeros can be dropped, `.` is not a valid real number: this
      * method specifies how to report that to the user.
      * @since 4.1.0
      * @note defaults to a ''vanilla'' explain: "a real number cannot drop both a leading and trailing zero"
      * @group numeric
      */
    def preventRealDoubleDroppedZero: PreventDotIsZeroConfig = ZeroDotReason("a real number cannot drop both a leading and trailing zero")

    /** This method describes the content of the error when an integer literal is parsed and it is not within the
      * required bit-width.
      * @param min the smallest value the integer could have taken
      * @param max the largest value the integer could have taken
      * @param nativeRadix the radix that the integer was parsed using
      * @since 4.1.0
      * @note defaults to a ''specialised'' error describing what the min and max bounds are.
      * @group numeric
      */
    def filterIntegerOutOfBounds(min: BigInt, max: BigInt, nativeRadix: Int): FilterConfig[BigInt] = new SpecialisedMessage[BigInt](fullAmend = false) {
        def message(n: BigInt) = Seq(s"literal is not within the range ${min.toString(nativeRadix)} to ${max.toString(nativeRadix)}")
    }

    /** This method describes the content of the error when a real literal is parsed and it is not representable exactly as the required precision.
      * @param name the name of the required precision (one of `doubleName` or `floatName`)
      * @since 4.1.0
      * @note defaults to a ''specialised'' error stating that the literal is not exactly representable.
      * @group numeric
      */
    def filterRealNotExact(name: String): FilterConfig[BigDecimal] = new SpecialisedMessage[BigDecimal](fullAmend = false) {
        def message(n: BigDecimal) = Seq(s"literal cannot be represented exactly as an $name")
    }

    /** This method describes the content of the error when a real literal is parsed and it is not within the bounds perscribed by the required precision.
      * @param name the name of the required precision (one of `doubleName` or `floatName`)
      * @param min the smallest value the real could have taken
      * @param max the largest value the real could have taken
      * @since 4.1.0
      * @note defaults to a ''specialised'' error describing what the min and max bounds are.
      * @group numeric
      */
    def filterRealOutOfBounds(name: String, min: BigDecimal, max: BigDecimal): FilterConfig[BigDecimal] =
        new SpecialisedMessage[BigDecimal](fullAmend = false) {
            def message(n: BigDecimal) = Seq(s"literal is not within the range $min to $max and is not an $name")
        }

    // expensive ;)
    // this is not as effective as it may seem, because reasons cannot be hints
    // it's possible a preventative error could be more effective?
    /*def verifiedIntegerBadCharsUsedInLiteral: Option[(predicate.CharPredicate, Int => String)] =
        None*/

    /** What is the name given to doubles.
      * @since 4.1.0
      * @note defaults to "IEEE 754 double-precision float"
      * @group numeric
      */
    def doubleName: String = "IEEE 754 double-precision float"
    /** What is the name given to floats.
      * @since 4.1.0
      * @note defaults to "IEEE 754 single-precision float"
      * @group numeric
      */
    def floatName: String = "IEEE 754 single-precision float"

    private [token] final def labelDecimal(bits: Int, signed: Boolean): LabelWithExplainConfig = {
        if (signed) labelIntegerSignedDecimal(bits) else labelIntegerUnsignedDecimal(bits)
    }
    private [token] final def labelHexadecimal(bits: Int, signed: Boolean): LabelWithExplainConfig = {
        if (signed) labelIntegerSignedHexadecimal(bits) else labelIntegerUnsignedHexadecimal(bits)
    }
    private [token] final def labelOctal(bits: Int, signed: Boolean): LabelWithExplainConfig = {
        if (signed) labelIntegerSignedOctal(bits) else labelIntegerUnsignedOctal(bits)
    }
    private [token] final def labelBinary(bits: Int, signed: Boolean): LabelWithExplainConfig = {
        if (signed) labelIntegerSignedBinary(bits) else labelIntegerUnsignedBinary(bits)
    }
    private [token] final def labelNumber(bits: Int, signed: Boolean): LabelWithExplainConfig = {
        if (signed) labelIntegerSignedNumber(bits) else labelIntegerUnsignedNumber(bits)
    }

    /** How should an identifier be referred to in an error message.
      * @since 4.1.0
      * @note defaults to "identifier"
      * @group names
      */
    def labelNameIdentifier: String = "identifier"
    /** How should a user-defined operator be referred to in an error message.
      * @since 4.1.0
      * @note defaults to "operator"
      * @group names
      */
    def labelNameOperator: String = "operator"
    /** How should an illegally parsed hard keyword be referred to as an unexpected component.
      * @param v the illegal identifier
      * @since 4.1.0
      * @note defaults to "keyword v"
      * @group names
      */
    def unexpectedNameIllegalIdentifier(v: String): String = s"keyword $v"
    /** How should an illegally parsed hard operator be referred to as an unexpected component.
      * @since 4.1.0
      * @note defaults to "reserved operator v"
      * @group names
      */
    def unexpectedNameIllegalOperator(v: String): String = s"reserved operator $v"
    /** When parsing identifiers that are required to have specific start characters, how should bad identifiers be reported.
      * @since 4.1.0
      * @note defaults to unexpected "identifier v"
      * @group names
      */
    def filterNameIllFormedIdentifier: FilterConfig[String] = new Unexpected[String](fullAmend = false) {
        def unexpected(v: String) = s"identifer $v"
    }
    /** When parsing operators that are required to have specific start/end characters, how should bad operators be reported.
      * @since 4.1.0
      * @note defaults to unexpected "operator v"
      * @group names
      */
    def filterNameIllFormedOperator: FilterConfig[String] = new Unexpected[String](fullAmend = false) {
        def unexpected(v: String) = s"operator $v"
    }

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharAscii: LabelWithExplainConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharLatin1: LabelWithExplainConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharBasicMultilingualPlane: LabelWithExplainConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharUtf16: LabelWithExplainConfig = NotConfigured

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharAsciiEnd: LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharLatin1End: LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharBasicMultilingualPlaneEnd: LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelCharUtf16End: LabelConfig = NotConfigured

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringLatin1(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringUtf16(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringLatin1End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringUtf16End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringCharacter: LabelConfig = Label("string character")
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelGraphicCharacter: LabelWithExplainConfig = Label("graphic character")
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeSequence: LabelWithExplainConfig = Label("escape sequence") //different to "invalid escape sequence"!
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeNumeric(radix: Int): LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeNumericEnd(prefix: Char, radix: Int): LabelWithExplainConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeEnd: LabelWithExplainConfig = LabelAndReason("end of escape sequence", "invalid escape sequence")
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringEscapeEmpty: LabelConfig = NotConfigured
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringEscapeGap: LabelConfig = Label("string gap")
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def labelStringEscapeGapEnd: LabelConfig = Label("end of string gap")

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterCharNonBasicMultilingualPlane: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-BMP character"
    }
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterCharNonAscii: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-ascii character"
    }
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterCharNonLatin1: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-latin1 character"
    }

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterStringNonAscii: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder](fullAmend = false) {
        def message(@unused s: StringBuilder) = Seq("non-ascii characters in string literal, this is not allowed")
    }

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterStringNonLatin1: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder](fullAmend = false) {
        def message(@unused s: StringBuilder) = Seq("non-latin1 characters in string literal, this is not allowed")
    }

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterEscapeCharRequiresExactDigits(@unused radix: Int, needed: Seq[Int]): SpecialisedFilterConfig[Int] = new SpecialisedMessage[Int](fullAmend = false) {
        def message(got: Int) = Seq(s"numeric escape requires ${parsley.errors.helpers.combineAsList(needed.toList.map(_.toString))} digits, but only got $got")
    }

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def filterEscapeCharNumericSequenceIllegal(maxEscape: Int, radix: Int): SpecialisedFilterConfig[BigInt] = new SpecialisedMessage[BigInt](fullAmend = false) {
        def message(escapeChar: BigInt) = Seq(
            if (escapeChar > BigInt(maxEscape)) s"${escapeChar.toString(radix)} is greater than the maximum character value of ${BigInt(maxEscape).toString(radix)}"
            else s"illegal unicode codepoint: ${escapeChar.toString(radix)}")
    }

    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def verifiedCharBadCharsUsedInLiteral: VerifiedBadChars = Unverified
    /** TODO: Document
      * @since 4.1.0
      * @group text
      */
    def verifiedStringBadCharsUsedInLiteral: VerifiedBadChars = Unverified

    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolSemi: LabelConfig = Label("semicolon")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolComma: LabelConfig = Label("comma")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolColon: LabelConfig = Label("colon")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolDot: LabelConfig = Label("dot")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenParen: LabelConfig = Label("open parenthesis")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenBrace: LabelConfig = Label("open brace")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenSquare: LabelConfig = Label("open square bracket")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenAngle: LabelConfig = Label("open angle bracket")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingParen: LabelConfig = Label("closing parenthesis")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingBrace: LabelConfig = Label("closing brace")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingSquare: LabelConfig = Label("closing square bracket")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingAngle: LabelConfig = Label("closing angle bracket")
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolKeyword(symbol: String): LabelConfig = Label(symbol)
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOperator(symbol: String): LabelConfig = Label(symbol)
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
    /** TODO: Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"

    /** TODO: Document
      * @since 4.1.0
      * @group space
      */
    def labelSpaceEndOfLineComment: LabelWithExplainConfig = Label("end of comment")
    /** TODO: Document
      * @since 4.1.0
      * @group space
      */
    def labelSpaceEndOfMultiComment: LabelWithExplainConfig = Label("end of comment")
}
