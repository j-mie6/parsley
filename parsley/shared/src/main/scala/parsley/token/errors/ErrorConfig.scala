/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.XCompat.unused

/** TODO:
  * @since 4.1.0
  */
class ErrorConfig {
    // numeric
    def labelNumericBreakChar: LabelWithExplainConfig = NotConfigured

    def labelIntegerUnsignedDecimal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedHexadecimal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedOctal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedBinary: LabelWithExplainConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedNumber: LabelWithExplainConfig = NotConfigured
    def labelIntegerUnsignedDecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedDecimal
    def labelIntegerUnsignedHexadecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedHexadecimal
    def labelIntegerUnsignedOctal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedOctal
    def labelIntegerUnsignedBinary(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedBinary
    def labelIntegerUnsignedNumber(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedNumber

    def labelIntegerSignedDecimal: LabelWithExplainConfig = labelIntegerSignedNumber
    def labelIntegerSignedHexadecimal: LabelWithExplainConfig = labelIntegerSignedNumber
    def labelIntegerSignedOctal: LabelWithExplainConfig = labelIntegerSignedNumber
    def labelIntegerSignedBinary: LabelWithExplainConfig = labelIntegerSignedNumber
    def labelIntegerSignedNumber: LabelWithExplainConfig = NotConfigured
    def labelIntegerSignedDecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedDecimal
    def labelIntegerSignedHexadecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedHexadecimal
    def labelIntegerSignedOctal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedOctal
    def labelIntegerSignedBinary(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedBinary
    def labelIntegerSignedNumber(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedNumber

    def labelIntegerDecimalEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerHexadecimalEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerOctalEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerBinaryEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerNumberEnd: LabelConfig = NotConfigured

    def labelRealDecimal: LabelWithExplainConfig = labelRealNumber
    def labelRealHexadecimal: LabelWithExplainConfig = labelRealNumber
    def labelRealOctal: LabelWithExplainConfig = labelRealNumber
    def labelRealBinary: LabelWithExplainConfig = labelRealNumber
    def labelRealNumber: LabelWithExplainConfig = NotConfigured
    def labelRealFloatDecimal: LabelWithExplainConfig = labelRealDecimal
    def labelRealFloatHexadecimal: LabelWithExplainConfig = labelRealHexadecimal
    def labelRealFloatOctal: LabelWithExplainConfig = labelRealOctal
    def labelRealFloatBinary: LabelWithExplainConfig = labelRealBinary
    def labelRealFloatNumber: LabelWithExplainConfig = labelRealNumber
    def labelRealDoubleDecimal: LabelWithExplainConfig = labelRealDecimal
    def labelRealDoubleHexadecimal: LabelWithExplainConfig = labelRealHexadecimal
    def labelRealDoubleOctal: LabelWithExplainConfig = labelRealOctal
    def labelRealDoubleBinary: LabelWithExplainConfig = labelRealBinary
    def labelRealDoubleNumber: LabelWithExplainConfig = labelRealNumber

    def labelRealDecimalEnd: LabelConfig = labelRealNumberEnd
    def labelRealHexadecimalEnd: LabelConfig = labelRealNumberEnd
    def labelRealOctalEnd: LabelConfig = labelRealNumberEnd
    def labelRealBinaryEnd: LabelConfig = labelRealNumberEnd
    def labelRealNumberEnd: LabelConfig = NotConfigured

    def labelRealDot: LabelWithExplainConfig = NotConfigured
    def labelRealExponent: LabelWithExplainConfig = NotConfigured
    def labelRealExponentEnd: LabelConfig = NotConfigured

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

    def preventRealDoubleDroppedZero: PreventDotIsZeroConfig = ZeroDotReason("a real number cannot drop both a leading and trailing zero")

    def messageIntOutOfBounds(min: BigInt, max: BigInt, nativeRadix: Int): SpecialisedFilterConfig[BigInt] =
        new SpecialisedMessage[BigInt](fullAmend = false) {
            def message(n: BigInt) = Seq(
                if (n < min) s"literal ${n.toString(nativeRadix)} is less than min value of ${min.toString(nativeRadix)}"
                else s"literal ${n.toString(nativeRadix)} is larger than max value of ${max.toString(nativeRadix)}"
            )
        }

    def messageRealNotExact(name: String): SpecialisedFilterConfig[BigDecimal] = new SpecialisedMessage[BigDecimal](fullAmend = false) {
        def message(n: BigDecimal) = Seq(s"literal $n cannot be represented exactly as an $name")
    }

    def messageRealOutOfBounds(name: String, min: BigDecimal, @unused max: BigDecimal): SpecialisedFilterConfig[BigDecimal] = new SpecialisedMessage[BigDecimal](fullAmend = false) {
        def message(n: BigDecimal) = Seq(
            if (n < min) s"literal $n is too small to be an $name"
            else s"literal $n is too large to be an $name"
        )
    }

    // expensive ;)
    // this is not as effective as it may seem, because reasons cannot be hints
    // it's possible a preventative error could be more effective?
    /*def verifiedIntegerBadCharsUsedInLiteral: Option[(predicate.CharPredicate, Int => String)] =
        None*/

    def doubleName: String = "IEEE 754 double-precision float"
    def floatName: String = "IEEE 754 single-precision float"

    // names
    def labelNameIdentifier: String = "identifier"
    def labelNameOperator: String = "operator"
    def unexpectedNameIllegalIdentifier(v: String): String = s"keyword $v"
    def unexpectedNameIllegalOperator(v: String): String = s"reserved operator $v"
    def unexpectedNameIllFormedIdentifier: FilterConfig[String] = new Unexpected[String](fullAmend = false) {
        def unexpected(v: String) = s"identifer $v"
    }
    def unexpectedNameIllFormedOperator: FilterConfig[String] = new Unexpected[String](fullAmend = false) {
        def unexpected(v: String) = s"operator $v"
    }

    // text
    def labelCharAscii: LabelWithExplainConfig = NotConfigured
    def labelCharLatin1: LabelWithExplainConfig = NotConfigured
    def labelCharBasicMultilingualPlane: LabelWithExplainConfig = NotConfigured
    def labelCharUtf16: LabelWithExplainConfig = NotConfigured

    def labelCharAsciiEnd: LabelConfig = NotConfigured
    def labelCharLatin1End: LabelConfig = NotConfigured
    def labelCharBasicMultilingualPlaneEnd: LabelConfig = NotConfigured
    def labelCharUtf16End: LabelConfig = NotConfigured

    def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured
    def labelStringLatin1(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured
    def labelStringUtf16(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured

    def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    def labelStringLatin1End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    def labelStringUtf16End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured

    def labelStringCharacter: LabelConfig = Label("string character")
    def labelGraphicCharacter: LabelWithExplainConfig = Label("graphic character")
    def labelEscapeSequence: LabelWithExplainConfig = Label("escape sequence") //different to "invalid escape sequence"!
    def labelEscapeNumeric(radix: Int): LabelConfig = NotConfigured
    def labelEscapeNumericEnd(prefix: Char, radix: Int): LabelWithExplainConfig = NotConfigured
    def labelEscapeEnd: LabelWithExplainConfig = LabelAndReason("end of escape sequence", "invalid escape sequence")
    def labelStringEscapeEmpty: LabelConfig = NotConfigured
    def labelStringEscapeGap: LabelConfig = Label("string gap")
    def labelStringEscapeGapEnd: LabelConfig = Label("end of string gap")

    def unexpectedCharNonBasicMultilingualPlane: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-BMP character"
    }
    def unexpectedCharNonAscii: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-ascii character"
    }
    def unexpectedCharNonLatin1: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-latin1 character"
    }

    def messageStringNonAscii: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder](fullAmend = false) {
        def message(@unused s: StringBuilder) = Seq("non-ascii characters in string literal, this is not allowed")
    }

    def messageStringNonLatin1: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder](fullAmend = false) {
        def message(@unused s: StringBuilder) = Seq("non-latin1 characters in string literal, this is not allowed")
    }

    def messageEscapeCharRequiresExactDigits(@unused radix: Int, needed: Seq[Int]): SpecialisedFilterConfig[Int] = new SpecialisedMessage[Int](fullAmend = false) {
        def message(got: Int) = Seq(s"numeric escape requires ${parsley.errors.helpers.combineAsList(needed.toList.map(_.toString))} digits, but only got $got")
    }

    def messageEscapeCharNumericSequenceIllegal(maxEscape: Int, radix: Int): SpecialisedFilterConfig[BigInt] = new SpecialisedMessage[BigInt](fullAmend = false) {
        def message(escapeChar: BigInt) = Seq(
            if (escapeChar > BigInt(maxEscape)) s"${escapeChar.toString(radix)} is greater than the maximum character value of ${BigInt(maxEscape).toString(radix)}"
            else s"illegal unicode codepoint: ${escapeChar.toString(radix)}")
    }

    // expensive ;)
    def verifiedCharBadCharsUsedInLiteral: VerifiedBadChars = Unverified
    def verifiedStringBadCharsUsedInLiteral: VerifiedBadChars = Unverified

    // symbol
    def labelSymbolSemi: LabelConfig = Label("semicolon")
    def labelSymbolComma: LabelConfig = Label("comma")
    def labelSymbolColon: LabelConfig = Label("colon")
    def labelSymbolDot: LabelConfig = Label("dot")
    def labelSymbolOpenParen: LabelConfig = Label("open parenthesis")
    def labelSymbolOpenBrace: LabelConfig = Label("open brace")
    def labelSymbolOpenSquare: LabelConfig = Label("open square bracket")
    def labelSymbolOpenAngle: LabelConfig = Label("open angle bracket")
    def labelSymbolClosingParen: LabelConfig = Label("closing parenthesis")
    def labelSymbolClosingBrace: LabelConfig = Label("closing brace")
    def labelSymbolClosingSquare: LabelConfig = Label("closing square bracket")
    def labelSymbolClosingAngle: LabelConfig = Label("closing angle bracket")
    def labelSymbolKeyword(symbol: String): LabelConfig = Label(symbol)
    def labelSymbolOperator(symbol: String): LabelConfig = Label(symbol)
    def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
    def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"

    // space
    def labelSpaceEndOfLineComment: LabelWithExplainConfig = Label("end of comment")
    def labelSpaceEndOfMultiComment: LabelWithExplainConfig = Label("end of comment")
}
