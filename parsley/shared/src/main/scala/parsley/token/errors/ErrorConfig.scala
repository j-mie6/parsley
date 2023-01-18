/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.XCompat.unused

/** TODO:
  * @since 4.1.0
  * @group errconfig
  *
  * @groupprio numeric 0
  * @groupname numeric Numeric Errors
  * @groupdesc numeric
  *
  * @groupprio text 0
  * @groupname text Text Errors
  * @groupdesc text
  *
  * @groupprio names 0
  * @groupname names Name Errors
  * @groupdesc names
  *
  * @groupprio symbol 0
  * @groupname symbol Symbol Errors
  * @groupdesc symbol
  *
  * @groupprio space 0
  * @groupname space Space Errors
  * @groupdesc space
  *
  */
class ErrorConfig {
    // numeric
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelNumericBreakChar: LabelWithExplainConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedDecimal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedHexadecimal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedOctal: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedBinary: LabelWithExplainConfig = labelIntegerUnsignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedNumber: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedDecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedDecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedHexadecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedHexadecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedOctal(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedOctal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedBinary(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedBinary
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerUnsignedNumber(@unused bits: Int): LabelWithExplainConfig = labelIntegerUnsignedNumber

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedDecimal: LabelWithExplainConfig = labelIntegerSignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedHexadecimal: LabelWithExplainConfig = labelIntegerSignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedOctal: LabelWithExplainConfig = labelIntegerSignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedBinary: LabelWithExplainConfig = labelIntegerSignedNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedNumber: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedDecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedDecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedHexadecimal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedHexadecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedOctal(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedOctal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedBinary(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedBinary
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerSignedNumber(@unused bits: Int): LabelWithExplainConfig = labelIntegerSignedNumber

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerDecimalEnd: LabelConfig = labelIntegerNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerHexadecimalEnd: LabelConfig = labelIntegerNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerOctalEnd: LabelConfig = labelIntegerNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerBinaryEnd: LabelConfig = labelIntegerNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelIntegerNumberEnd: LabelConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDecimal: LabelWithExplainConfig = labelRealNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealHexadecimal: LabelWithExplainConfig = labelRealNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealOctal: LabelWithExplainConfig = labelRealNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealBinary: LabelWithExplainConfig = labelRealNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealNumber: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealFloatDecimal: LabelWithExplainConfig = labelRealDecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealFloatHexadecimal: LabelWithExplainConfig = labelRealHexadecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealFloatOctal: LabelWithExplainConfig = labelRealOctal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealFloatBinary: LabelWithExplainConfig = labelRealBinary
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealFloatNumber: LabelWithExplainConfig = labelRealNumber
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDoubleDecimal: LabelWithExplainConfig = labelRealDecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDoubleHexadecimal: LabelWithExplainConfig = labelRealHexadecimal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDoubleOctal: LabelWithExplainConfig = labelRealOctal
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDoubleBinary: LabelWithExplainConfig = labelRealBinary
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDoubleNumber: LabelWithExplainConfig = labelRealNumber

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDecimalEnd: LabelConfig = labelRealNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealHexadecimalEnd: LabelConfig = labelRealNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealOctalEnd: LabelConfig = labelRealNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealBinaryEnd: LabelConfig = labelRealNumberEnd
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealNumberEnd: LabelConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealDot: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def labelRealExponent: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
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

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def preventRealDoubleDroppedZero: PreventDotIsZeroConfig = ZeroDotReason("a real number cannot drop both a leading and trailing zero")

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def filterIntOutOfBounds(min: BigInt, max: BigInt, nativeRadix: Int): SpecialisedFilterConfig[BigInt] =
        new SpecialisedMessage[BigInt](fullAmend = false) {
            def message(n: BigInt) = Seq(
                if (n < min) s"literal ${n.toString(nativeRadix)} is less than min value of ${min.toString(nativeRadix)}"
                else s"literal ${n.toString(nativeRadix)} is larger than max value of ${max.toString(nativeRadix)}"
            )
        }

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def filterRealNotExact(name: String): SpecialisedFilterConfig[BigDecimal] = new SpecialisedMessage[BigDecimal](fullAmend = false) {
        def message(n: BigDecimal) = Seq(s"literal $n cannot be represented exactly as an $name")
    }

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def filterRealOutOfBounds(name: String, min: BigDecimal, @unused max: BigDecimal): SpecialisedFilterConfig[BigDecimal] = new SpecialisedMessage[BigDecimal](fullAmend = false) {
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

    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def doubleName: String = "IEEE 754 double-precision float"
    /** @todo Document
      * @since 4.1.0
      * @group numeric
      */
    def floatName: String = "IEEE 754 single-precision float"

    /** @todo Document
      * @since 4.1.0
      * @group names
      */
    def labelNameIdentifier: String = "identifier"
    /** @todo Document
      * @since 4.1.0
      * @group names
      */
    def labelNameOperator: String = "operator"
    /** @todo Document
      * @since 4.1.0
      * @group names
      */
    def unexpectedNameIllegalIdentifier(v: String): String = s"keyword $v"
    /** @todo Document
      * @since 4.1.0
      * @group names
      */
    def unexpectedNameIllegalOperator(v: String): String = s"reserved operator $v"
    /** @todo Document
      * @since 4.1.0
      * @group names
      */
    def filterNameIllFormedIdentifier: FilterConfig[String] = new Unexpected[String](fullAmend = false) {
        def unexpected(v: String) = s"identifer $v"
    }
    /** @todo Document
      * @since 4.1.0
      * @group names
      */
    def filterNameIllFormedOperator: FilterConfig[String] = new Unexpected[String](fullAmend = false) {
        def unexpected(v: String) = s"operator $v"
    }

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharAscii: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharLatin1: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharBasicMultilingualPlane: LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharUtf16: LabelWithExplainConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharAsciiEnd: LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharLatin1End: LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharBasicMultilingualPlaneEnd: LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelCharUtf16End: LabelConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringLatin1(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringUtf16(multi: Boolean, raw: Boolean): LabelWithExplainConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringLatin1End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringUtf16End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringCharacter: LabelConfig = Label("string character")
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelGraphicCharacter: LabelWithExplainConfig = Label("graphic character")
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeSequence: LabelWithExplainConfig = Label("escape sequence") //different to "invalid escape sequence"!
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeNumeric(radix: Int): LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeNumericEnd(prefix: Char, radix: Int): LabelWithExplainConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelEscapeEnd: LabelWithExplainConfig = LabelAndReason("end of escape sequence", "invalid escape sequence")
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringEscapeEmpty: LabelConfig = NotConfigured
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringEscapeGap: LabelConfig = Label("string gap")
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def labelStringEscapeGapEnd: LabelConfig = Label("end of string gap")

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterCharNonBasicMultilingualPlane: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-BMP character"
    }
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterCharNonAscii: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-ascii character"
    }
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterCharNonLatin1: VanillaFilterConfig[Int] = new Because[Int](fullAmend = false) {
        def reason(@unused x: Int) = "non-latin1 character"
    }

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterStringNonAscii: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder](fullAmend = false) {
        def message(@unused s: StringBuilder) = Seq("non-ascii characters in string literal, this is not allowed")
    }

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterStringNonLatin1: SpecialisedFilterConfig[StringBuilder] = new SpecialisedMessage[StringBuilder](fullAmend = false) {
        def message(@unused s: StringBuilder) = Seq("non-latin1 characters in string literal, this is not allowed")
    }

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterEscapeCharRequiresExactDigits(@unused radix: Int, needed: Seq[Int]): SpecialisedFilterConfig[Int] = new SpecialisedMessage[Int](fullAmend = false) {
        def message(got: Int) = Seq(s"numeric escape requires ${parsley.errors.helpers.combineAsList(needed.toList.map(_.toString))} digits, but only got $got")
    }

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def filterEscapeCharNumericSequenceIllegal(maxEscape: Int, radix: Int): SpecialisedFilterConfig[BigInt] = new SpecialisedMessage[BigInt](fullAmend = false) {
        def message(escapeChar: BigInt) = Seq(
            if (escapeChar > BigInt(maxEscape)) s"${escapeChar.toString(radix)} is greater than the maximum character value of ${BigInt(maxEscape).toString(radix)}"
            else s"illegal unicode codepoint: ${escapeChar.toString(radix)}")
    }

    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def verifiedCharBadCharsUsedInLiteral: VerifiedBadChars = Unverified
    /** @todo Document
      * @since 4.1.0
      * @group text
      */
    def verifiedStringBadCharsUsedInLiteral: VerifiedBadChars = Unverified

    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolSemi: LabelConfig = Label("semicolon")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolComma: LabelConfig = Label("comma")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolColon: LabelConfig = Label("colon")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolDot: LabelConfig = Label("dot")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenParen: LabelConfig = Label("open parenthesis")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenBrace: LabelConfig = Label("open brace")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenSquare: LabelConfig = Label("open square bracket")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOpenAngle: LabelConfig = Label("open angle bracket")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingParen: LabelConfig = Label("closing parenthesis")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingBrace: LabelConfig = Label("closing brace")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingSquare: LabelConfig = Label("closing square bracket")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolClosingAngle: LabelConfig = Label("closing angle bracket")
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolKeyword(symbol: String): LabelConfig = Label(symbol)
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolOperator(symbol: String): LabelConfig = Label(symbol)
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
    /** @todo Document
      * @since 4.1.0
      * @group symbol
      */
    def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"

    /** @todo Document
      * @since 4.1.0
      * @group space
      */
    def labelSpaceEndOfLineComment: LabelWithExplainConfig = Label("end of comment")
    /** @todo Document
      * @since 4.1.0
      * @group space
      */
    def labelSpaceEndOfMultiComment: LabelWithExplainConfig = Label("end of comment")
}
