/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.{/*attempt,*/ empty}
import parsley.character.{/*bit, */char/*, digit, hexDigit, octDigit*/}
//import parsley.combinator.ensure
//import parsley.implicits.zipped.Zipped3
import parsley.token.descriptions.text.{EscapeDesc, NumberOfDigits, NumericEscape}
import parsley.token.errors.{ErrorConfig, NotConfigured}
import parsley.token.numeric

import parsley.internal.deepembedding.singletons.token

/** This class' implementation has been optimised for performance. If you've stumbled on
  * this file hoping to learn about how it works, this is the wrong place. The original,
  * unoptimised implementation is preserved for testing in the corresponding place in the
  * "test" folder. This is because a requirement of optimisation is that the semantics can
  * still be implemented by plain combinators. Go look there!
  */
private [token] class Escape(desc: EscapeDesc, err: ErrorConfig, generic: numeric.Generic) {
    private val escMapped = if (desc.escTrie.isEmpty) empty else new Parsley(new token.EscapeMapped(desc.escTrie, desc.escs))

    private def boundedChar(p: Parsley[BigInt], maxValue: Int, prefix: Option[Char], radix: Int) = err.labelEscapeNumeric(radix) {
        val numericTail = err.filterEscapeCharNumericSequenceIllegal(maxValue, radix).collect(p) {
            case n if n <= maxValue && Character.isValidCodePoint(n.toInt) => n.toInt
        }
        prefix match {
            case None => numericTail
            case Some(c) => char(c) *> err.labelEscapeNumericEnd(c, radix)(numericTail)
        }
    }

    private def atMost(n: Int, radix: Int): Parsley[BigInt] = new Parsley(new token.EscapeAtMost(n, radix))
    /*private def exactly(n: Int, full: Int, radix: Int, reqDigits: Seq[Int]): Parsley[BigInt] = {
        new Parsley(new token.EscapeExactly(n, full, radix, err.filterEscapeCharRequiresExactDigits(radix, reqDigits)))
    }*/

    //private lazy val digitsParsed = parsley.registers.Reg.make[Int]
    private def oneOfExactly(n: Int, ns: List[Int], radix: Int): Parsley[BigInt] = {
        new Parsley(new token.EscapeOneOfExactly(radix, (n :: ns).sorted, err.filterEscapeCharRequiresExactDigits(radix, (n :: ns).sorted)))
        /*val reqDigits@(m :: ms) = (n :: ns).sorted // make this a precondition of the description?
        def go(digits: Int, m: Int, ns: List[Int]): Parsley[BigInt] = ns match {
            case Nil => exactly(digits, m, radix, reqDigits) <* digitsParsed.put(digits)
            case n :: ns  =>
                val theseDigits = exactly(digits, m, radix, reqDigits)
                val restDigits = (
                        attempt(go(n-m, n, ns).map(Some(_)) <* digitsParsed.modify(_ + digits))
                      | digitsParsed.put(digits) #> None
                )
                (theseDigits, restDigits, digitsParsed.get).zipped[BigInt] {
                    case (x, None, _) => x
                    case (x, Some(y), exp) => (x * BigInt(radix).pow(exp - digits) + y) // digits is removed here, because it's been added before the get
                }
        }
        go(m, m, ms)*/
    }

    private def fromDesc(radix: Int, desc: NumericEscape, integer: =>Parsley[BigInt]): Parsley[Int] = desc match {
        case NumericEscape.Illegal => empty
        case NumericEscape.Supported(prefix, numberOfDigits, maxValue) => numberOfDigits match {
            case NumberOfDigits.Unbounded         => boundedChar(integer, maxValue, prefix, radix)
            case NumberOfDigits.AtMost(n)         => boundedChar(atMost(n, radix), maxValue, prefix, radix)
            case NumberOfDigits.Exactly(n, ns@_*) => boundedChar(oneOfExactly(n, ns.toList, radix), maxValue, prefix, radix)
        }
    }

    private val decimalEscape = fromDesc(radix = 10, desc.decimalEscape, generic.zeroAllowedDecimal(NotConfigured))
    private val hexadecimalEscape = fromDesc(radix = 16, desc.hexadecimalEscape, generic.zeroAllowedHexadecimal(NotConfigured))
    private val octalEscape = fromDesc(radix = 8, desc.octalEscape, generic.zeroAllowedOctal(NotConfigured))
    private val binaryEscape = fromDesc(radix = 2, desc.binaryEscape, generic.zeroAllowedBinary(NotConfigured))
    private val numericEscape = decimalEscape <|> hexadecimalEscape <|> octalEscape <|> binaryEscape
    val escapeCode = err.labelEscapeEnd(escMapped <|> numericEscape)
    val escapeBegin = err.labelEscapeSequence(char(desc.escBegin))
    val escapeChar = escapeBegin *> escapeCode
}
