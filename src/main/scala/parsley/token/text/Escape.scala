/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.{attempt, empty, pure, unit}
import parsley.character.{char, strings}
import parsley.combinator.choice
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.token.descriptions.{CtrlEscape, EscapeDesc, NumericEscape}
import parsley.token.numeric

class Escape private[token] (desc: EscapeDesc) {
    // NOTE: `strings`, while nice, is not perfect as it doesn't leverage a trie-based folding
    //       on the possibilities. We'll want trie-based folding here, or at least a specialised
    //       instruction that has the trie lookup logic baked in.
    val (x::xs) = desc.escMap.view.map {
        case (e, c) => e -> pure(c)
    }.toList
    // We do need to backtrack out of this if things go wrong, it's possible another escape sequence might share a lead
    private val escMapped = attempt(strings(x, xs: _*))
    private val ctrlEscape = desc.ctrlEscape match {
        case CtrlEscape.Illegal => empty
        case CtrlEscape.Supported(prefix, mapping) => char(prefix) *> choice(mapping.view.map {
            case (e, c) => char(e) #> c.toInt
        }.toSeq: _*)
    }

    def boundedChar(p: Parsley[BigInt], maxValue: Int, prefix: Option[Char], radix: Int): Parsley[Int] =
        prefix.fold(unit)(c => char(c).void) *> amend {
            val prefixString = prefix.fold("")(c => s"c")
            entrench(p).collectMsg(n => Seq(
                if (n > maxValue)
                    s"\\$prefixString${n.toString(radix)} is greater than the maximum character of \\$prefixString${BigInt(maxValue).toString(radix)}"
                else s"illegal unicode codepoint: \\$prefixString${n.toString(radix)}")) {
                case n if n <= maxValue && Character.isValidCodePoint(n.toInt) => n.toInt
            }
        }

    def fromDesc(radix: Int, desc: NumericEscape, integer: Parsley[BigInt]): Parsley[Int] = desc match {
        case NumericEscape.Illegal => empty
        case NumericEscape.Supported(prefix, maxValue) => boundedChar(integer, maxValue, prefix, radix)
    }

    // TODO: These actually might have a fixed number of digits, we should add configuration for that
    private val decimalEscape = fromDesc(radix = 10, desc.decimalEscape, numeric.Generic.zeroAllowedDecimal)
    private val hexadecimalEscape = fromDesc(radix = 16, desc.hexadecimalEscape, numeric.Generic.zeroAllowedHexadecimal)
    private val octalEscape = fromDesc(radix = 8, desc.octalEscape, numeric.Generic.zeroAllowedOctal)
    private val binaryEscape = fromDesc(radix = 2, desc.binaryEscape, numeric.Generic.zeroAllowedBinary)
    private val numericEscape = decimalEscape <|> hexadecimalEscape <|> octalEscape <|> binaryEscape
    val escapeCode = escMapped <|> ctrlEscape <|> numericEscape
    val escapeChar = char(desc.escBegin) *> escapeCode
}
