package parsley.token.descriptions.text

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class NumberOfDigits
/** TODO:
  *
  * @since 4.0.0
  */
object NumberOfDigits {
    /** TODO:
      *
      * @param n
      * @since 4.0.0
      */
    case class AtMost(n: Int) extends NumberOfDigits {
        if (n <= 0) throw new IllegalArgumentException("AtMost may only be passed a number of digits greater than 0")
    }
    /** TODO:
      *
      * @param n0
      * @param ns
      * @since 4.0.0
      */
    case class Exactly(n0: Int, ns: Int*) extends NumberOfDigits {
        if (n0 <= 0) throw new IllegalArgumentException("Exactly may only be passed a number of digits greater than 0")
        if (ns.exists(_ <= 0)) throw new IllegalArgumentException("Exactly may only be passed a number of digits greater than 0")
    }
    /** TODO:
      *
      * @since 4.0.0
      */
    case object Unbounded extends NumberOfDigits
}

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class NumericEscape
/** TODO:
  *
  * @since 4.0.0
  */
object NumericEscape {
    /** TODO:
      *
      * @param prefix
      * @param numDigits
      * @param maxValue
      * @since 4.0.0
      */
    case class Supported(prefix: Option[Char], numDigits: NumberOfDigits, maxValue: Int) extends NumericEscape
    /** TODO:
      *
      * @since 4.0.0
      */
    case object Illegal extends NumericEscape
}

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class CtrlEscape
/** TODO:
  *
  * @since 4.0.0
  */
object CtrlEscape {
    /** TODO:
      *
      * @param prefix
      * @param mapping
      * @since 4.0.0
      */
    case class Supported(prefix: Char, mapping: Map[Char, Int]) extends CtrlEscape
    /** TODO:
      *
      * @since 4.0.0
      */
    case object Illegal extends CtrlEscape
}

/** This class describes the valid escape sequences within character and string literals.
  *
  * This allows for the definition of different escape sequences as direct literals, mapping
  * from single or multiple characters to specific values, numeric escape sequences with different
  * bases, as well as supporting zero-width escapes and line continuations via string gaps.
  *
  * @param escBegin the character that starts an escape sequence, very often this is `'\\'`.
  * @param literals the characters that can be directly escaped, but still represent themselves, for instance `'"'`, or `'\\'`.
  * @param singleMap the possible single-character escape sequences and the (full UTF-16) character they map to, for instance `'n' -> 0xa`.
  * @param multiMap the possible multi-character escape sequences and the (full UTF-16) character they map to.
  * @param decimalEscape TODO:
  * @param hexadecimalEscape TODO:
  * @param octalEscape TODO:
  * @param binaryEscape TODO:
  * @param emptyEscape if one should exist, the character which has no effect on the string but can be used to disambiguate other
                       escape sequences: in Haskell this would be `\&`.
  * @param gapsSupported specifies whether or not ''string gaps'' are supported: this is where whitespace can be injected between two
  *                      `escBegin` characters and this will all be ignored in the final string, such that `"hello \      \world"` is `"hello world"`.
  * @param ctrlEscape describes possible escapes that take an additional prefix character, used to represent old control-key combinations:
  *                   in Haskell, these are of the form `\^x` where `x` is some character.
  * @since 4.0.0
  */
case class EscapeDesc (escBegin: Char,
                       literals: Set[Char],
                       singleMap: Map[Char, Int],
                       multiMap: Map[String, Int],
                       decimalEscape: NumericEscape,
                       hexadecimalEscape: NumericEscape,
                       octalEscape: NumericEscape,
                       binaryEscape: NumericEscape,
                       emptyEscape: Option[Char],
                       gapsSupported: Boolean,
                       ctrlEscape: CtrlEscape,
                      ) {
    // TODO: this needs to be a Radix, I think we'll need parsley.collection.immutable.Radix too
    private [token] val escMap = multiMap ++ literals.map(c => s"$c" -> c.toInt) ++ singleMap.map {
        case (k, v) => s"$k" -> v
    }
}
/** TODO:
  *
  * @since 4.0.0
  */
object EscapeDesc {
    /** TODO:
      *
      * @since 4.0.0
      */
    val haskell = EscapeDesc(escBegin = '\\',
                             literals = Set('\'', '\"', '\\'),
                             singleMap = Map('0' -> 0x0000,
                                             'a' -> 0x0007,
                                             'b' -> 0x0008,
                                             'f' -> 0x000c,
                                             'n' -> 0x000a,
                                             'r' -> 0x000d,
                                             't' -> 0x0009,
                                             'v' -> 0x000b),
                             multiMap = Map("NUL" -> 0x0000,
                                            "SOH" -> 0x0001,
                                            "STX" -> 0x0002,
                                            "ETX" -> 0x0003,
                                            "EOT" -> 0x0004,
                                            "ENQ" -> 0x0005,
                                            "ACK" -> 0x0006,
                                            "BEL" -> 0x0007,
                                            "BS"  -> 0x0008,
                                            "HT"  -> 0x0009,
                                            "LF"  -> 0x000a,
                                            "VT"  -> 0x000b,
                                            "FF"  -> 0x000c,
                                            "CR"  -> 0x000d,
                                            "SO"  -> 0x000e,
                                            "SI"  -> 0x000f,
                                            "DLE" -> 0x0010,
                                            "DC1" -> 0x0011,
                                            "DC2" -> 0x0012,
                                            "DC3" -> 0x0013,
                                            "DC4" -> 0x0014,
                                            "NAK" -> 0x0015,
                                            "SYN" -> 0x0016,
                                            "ETB" -> 0x0017,
                                            "CAN" -> 0x0018,
                                            "EM"  -> 0x0019,
                                            "SUB" -> 0x001a,
                                            "ESC" -> 0x001b,
                                            "FS"  -> 0x001c,
                                            "GS"  -> 0x001d,
                                            "RS"  -> 0x001e,
                                            "US"  -> 0x001f,
                                            "SP"  -> 0x0020,
                                            "DEL" -> 0x007f),
                             decimalEscape = NumericEscape.Supported(prefix = None, NumberOfDigits.Unbounded, maxValue = 0x10ffff),
                             hexadecimalEscape = NumericEscape.Supported(prefix = Some('x'), NumberOfDigits.Unbounded, maxValue = 0x10ffff),
                             octalEscape = NumericEscape.Supported(prefix = Some('o'), NumberOfDigits.Unbounded, maxValue = 0x10ffff),
                             binaryEscape = NumericEscape.Illegal,
                             emptyEscape = Some('&'),
                             gapsSupported = true,
                             ctrlEscape = CtrlEscape.Supported(prefix = '^', mapping = ('@' to '_').map(c => c -> (c - '@')).toMap))
}

/** TODO:
  *
  * @param escapeSequences
  * @param characterLiteralEnd
  * @param stringEnds
  * @param multiStringEnds
  * @param graphicCharacter
  * @since 4.0.0
  */
case class TextDesc (escapeSequences: EscapeDesc,
                     characterLiteralEnd: Char,
                     stringEnds: Set[String],
                     multiStringEnds: Set[String],
                     graphicCharacter: Char => Boolean) {
}

/** TODO:
  *
  * @since 4.0.0
  */
object TextDesc {
    /** TODO:
      *
      * @since 4.0.0
      */
    val plain = TextDesc(escapeSequences = EscapeDesc.haskell,
                         characterLiteralEnd = '\'',
                         stringEnds = Set("\""),
                         multiStringEnds = Set.empty,
                         graphicCharacter = _ >= ' ')
}
