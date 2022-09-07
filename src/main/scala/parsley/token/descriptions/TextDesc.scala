package parsley.token.descriptions.text

sealed abstract class NumberOfDigits
object NumberOfDigits {
    case class AtMost(n: Int) extends NumberOfDigits {
        if (n <= 0) throw new IllegalArgumentException("AtMost may only be passed a number of digits greater than 0")
    }
    case class Exactly(n0: Int, ns: Int*) extends NumberOfDigits {
        if (n0 <= 0) throw new IllegalArgumentException("Exactly may only be passed a number of digits greater than 0")
        if (ns.exists(_ <= 0)) throw new IllegalArgumentException("Exactly may only be passed a number of digits greater than 0")
    }
    case object Unbounded extends NumberOfDigits
}

sealed abstract class NumericEscape
object NumericEscape {
    case class Supported(prefix: Option[Char], numDigits: NumberOfDigits, maxValue: Int) extends NumericEscape
    case object Illegal extends NumericEscape
}

sealed abstract class CtrlEscape
object CtrlEscape {
    case class Supported(prefix: Char, mapping: Map[Char, Int]) extends CtrlEscape
    case object Illegal extends CtrlEscape
}

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
object EscapeDesc {
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

case class TextDesc (escapeChars: EscapeDesc,
                     characterLiteralEnd: Char,
                     stringEnds: Set[String],
                     multiStringEnds: Set[String],
                     graphicCharacter: Char => Boolean) {
}

object TextDesc {
    val plain = TextDesc(escapeChars = EscapeDesc.haskell,
                         characterLiteralEnd = '\'',
                         stringEnds = Set("\""),
                         multiStringEnds = Set.empty,
                         graphicCharacter = _ >= ' ')
}