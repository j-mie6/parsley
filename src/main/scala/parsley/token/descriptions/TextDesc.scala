package parsley.token.descriptions

private [token] // TODO: remove
sealed abstract class NumericEscape
private [token] // TODO: remove
object NumericEscape {
    case class Supported(prefix: Option[Char], maxValue: Int) extends NumericEscape
    case object Illegal extends NumericEscape
}

private [token] // TODO: remove
sealed abstract class CtrlEscape
private [token] // TODO: remove
object CtrlEscape {
    case class Supported(prefix: Option[Char], mapping: Map[Char, Char]) extends CtrlEscape
    case object Illegal extends CtrlEscape
}

private [token] // TODO: remove
case class EscapeDesc (escBegin: Char,
                       literals: Set[Char],
                       singleMap: Map[Char, Char],
                       multiMap: Map[String, Char],
                       decimalEscape: NumericEscape,
                       hexadecimalEscape: NumericEscape,
                       octalEscape: NumericEscape,
                       binaryEscape: NumericEscape,
                       emptyEscape: Option[Char],
                       gapsSupported: Boolean,
                       ctrlEscape: CtrlEscape,
                      ) {
    // TODO: this needs to be a Radix, I think we'll need parsley.collection.immutable.Radix too
    private [token] val escMap = multiMap ++ literals.map(c => s"c" -> c) ++ singleMap.map {
        case (k, v) => s"k" -> v
    }
}

private [token] // TODO: remove
case class TextDesc (escapeChars: EscapeDesc) {
}

private [token]
object TextDesc {
    val plain = TextDesc(
        escapeChars = EscapeDesc(escBegin = '\\',
                                 literals = Set('\'', '\"', '\\'),
                                 singleMap = Map('0' -> '\u0000',
                                                 'a' -> '\u0007',
                                                 'b' -> '\u0008',
                                                 'f' -> '\u000c',
                                                 'n' -> '\u000a',
                                                 'r' -> '\u000d',
                                                 't' -> '\u0009',
                                                 'v' -> '\u000b'),
                                 multiMap = Map("NUL" -> '\u0000',
                                                "SOH" -> '\u0001',
                                                "STX" -> '\u0002',
                                                "ETX" -> '\u0003',
                                                "EOT" -> '\u0004',
                                                "ENQ" -> '\u0005',
                                                "ACK" -> '\u0006',
                                                "BEL" -> '\u0007',
                                                "BS"  -> '\u0008',
                                                "HT"  -> '\u0009',
                                                "LF"  -> '\u000a',
                                                "VT"  -> '\u000b',
                                                "FF"  -> '\u000c',
                                                "CR"  -> '\u000d',
                                                "SO"  -> '\u000e',
                                                "SI"  -> '\u000f',
                                                "DLE" -> '\u0010',
                                                "DC1" -> '\u0011',
                                                "DC2" -> '\u0012',
                                                "DC3" -> '\u0013',
                                                "DC4" -> '\u0014',
                                                "NAK" -> '\u0015',
                                                "SYN" -> '\u0016',
                                                "ETB" -> '\u0017',
                                                "CAN" -> '\u0018',
                                                "EM"  -> '\u0019',
                                                "SUB" -> '\u001a',
                                                "ESC" -> '\u001b',
                                                "FS"  -> '\u001c',
                                                "GS"  -> '\u001d',
                                                "RS"  -> '\u001e',
                                                "US"  -> '\u001f',
                                                "SP"  -> '\u0020',
                                                "DEL" -> '\u007f'),
                                 decimalEscape = NumericEscape.Supported(prefix = None, maxValue = 0x10ffff),
                                 hexadecimalEscape = NumericEscape.Supported(prefix = Some('x'), maxValue = 0x10ffff),
                                 octalEscape = NumericEscape.Supported(prefix = Some('o'), maxValue = 0x10ffff),
                                 binaryEscape = NumericEscape.Illegal,
                                 emptyEscape = Some('&'),
                                 gapsSupported = true,
                                 ctrlEscape = CtrlEscape.Supported(prefix = Some('^'),
                                                                   mapping = ('@' to '_').map(c => c -> (c - '@').toChar).toMap)))
}
