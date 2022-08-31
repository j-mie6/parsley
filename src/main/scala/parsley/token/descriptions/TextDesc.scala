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
                       mapped: Map[Char, Char],
                       decimalEscape: NumericEscape,
                       hexadecimalEscape: NumericEscape,
                       octalEscape: NumericEscape,
                       binaryEscape: NumericEscape,
                       emptyEscape: Option[Char],
                       gapsSupported: Boolean,
                       ctrlEscape: CtrlEscape,
                      )

private [token] // TODO: remove
case class TextDesc (escapeChars: EscapeDesc) {
}

private [token]
object TextDesc {
    val plain = TextDesc(
        escapeChars = EscapeDesc(escBegin = '\\',
                                 literals = Set(/*TODO:*/),
                                 mapped = Map(/*TODO:*/),
                                 decimalEscape = NumericEscape.Supported(prefix = None, maxValue = 0x10ffff),
                                 hexadecimalEscape = NumericEscape.Supported(prefix = Some('x'), maxValue = 0x10ffff),
                                 octalEscape = NumericEscape.Supported(prefix = Some('o'), maxValue = 0x10ffff),
                                 binaryEscape = NumericEscape.Illegal,
                                 emptyEscape = Some('&'),
                                 gapsSupported = true,
                                 ctrlEscape = CtrlEscape.Supported(prefix = None, mapping = Map(/*TODO:*/))))
}
