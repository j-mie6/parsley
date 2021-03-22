package parsley.errors

trait ErrorBuilder[Err] {
    private [errors] final type _Err = Err

    def format(pos: Position, source: Context, ctxs: NestedContexts, lines: ErrorInfoLines): Err

    type Position
    type Context
    def pos(line: Int, col: Int): Position
    def source(sourceName: Option[String]): Context
    def contexualScope(context: String): Context

    type NestedContexts
    def nestContexts(contexts: List[Context]): NestedContexts

    type ErrorInfoLines
    def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines
    def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines

    type ExpectedItems
    type Messages
    def combineExpectedItems(alts: Set[Item]): ExpectedItems
    def combineMessages(alts: Set[Message]): Messages

    type UnexpectedLine
    type ExpectedLine
    type Message
    type LineInfo
    def unexpected(item: Option[Item]): UnexpectedLine
    def expected(alts: ExpectedItems): ExpectedLine
    def reason(reason: String): Message
    def message(msg: String): Message
    def lineInfo(line: String, errorPointsAt: Int): LineInfo

    type Item
    type Raw <: Item
    type Named <: Item
    type EndOfInput <: Item
    def raw(item: String): Raw
    def named(item: String): Named
    val endOfInput: EndOfInput
}

object ErrorBuilder {
    implicit val stringError: ErrorBuilder[String] = new DefaultErrorBuilder
}
