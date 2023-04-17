package parsley.debugger

/** A representation of the attempts a parser has made during parse-time.
  *
  * @param rawInput   The input parsed, as raw text.
  * @param fromOffset This offset is where the parse attempt started in the input.
  * @param toOffset   This offset is where the parse attempt finished in the input.
  * @param fromPos    [[fromOffset]] represented as a (line, column) pair.
  * @param toPos      [[toOffset]] represented as a (line, column pair).
  * @param success    Was this parse attempt successful?
  */
case class ParseAttempt
  ( rawInput: String
  , fromOffset: Int
  , toOffset: Int
  , fromPos: (Int, Int)
  , toPos: (Int, Int)
  , success: Boolean
  )
