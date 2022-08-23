package parsley.errors

sealed abstract class Token
case class Raw(tok: String) extends Token
case class Named(name: String) extends Token
case object EndOfInput extends Token
