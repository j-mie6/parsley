package parsley.expr

/** Denotes the fixity and associativity of an operator*/
sealed trait Fixity {
  type Op[A] = GOp[A, A]
  type GOp[-A, B]
}
case object InfixL extends Fixity {
  override type GOp[-A, B] = (B, A) => B
}
case object InfixR extends Fixity {
  override type GOp[-A, B] = (A, B) => B
}
case object Prefix extends Fixity {
  override type GOp[-A, B] = B => B
}
case object Postfix extends Fixity {
  override type GOp[-A, B] = B => B
}