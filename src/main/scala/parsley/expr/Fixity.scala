package parsley.expr

import scala.language.higherKinds

/**
  * Denotes the fixity and associativity of an operator. Importantly, it also specifies the type of the
  * of the operations themselves. For non-monolithic structures this is described by `fixity.GOp` and
  * for monolithic/subtyping based structures this is described by `fixity.Op`.
  * @since 2.2.0
  */
sealed trait Fixity {
  type Op[A] = GOp[A, A]
  type GOp[-A, B]
}

/**
  * Describes left-associative binary operators
  * @since 2.2.0
  */
case object InfixL extends Fixity {
  override type GOp[-A, B] = (B, A) => B
}

/**
  * Describes right-associative binary operators
  * @since 2.2.0
  */
case object InfixR extends Fixity {
  override type GOp[-A, B] = (A, B) => B
}

/**
  * Describes unary prefix operators
  * @since 2.2.0
  */
case object Prefix extends Fixity {
  override type GOp[-A, B] = B => B
}

/**
  * Describes unary postfix operators
  * @since 2.2.0
  */
case object Postfix extends Fixity {
  override type GOp[-A, B] = B => B
}