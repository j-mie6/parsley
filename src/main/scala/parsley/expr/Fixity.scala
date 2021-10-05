package parsley.expr

import scala.language.higherKinds

/**
  * Denotes the fixity and associativity of an operator. Importantly, it also specifies the type of the
  * of the operations themselves.
  * @since 4.0.0
  */
sealed trait Fixity {
    type Op[A, B]
}

/**
  * Describes left-associative binary operators
  * @since 4.0.0
  */
case object InfixL extends Fixity {
    override type Op[-A, B] = (B, A) => B
}

/**
  * Describes right-associative binary operators
  * @since 4.0.0
  */
case object InfixR extends Fixity {
    override type Op[-A, B] = (A, B) => B
}

/**
  * Describes unary prefix operators
  * @since 4.0.0
  */
case object Prefix extends Fixity {
    override type Op[A, B] = B => B
}

/**
  * Describes unary postfix operators
  * @since 4.0.0
  */
case object Postfix extends Fixity {
    override type Op[A, B] = B => B
}

/**
  * Describes non-associative operators
  * @since 4.0.0
  */
case object InfixN extends Fixity {
    override type Op[-A, +B] = (A, A) => B
}