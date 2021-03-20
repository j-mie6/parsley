package parsley.expr

import parsley.XCompat._
import parsley.Parsley

/**
  * For more complex expression parser types `Levels` can be used to
  * describe the precedence table whilst preserving the intermediate
  * structure between each level.
  * @tparam A The base type accepted by this list of levels
  * @tparam B The type of structure produced by the list of levels
  * @since 3.0.0
  */
sealed trait Levels[-A, +B] {
    /**
      * Builds a larger precedence table from strongest to weakest
      * @tparam C The new result type for the larger table
      * @param ops The operators that transform the previous, stronger, layer into the new result
      */
    final def :+[C](ops: Ops[B, C]): Levels[A, C] = Level(this, ops)
    /**
      * Builds a larger parser precedence table from weakest to strongest
      * @tparam C The new result type for the larger table
      * @param ops The operators that transform the next, stronger, layer into the new result
      */
    final def +:[C](ops: Ops[B, C]): Levels[A, C] = Level(this, ops)
}
/**
  * This represents a single new level of the hierarchy, with stronger
  * precedence than its tail.
  * @tparam A The base type accepted by the layer below
  * @tparam B The intermediate type produced by the layer below to be fed into this level
  * @tparam C The type of structure produced by this layer
  * @param ops The operators accepted at this level
  * @param lvls The next, stronger, levels in the precedence table
  * @return A larger precedence table transforming atoms of type `A` into
  *          a structure of type `C`.
  * @since 3.0.0
  */
case class Level[-A, B, C](lvls: Levels[A, B], ops: Ops[B, C]) extends Levels[A, C]
private [expr] case class Atoms_[A, B](ev: A =:= B, atoms: Parsley[A]*) extends Levels[A, B]

/**
  * This represents the final level of the hierarchy, with the strongest binding.
  */
object Atoms {
    /**
      * Given some atoms, produces the base of the precedence hierarchy
      * @tparam A The base type of the hierarchy
      * @param atoms The atoms at the bottom of the precedence table
      */
    def apply[A](atoms: Parsley[A]*): Levels[A, A] = new Atoms_(refl[A], atoms: _*)
}