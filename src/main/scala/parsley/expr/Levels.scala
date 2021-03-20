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
sealed trait Levels[-A, +B]
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
private [expr] case class Atoms[A, B](ev: A =:= B, atoms: Parsley[A]*) extends Levels[A, B]
object Atoms {
    def apply[A](atoms: Parsley[A]*): Levels[A, A] = new Atoms(refl[A], atoms: _*)
}
object Levels {
    implicit class LevelBuilder[-A, +B](lvls: Levels[A, B]) {
        def :+[C](lvl: Ops[B, C]): Levels[A, C] = Level(lvls, lvl)
        def +:[C](lvl: Ops[B, C]): Levels[A, C] = Level(lvls, lvl)
    }
}