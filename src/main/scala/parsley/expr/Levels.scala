package parsley.expr

import parsley.XCompat._

/**
 * For more complex expression parser types `Levels` can be used to
 * describe the precedence table whilst preserving the intermediate
 * structure between each level.
 * @tparam A The base type accepted by this list of levels
 * @tparam B The type of structure produced by the list of levels
 * @since 2.2.0
 */
sealed trait Levels[-A, +B]
/**
 * This represents a single new level of the hierarchy, with stronger
 * precedence than its tail.
 * @tparam A The base type accepted by this layer
 * @tparam B The intermediate type that will be provided to the next layer
 * @tparam C The type of structure produced by the next layers
 * @param ops The operators accepted at this level
 * @param lvls The next, weaker, levels in the precedence table
 * @return A larger precedence table transforming atoms of type `A` into
 *          a structure of type `C`.
 * @since 2.2.0
 */
final case class Level[-A, B, +C](ops: Ops[A, B], lvls: Levels[B, C]) extends Levels[A, C]
private [expr] final case class NoLevel[A, B](ev: A =:= B) extends Levels[A, B]
object Levels {
    /**
     * This represents the end of a precedence table. It will not
     * touch the structure in any way.
     * @tparam A The type of the structure to be produced by the table.
     * @since 2.2.0
     */
    def empty[A]: Levels[A, A] = NoLevel(refl[A])

    implicit class LevelBuilder[B, +C](lvls: Levels[B, C]) {
        def +:[A](lvl: Ops[A, B]): Levels[A, C] = Level(lvl, lvls)
    }
}