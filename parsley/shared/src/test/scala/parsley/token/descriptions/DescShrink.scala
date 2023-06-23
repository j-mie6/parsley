package parsley.token.descriptions

import org.scalacheck.Shrink

import parsley.token.descriptions.text._
//import org.typelevel.scalaccompat.annotation._

object DescShrink {
    // TODO: shrinking logic for new esc desc stuff
    implicit val escDescShrink: Shrink[EscapeDesc] = Shrink {
        case desc@EscapeDesc(_, literals, singles, multis, _, _, _, _, _, _) =>
            //@nowarn213("cat=deprecation")
            //@nowarn3("cat=deprecation")
            val shrinkLiterals = for (ls <- Shrink.shrink(literals)) yield desc.copy(literals = ls)
            //@nowarn213("cat=deprecation")
            //@nowarn3("cat=deprecation")
            val shrinkSingles = Shrink.shrink(singles).collect {
                case ss if ss.forall(kv => Character.isValidCodePoint(kv._2)) => desc.copy(singleMap = ss)
            }
            //@nowarn213("cat=deprecation")
            //@nowarn3("cat=deprecation")
            val shrinkMultis = Shrink.shrink(multis).collect {
                case ms if ms.forall(kv => Character.isValidCodePoint(kv._2)) &&
                           !singles.keys.exists(c => ms.contains(s"$c")) => desc.copy(multiMap = ms.filter(_._1.nonEmpty))
            }
            shrinkMultis ++ shrinkSingles ++ shrinkLiterals
    }
}
