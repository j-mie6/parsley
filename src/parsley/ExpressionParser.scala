package parsley

import parsley.ExpressionParser._
import parsley.Combinator._

class ExpressionParser[A](table: List[OpList[A]], atom: =>Parsley[A])
{
    private def convertOperator(ops: OpList[A]): Parsley[A] => Parsley[A] = ops match
    {
        case Infixes(ops, assoc) => assoc match
        {
            case AssocLeft => chainl1(_, choice(ops))
            case AssocRight => chainr1(_, choice(ops))
        }
        case Prefixes(ops) => chainPre(_, choice(ops))
        case Postfixes(ops) => chainPost(_, choice(ops))
    }

    lazy val expr = table.map(convertOperator).foldLeft(atom)((p, op) => op(p))
}

object ExpressionParser
{
    sealed trait Assoc
    case object AssocLeft extends Assoc
    case object AssocRight extends Assoc

    sealed trait OpList[A]
    case class Infixes[A](op: List[Parsley[A => A => A]], assoc: Assoc) extends OpList[A]
    case class Prefixes[A](op: List[Parsley[A => A]]) extends OpList[A]
    case class Postfixes[A](op: List[Parsley[A => A]]) extends OpList[A]
}