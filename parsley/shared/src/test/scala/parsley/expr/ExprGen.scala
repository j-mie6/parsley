package parsley.expr

import parsley.syntax.character.{stringLift, charLift}
import parsley.character
import org.scalacheck.Gen
import parsley.Parsley

object ExprGen {
    private type UnaryOp = (String, Int => Int)
    private type BinaryOp = (String, (Int, Int) => Int)

    case class OpsDef(fixity: Fixity, ops: List[(String, Any)])

    private val fixityGen: Gen[Fixity] = Gen.oneOf(
        InfixL,
        InfixN,
        InfixR,
        Prefix,
        Postfix
    )

    private val infixOps: Set[BinaryOp] = Set(
        ("+", (a: Int, b: Int) => a + b),
        ("-", (a: Int, b: Int) => a - b),
        ("*", (a: Int, b: Int) => a * b),
        ("/", (a: Int, b: Int) => if (b == 0) 1 else a / b),
        ("==", (a: Int, b: Int) => if (a == b) 1 else 0)
    )

    private val prefixOps: Set[UnaryOp] = Set(
        ("+", (a: Int) => a + 1),
        ("-", (a: Int) => -a)
    )

    private val postfixOps: Set[UnaryOp] = Set(
        ("!", (a: Int) => (1 to a).product),
        ("++", (a: Int) => a + 1) // This will cause ambiguity with infix +
    )

    private def prefixOpsDefGen(availableOps: Set[UnaryOp]): Gen[(OpsDef, Set[UnaryOp])] = for {
        ops <- Gen.pick(1, availableOps)
    } yield {
        (OpsDef(Prefix, ops.toList), availableOps -- ops)
    }

    private def postfixOpsDefGen(availableOps: Set[UnaryOp]): Gen[(OpsDef, Set[UnaryOp])] = for {
        ops <- Gen.pick(1, availableOps)
    } yield {
        (OpsDef(Postfix, ops.toList), availableOps -- ops)
    }

    private def infixOpsDefGen(fixity: Fixity, availableOps: Set[BinaryOp]): Gen[(OpsDef, Set[BinaryOp])] = for {
        ops <- Gen.pick(1, availableOps)
    } yield {
        (OpsDef(fixity, ops.toList), availableOps -- ops)
    }

    val exprPairGen: Gen[(Parsley[Int], Parsley[Int], List[OpsDef])] = {
        def loop(infixPool: Set[BinaryOp], prefixPool: Set[UnaryOp], postfixPool: Set[UnaryOp], acc: List[OpsDef]): Gen[List[OpsDef]] = {
            if (infixPool.isEmpty && prefixPool.isEmpty && postfixPool.isEmpty) {
                Gen.const(acc)
            } else {
                fixityGen.flatMap {
                    case fixity@(InfixL | InfixN | InfixR) if infixPool.nonEmpty =>
                        infixOpsDefGen(fixity, infixPool).flatMap {
                            case (opsDef, remaining) => loop(remaining, prefixPool, postfixPool, opsDef :: acc)
                        }
                    case Prefix if prefixPool.nonEmpty =>
                        prefixOpsDefGen(prefixPool).flatMap {
                            case (opsDef, remaining) => loop(infixPool, remaining, postfixPool, opsDef :: acc)
                        }
                    case Postfix if postfixPool.nonEmpty =>
                        postfixOpsDefGen(postfixPool).flatMap {
                            case (opsDef, remaining) => loop(infixPool, prefixPool, remaining, opsDef :: acc)
                        }
                    case _ => loop(infixPool, prefixPool, postfixPool, acc)
                }
            }
        }

        for {
            opsDefs <- loop(infixOps, prefixOps, postfixOps, Nil)
        } yield {
            val int = character.digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
            lazy val originalAtoms: OriginalPrec[Int] = OriginalAtoms[Int](int, '(' ~> originalExpr <~ ')')

            lazy val originalExpr = originalPrecedence[Int](
                opsDefs.foldLeft(originalAtoms) {
                    case (acc, OpsDef(fixity, ops)) => {
                        val opsWithFixity = ops.map { case (s, f) => s as f.asInstanceOf[fixity.Op[Int, Int]] }
                        val originalOps = OriginalOps(fixity)(opsWithFixity(0), opsWithFixity.tail: _*)
                        acc :+ originalOps
                    }
                }
            )

            lazy val newAtoms: Prec[Int] = Atoms[Int](int, '(' ~> newExpr <~ ')')

            lazy val newExpr = precedence[Int](
                opsDefs.foldLeft(newAtoms) {
                    case (acc, OpsDef(fixity, ops)) => {
                        val opsWithFixity = ops.map { case (s, f) => s as f.asInstanceOf[fixity.Op[Int, Int]] }
                        val newOps = Ops(fixity)(opsWithFixity(0), opsWithFixity.tail: _*)
                        acc :+ newOps
                    }
                }
            )

            (originalExpr, newExpr, opsDefs)
        }
    }

    def successInputsGen(opsDefs: List[OpsDef]): Gen[List[String]] = {
        // currently ignores InfixN possible error case
        // PRE: opsDefs is not empty
        if (opsDefs.isEmpty) Gen.fail
        
        val ops = opsDefs.flatMap(opsDef => opsDef.ops.map(op => (op._1, opsDef.fixity)))
        val digits = (0 to 99).map(_.toString)

        def loop(depth: Int): Gen[String] = {
            if (depth > 4) Gen.oneOf(digits)
            else {
                val exprGen = for {
                    op <- Gen.oneOf(ops)
                    expr <- op._2 match {
                        case InfixL | InfixN | InfixR =>
                            for {
                                left <- loop(depth + 1)
                                right <- loop(depth + 1)
                            } yield s"$left${op._1}$right"
                        case Prefix =>
                            for {
                                inner <- loop(depth + 1)
                            } yield s"${op._1}$inner"
                        case Postfix =>
                            for {
                                inner <- loop(depth + 1)
                            } yield s"$inner${op._1}"
                    }
                } yield expr
                // val bracketGen = for {
                //     inner <- loop(depth + 1)
                // } yield s"($inner)"
                return Gen.oneOf(exprGen, Gen.oneOf(digits))
            }
        }

        // Generate the expression by either choosing an operator (and recursively generating the inputs) or a digit / bracketed sub expression
        Gen.listOfN(100, loop(0))
    }
}