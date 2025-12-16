/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.expr

import parsley.syntax.character.{stringLift, charLift}
import parsley.character
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import parsley.Parsley
import parsley.Parsley.atomic
import parsley.generic.ParserBridge1
import parsley.generic.ParserBridge2

object ExprGen {
    private type UnaryOp = (String, TestExpr => TestExpr)
    private type BinaryOp = (String, (TestExpr, TestExpr) => TestExpr)

    case class OpsDef(fixity: Fixity, ops: List[(String, Any)])

    private val fixityGen: Gen[Fixity] = Gen.oneOf(
        InfixL,
        InfixN,
        InfixR,
        Prefix,
        Postfix
    )

    sealed trait TestExpr

    case class Num(value: Int) extends TestExpr

    case class InfixPlus(x: TestExpr, y: TestExpr) extends TestExpr
    case class InfixMinus(x: TestExpr, y: TestExpr) extends TestExpr
    case class InfixMult(x: TestExpr, y: TestExpr) extends TestExpr
    case class InfixDiv(x: TestExpr, y: TestExpr) extends TestExpr
    case class InfixEq(x: TestExpr, y: TestExpr) extends TestExpr

    case class PrefixPlus(x: TestExpr) extends TestExpr
    case class PrefixMinus(x: TestExpr) extends TestExpr

    case class PostfixFactorial(x: TestExpr) extends TestExpr
    case class PostfixIncrement(x: TestExpr) extends TestExpr

    object Num extends ParserBridge1[Int, TestExpr]

    object InfixPlus extends ParserBridge2[TestExpr, TestExpr, TestExpr]
    object InfixMinus extends ParserBridge2[TestExpr, TestExpr, TestExpr]
    object InfixMult extends ParserBridge2[TestExpr, TestExpr, TestExpr]
    object InfixDiv extends ParserBridge2[TestExpr, TestExpr, TestExpr]
    object InfixEq extends ParserBridge2[TestExpr, TestExpr, TestExpr]

    object PrefixPlus extends ParserBridge1[TestExpr, TestExpr]
    object PrefixMinus extends ParserBridge1[TestExpr, TestExpr]

    object PostfixFactorial extends ParserBridge1[TestExpr, TestExpr]
    object PostfixIncrement extends ParserBridge1[TestExpr, TestExpr]

    private val infixOps: Set[BinaryOp] = Set(
        ("+", InfixPlus(_, _)),
        ("-", InfixMinus(_, _)),
        ("*", InfixMult(_, _)),
        ("/", InfixDiv(_, _)),
        ("==", InfixEq(_, _))
    )

    private val prefixOps: Set[UnaryOp] = Set(
        ("+", PrefixPlus(_)),
        ("-", PrefixMinus(_))
    )

    private val postfixOps: Set[UnaryOp] = Set(
        ("!", PostfixFactorial(_)),
        ("$", PostfixIncrement(_))
    )

    private def prefixOpsDefGen(availableOps: Set[UnaryOp]): Gen[(OpsDef, Set[UnaryOp])] = for {
        numOps <- Gen.choose(1, availableOps.size)
        ops <- Gen.pick(numOps, availableOps)
    } yield {
        (OpsDef(Prefix, ops.toList), availableOps -- ops)
    }

    private def postfixOpsDefGen(availableOps: Set[UnaryOp]): Gen[(OpsDef, Set[UnaryOp])] = for {
        numOps <- Gen.choose(1, availableOps.size)
        ops <- Gen.pick(numOps, availableOps)
    } yield {
        (OpsDef(Postfix, ops.toList), availableOps -- ops)
    }

    private def infixOpsDefGen(fixity: Fixity, availableOps: Set[BinaryOp]): Gen[(OpsDef, Set[BinaryOp])] = for {
        numOps <- Gen.choose(1, availableOps.size)
        ops <- Gen.pick(numOps, availableOps)
    } yield {
        (OpsDef(fixity, ops.toList), availableOps -- ops)
    }

    val exprPairGen: Gen[(Parsley[TestExpr], Parsley[TestExpr], List[OpsDef])] = {
        def loop(infixPool: Set[BinaryOp], prefixPool: Set[UnaryOp], postfixPool: Set[UnaryOp], acc: List[OpsDef]): Gen[List[OpsDef]] = {
            def continue(): Gen[List[OpsDef]] = if (infixPool.isEmpty && prefixPool.isEmpty && postfixPool.isEmpty) {
                Gen.const(acc)
            } else {
                fixityGen.flatMap {
                    case fixity@(InfixL | InfixN | InfixR) if infixPool.nonEmpty =>
                        infixOpsDefGen(fixity, infixPool).flatMap {
                            case (opsDef, remaining) => loop(remaining, prefixPool, postfixPool.empty, acc :+ opsDef)
                        }
                    case Prefix if prefixPool.nonEmpty =>
                        prefixOpsDefGen(prefixPool).flatMap {
                            case (opsDef, remaining) => loop(infixPool, remaining, postfixPool, acc :+ opsDef)
                        }
                    case Postfix if postfixPool.nonEmpty =>
                        postfixOpsDefGen(postfixPool).flatMap {
                            case (opsDef, remaining) => loop(infixPool, prefixPool, remaining, acc :+ opsDef)
                        }
                    case _ => loop(infixPool, prefixPool, postfixPool, acc)
                }
            }
            
            if (acc.nonEmpty) {
                Gen.frequency(
                    2 -> Gen.const(acc),
                    3 -> continue()
                )
            } else {
                continue()
            }
        }

        for {
            opsDefs <- loop(infixOps, prefixOps, postfixOps, Nil)
        } yield {
            val int = character.digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

            lazy val originalAtoms: OriginalPrec[TestExpr] = OriginalAtoms[TestExpr](Num(int), '(' ~> originalExpr <~ ')')
            lazy val originalExpr = originalPrecedence[TestExpr](
                opsDefs.foldLeft(originalAtoms) {
                    case (acc, OpsDef(fixity, ops)) => {
                        val opsWithFixity = ops.map { case (s, f) => atomic(s) as f.asInstanceOf[fixity.Op[TestExpr, TestExpr]] }
                        val originalOps = OriginalOps(fixity)(opsWithFixity(0), opsWithFixity.tail: _*)
                        acc :+ originalOps
                    }
                }
            )

            lazy val newAtoms: Prec[TestExpr] = Atoms[TestExpr](Num(int), '(' ~> newExpr <~ ')')
            lazy val newExpr = precedence[TestExpr](
                opsDefs.foldLeft(newAtoms) {
                    case (acc, OpsDef(fixity, ops)) => {
                        val opsWithFixity = ops.map { case (s, f) => atomic(s) as f.asInstanceOf[fixity.Op[TestExpr, TestExpr]] }
                        val newOps = Ops(fixity)(opsWithFixity(0), opsWithFixity.tail: _*)
                        acc :+ newOps
                    }
                }
            )

            (originalExpr, newExpr, opsDefs)
        }
    }

    def inputsGen(
        opsDefs: List[OpsDef],
        numInputs: Int,
        failureRate: Int = 20,
        invalidCharacters: List[String] = List("@", "#", "$", "%", "^", "~", "`", "\\", "|", ",", "<", ">", "?")
    ): Gen[List[String]] = {
        require(opsDefs.nonEmpty, "OpsDefs cannot be empty")
        require(numInputs > 0, "Number of inputs must be positive")
        require(failureRate >= 0 && failureRate <= 100, "Failure rate must be between 0 and 100")
        
        val ops = opsDefs.flatMap(opsDef => opsDef.ops.map(op => (op._1, opsDef.fixity)))

        val genIntString = arbitrary[Int].map(_.toString)

        def validExprGen(depth: Int): Gen[String] = {
            if (depth > 4) {
                genIntString
            } else {
                // Create different types of expressions based on depth
                val operatorExpr = for {
                    op <- Gen.oneOf(ops)
                    expr <- op._2 match {
                        case InfixL | InfixN | InfixR =>
                            for {
                                left <- validExprGen(depth + 1)
                                right <- validExprGen(depth + 1)
                            } yield s"$left${op._1}$right"
                        case Prefix =>
                            for {
                                inner <- validExprGen(depth + 1)
                            } yield s"${op._1}$inner"
                        case Postfix =>
                            for {
                                inner <- validExprGen(depth + 1)
                            } yield s"$inner${op._1}"
                    }
                } yield expr
                val bracketedExpr = for {
                    inner <- validExprGen(depth + 1)
                } yield s"($inner)"
              
                Gen.frequency(
                    (3, operatorExpr),
                    (2, genIntString),
                    (1, bracketedExpr)
                )
            }
        }

        val mutations: List[String => Gen[String]] = List(
            // Remove a sequence of characters
            (s: String) => if (s.isEmpty) Gen.const("") else for {
                startIdx <- Gen.choose(0, s.length - 1)
                len <- Gen.choose(1, Math.min(3, s.length - startIdx))
            } yield s.substring(0, startIdx) + s.substring(startIdx + len),

            // Add invalid characters
            (s: String) => for {
                idx <- Gen.choose(0, s.length)
                numChars <- Gen.choose(1, 3)
                chars <- Gen.listOfN(numChars, Gen.oneOf(invalidCharacters))
            } yield s.substring(0, idx) + chars.mkString + s.substring(idx),

            // Replace characters with invalid ones
            (s: String) => if (s.isEmpty()) Gen.oneOf(invalidCharacters) else for {
                idx <- Gen.choose(0, s.length - 1)
                replacementChar <- Gen.oneOf(invalidCharacters)
            } yield s.substring(0, idx) + replacementChar + s.substring(idx + 1),

            // Add unbalanced parentheses
            (s: String) => for {
                idx <- Gen.choose(0, s.length)
                paren <- Gen.oneOf("(", ")")
            } yield s.substring(0, idx) + paren + s.substring(idx)
        )

        def applyRandomMutation(s: String): Gen[String] = Gen.oneOf(mutations).flatMap(mutation => mutation(s))

        def applyMutations(s: String, count: Int): Gen[String] = {
            if (count <= 0) Gen.const(s)
            else for {
                mutated <- applyRandomMutation(s)
                result <- applyMutations(mutated, count - 1)
            } yield result
        }

        def corruptExpression(expr: String): Gen[String] = for {
            numMutations <- Gen.frequency(
                (50, Gen.const(1)),
                (30, Gen.const(2)),
                (15, Gen.const(3)),
                (5, Gen.const(4))
            )
            result <- applyMutations(expr, numMutations)
        } yield result

        for {
            validExprs <- Gen.listOfN(numInputs, validExprGen(0))

            result <- Gen.sequence(validExprs.map { expr =>
                for {
                    shouldCorrupt <- Gen.frequency(
                        (100 - failureRate, Gen.const(false)),
                        (failureRate, Gen.const(true))
                    )
                    finalExpr <- if (shouldCorrupt) corruptExpression(expr) else Gen.const(expr)
                } yield finalExpr
            })
        } yield result.toArray.toList.asInstanceOf[List[String]]
    }
}