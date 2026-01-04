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
import parsley.template.ParserBridge1
import parsley.template.ParserBridge2

object ExprGen {
    private type UnaryOp = (String, TestExpr => TestExpr)
    private type BinaryOp = (String, (TestExpr, TestExpr) => TestExpr)

    case class OpsDef(fixity: Fixity, ops: List[(String, Any)])

    private val fixityGen: Gen[Fixity] = Gen.oneOf(InfixL, InfixN, InfixR, Prefix, Postfix)

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

    private def opsDefGen[Op](fixity: Fixity, availableOps: Set[(String, Op)]): Gen[(OpsDef, Set[(String, Op)])] =
        for (ops <- Gen.atLeastOne(availableOps)) yield (OpsDef(fixity, ops.toList), availableOps -- ops)

    val exprPairGen: Gen[(Parsley[TestExpr], Parsley[TestExpr], List[OpsDef])] = {
        // NOTE: do not use a mutable buffer inside a generator, it will be reused.
        def loop(infixPool: Set[BinaryOp], prefixPool: Set[UnaryOp], postfixPool: Set[UnaryOp], acc: List[OpsDef]): Gen[List[OpsDef]] = {
            def continue: Gen[List[OpsDef]] =
                if (infixPool.isEmpty && prefixPool.isEmpty && postfixPool.isEmpty) Gen.const(acc.reverse)
                else fixityGen.flatMap {
                    case fixity@(InfixL | InfixN | InfixR) if infixPool.nonEmpty => opsDefGen(fixity, infixPool).flatMap { case (opsDef, remaining) =>
                        loop(remaining, prefixPool, Set.empty, opsDef :: acc)
                    }
                    case Prefix if prefixPool.nonEmpty => opsDefGen(Prefix, prefixPool).flatMap { case (opsDef, remaining) =>
                        loop(infixPool, remaining, postfixPool, opsDef :: acc)
                    }
                    case Postfix if postfixPool.nonEmpty => opsDefGen(Postfix, postfixPool).flatMap { case (opsDef, remaining) =>
                        loop(infixPool, prefixPool, remaining, opsDef :: acc)
                    }
                    case _ => loop(infixPool, prefixPool, postfixPool, acc)
                }

            if (acc.nonEmpty) Gen.frequency(2 -> Gen.const(acc.reverse), 3 -> continue) else continue
        }

        for (opsDefs <- loop(infixOps, prefixOps, postfixOps, Nil)) yield {
            val int = character.digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

            lazy val originalAtoms: OriginalPrec[TestExpr] = OriginalAtoms[TestExpr](Num(int), '(' ~> originalExpr <~ ')')
            lazy val originalExpr = originalPrecedence[TestExpr](
                opsDefs.foldLeft(originalAtoms) {
                    case (acc, OpsDef(fixity, ops)) => {
                        val opsWithFixity = ops.map { case (s, f) => atomic(s) as f.asInstanceOf[fixity.Op[TestExpr, TestExpr]] }
                        val originalOps = OriginalOps(fixity)(opsWithFixity(0), opsWithFixity.tail*)
                        acc :+ originalOps
                    }
                }
            )

            lazy val newAtoms: Prec[TestExpr] = Atoms[TestExpr](Num(int), '(' ~> newExpr <~ ')')
            lazy val newExpr = precedence[TestExpr](
                opsDefs.foldLeft(newAtoms) {
                    case (acc, OpsDef(fixity, ops)) => {
                        val opsWithFixity = ops.map { case (s, f) => atomic(s) as f.asInstanceOf[fixity.Op[TestExpr, TestExpr]] }
                        val newOps = Ops(fixity)(opsWithFixity(0), opsWithFixity.tail*)
                        acc :+ newOps
                    }
                }
            )

            (originalExpr, newExpr, opsDefs)
        }
    }

    def inputsGen(
        opsDefs: List[OpsDef],
        mutationRate: Double = 0.2,
        invalidCharacters: Set[String] = Set("@", "#", "$", "%", "^", "~", "`", "\\", "|", ",", "<", ">", "?")
    ): Gen[String] = {
        require(opsDefs.nonEmpty, "OpsDefs cannot be empty")
        require(mutationRate >= 0 && mutationRate <= 1, "Failure rate must be between 0 and 1")

        val ops = opsDefs.flatMap(opsDef => opsDef.ops.map(op => (op._1, opsDef.fixity)))

        val genIntString = arbitrary[Int].map(_.toString)

        // FIXME: this should use resize and do it properly, without arbitrary cut off
        def validExprGen(depth: Int): Gen[String] = {
            if (depth > 4) genIntString
            else {
                // Create different types of expressions based on depth
                val operatorExpr = for {
                    (op, fixity) <- Gen.oneOf(ops)
                    expr <- fixity match {
                        case InfixL | InfixN | InfixR => for (left <- validExprGen(depth + 1); right <- validExprGen(depth + 1)) yield s"$left$op$right"
                        case Prefix => for (inner <- validExprGen(depth + 1)) yield s"$op$inner"
                        case Postfix => for (inner <- validExprGen(depth + 1)) yield s"$inner$op"
                    }
                } yield expr
                val bracketedExpr = for (inner <- validExprGen(depth + 1)) yield s"($inner)"

                Gen.frequency(3 -> operatorExpr, 2 -> genIntString, 1 -> bracketedExpr)
            }
        }

        val mutators: List[String => Gen[String]] = List(
            // Remove a sequence of characters
            (s: String) => for ((start, end) <- splitGen(s); len <- Gen.choose(1, 3)) yield s"$start${end.drop(len)}",

            // Add invalid characters
            (s: String) => for ((start, end) <- splitGen(s); chars <- Gen.nonEmptyListOf(Gen.oneOf(invalidCharacters))) yield s"$start${chars.mkString}$end",

            // Replace characters with invalid ones
            (s: String) => if (s.isEmpty) Gen.oneOf(invalidCharacters) else for {
                idx <- Gen.choose(0, s.length - 1)
                (start, end) = s.splitAt(idx)
                replacementChar <- Gen.oneOf(invalidCharacters)
            } yield s"$start$replacementChar${end.tail}",

            // Add unbalanced parentheses
            (s: String) => for ((start, end) <- splitGen(s); paren <- Gen.oneOf("(", ")")) yield s"$start$paren$end",
        )

        for {
            expr <- validExprGen(0)
            shouldMutate <- Gen.prob(mutationRate)
            finalExpr <- if (shouldMutate) mutateExpr(expr, mutators) else Gen.const(expr)
        } yield finalExpr
    }

    def mutateExpr(expr: String, mutators: List[String => Gen[String]]): Gen[String] = for {
        numMutations <- Gen.choose(1, 4)
        result <- mutate(expr, numMutations, mutators)
    } yield result
    def mutate(s: String, mutators: List[String => Gen[String]]): Gen[String] = Gen.oneOf(mutators).flatMap(_.apply(s))
    def mutate(s: String, count: Int, mutators: List[String => Gen[String]]): Gen[String] = {
        if (count <= 0) Gen.const(s)
        else for {
            mutated <- mutate(s, mutators)
            result <- mutate(mutated, count - 1, mutators)
        } yield result
    }

    def splitGen(s: String): Gen[(String, String)] = for (idx <- Gen.choose(0, s.length)) yield s.splitAt(idx)
}
