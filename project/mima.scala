package parsley.build

import com.typesafe.tools.mima.core._

object mima {
    val issueFilters = Seq(
        ProblemFilters.exclude[Problem]("parsley.internal.*"),
        ProblemFilters.exclude[Problem]("parsley.X*"),
        // Until 5.0 (these are all misreported package private members)
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.numeric.Combined.this"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.token.text.RawCharacter$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.symbol.Symbol.this"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("parsley.token.numeric.Integer.bounded"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.token.numeric.Generic$"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.token.predicate$_CharSet$"),
        ProblemFilters.exclude[MissingFieldProblem]("parsley.token.predicate._CharSet"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.token.errors.ErrorConfig$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.errors.combinator#ErrorMethods.unexpected"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.token.errors.FilterOps"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.token.errors.FilterOps$"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("parsley.token.predicate#CharPredicate.asInternalPredicate"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("parsley.token.errors.FilterConfig.mkError"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("parsley.token.errors.FilterConfig.injectSnd"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.errors.*.asExpectItem"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.errors.*.asExpectDesc"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.errors.*.label"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("parsley.token.errors.*.this"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("parsley.errors.helpers#WhitespaceOrUnprintable.unapply"),
        // Expression refactor
        ProblemFilters.exclude[ReversedMissingMethodProblem]("parsley.expr.Fixity.chain"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("parsley.expr.Ops.chain"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.expr.Lefts*"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.expr.Rights*"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.expr.NonAssocs*"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.expr.Prefixes*"),
        ProblemFilters.exclude[MissingClassProblem]("parsley.expr.Postfixes*"),
        // sbt-typelevel 0.5 upgrade
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.errors.Label.asExpectItems"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("parsley.token.errors.LabelAndReason.asExpectItems"),
        // accidental exposure
        ProblemFilters.exclude[MissingClassProblem]("parsley.internal.machine.errors.DefuncError$"),
    )
}
