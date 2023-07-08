import com.typesafe.tools.mima.core._

val projectName = "parsley"
val Scala213 = "2.13.11"
val Scala212 = "2.12.18"
val Scala3 = "3.3.0"
val Java8 = JavaSpec.temurin("8")
val JavaLTS = JavaSpec.temurin("11")
val JavaLatest = JavaSpec.temurin("17")

val mainBranch = "master"

Global / onChangedBuildSource := ReloadOnSourceChanges

val releaseFlags = Seq("-Xdisable-assertions", "-opt:l:method,inline", "-opt-inline-from", "parsley.**", "-opt-warnings:at-inline-failed")
val noReleaseFlagsScala3 = true // maybe some day this can be turned off...

inThisBuild(List(
  tlBaseVersion := "4.4",
  organization := "com.github.j-mie6",
  organizationName := "Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>",
  startYear := Some(2020), // true start is 2018, but license is from 2020
  homepage := Some(url("https://github.com/j-mie6/parsley")),
  licenses := List("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  developers := List(
    tlGitHubDev("j-mie6", "Jamie Willis")
  ),
  versionScheme := Some("early-semver"),
  crossScalaVersions := Seq(Scala213, Scala212, Scala3),
  scalaVersion := Scala213,
  mimaBinaryIssueFilters ++= Seq(
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
  ),
  tlVersionIntroduced := Map(
    "2.13" -> "1.5.0",
    "2.12" -> "1.5.0",
    "3"    -> "3.1.2",
  ),
  // CI Configuration
  tlCiReleaseBranches := Seq(mainBranch, "staging/4.4"),
  tlCiScalafmtCheck := false,
  tlCiHeaderCheck := true,
  githubWorkflowJavaVersions := Seq(Java8, JavaLTS, JavaLatest),
  githubWorkflowAddedJobs += testCoverageJob(githubWorkflowGeneratedCacheSteps.value.toList),
  githubWorkflowConcurrency := None,
))

lazy val root = tlCrossRootProject.aggregate(parsley)

lazy val parsley = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("parsley"))
  .settings(
    name := projectName,

    headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
    headerEmptyLine := false,

    resolvers ++= Opts.resolver.sonatypeOssReleases, // Will speed up MiMA during fast back-to-back releases
    libraryDependencies ++= Seq(
        "org.scalatest" %%% "scalatest" % "3.2.16" % Test,
        "org.scalacheck" %%% "scalacheck" % "1.17.0" % Test,
        "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.15.0" % Test,
    ),

    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oI"),

    scalacOptions ++= {
        if (!isSnapshot.value && !(noReleaseFlagsScala3 && scalaBinaryVersion.value == "3")) releaseFlags else Seq.empty
    },

    Compile / doc / scalacOptions ++= Seq("-groups", "-doc-root-content", s"${baseDirectory.value.getParentFile.getPath}/rootdoc.md"),
  )
  .jsSettings(
    // JS lacks the IO module, so has its own rootdoc
    Compile / doc / scalacOptions ++= Seq("-doc-root-content", s"${baseDirectory.value.getPath}/rootdoc.md"),
  )

def testCoverageJob(cacheSteps: List[WorkflowStep]) = WorkflowJob(
    id = "coverage",
    name = "Run Test Coverage and Upload",
    cond = Some(s"github.ref == 'refs/heads/$mainBranch' || (github.event_name == 'pull_request' && github.base_ref == '$mainBranch')"),
    steps =
        WorkflowStep.Checkout ::
        WorkflowStep.SetupJava(List(JavaLTS)) :::
        cacheSteps ::: List(
            WorkflowStep.Sbt(name = Some("Generate coverage report"), commands = List("coverage", "parsley / test", "coverageReport")),
            WorkflowStep.Use(
                name = Some("Upload coverage to Code Climate"),
                ref = UseRef.Public(owner = "paambaati", repo = "codeclimate-action", ref = "v3.2.0"),
                env = Map("CC_TEST_REPORTER_ID" -> "c1f669dece75a1d69bf0dc45a682d64837badc112b8098271ccc0dca1bbc7a09"),
                params = Map("coverageLocations" -> "${{github.workspace}}/parsley/jvm/target/scala-2.13/coverage-report/cobertura.xml:cobertura"),
            )
        )
)
