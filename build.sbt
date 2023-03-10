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
  tlBaseVersion := "4.3",
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
  tlCiReleaseBranches := Seq(mainBranch),
  tlCiScalafmtCheck := false,
  tlCiHeaderCheck := true,
  githubWorkflowJavaVersions := Seq(Java8, JavaLTS, JavaLatest),
  githubWorkflowAddedJobs += testCoverageJob(githubWorkflowGeneratedCacheSteps.value.toList),
  // Website Configuration
  tlSitePublishBranch := Some("wiki-migration"),
  tlSiteApiUrl := Some(url("https://www.javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/")),
))

lazy val root = tlCrossRootProject.aggregate(parsley)

lazy val docs = project
  .in(file("site"))
  .settings(
    laikaConfig := {
      import laika.rewrite.link._

      LaikaConfig.defaults
      .withConfigValue(LinkConfig(apiLinks = Seq(
          ApiLinks(baseUri = "https://www.javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/")
      )))
      .withConfigValue("showRelatedProjects", tlSiteRelatedProjects.value.nonEmpty)
    },
    laikaTheme := {
      import laika.ast.Path.Root
      import laika.helium.config.{HeliumIcon, IconLink, ColorQuintet}
      import laika.theme.config.{Color}

      case class ColorTints(base: Color, light: Color, lighter: Color, dark: Color, darker: Color)

      val CharcoalLightGrey = Color.hex("2e2f30")
      // something inbetween?
      val CharcoalGrey      = Color.hex("242526")
      val CharcoalDarkGrey  = Color.hex("202025")
      val CharcoalBlack     = Color.hex("17171a")
      val Mint              = ColorTints(
        base    = Color.hex("96dec4"), //71ceac
        light   = Color.hex("baeedb"), lighter = Color.hex("d4f7ea"), //light a0e5cb
        dark    = Color.hex("4ab28a"), darker  = Color.hex("3d9a71"),
      )
      val MintCream         = Color.hex("f5fffa")
      val HeliumGold        = Color.hex("d4c478")
      val HeliumOrange      = Color.hex("f1c47b")
      val OffWhite          = Color.hex("f3f3f3")
      val DarkRed           = Color.hex("980d0d")

      // Mint Colour Wheel
      // Adjacents
      val AdjBlue = ColorTints(
        base    = Color.hex("75a2ca"),
        light   = Color.hex("a3c5e3"), lighter = Color.hex("d5e7f6"),
        dark    = Color.hex("4e7fab"), darker  = Color.hex("336693"),
      )
      val AdjLime = ColorTints(
        base    = Color.hex("afed82"),
        light   = Color.hex("e7fcd8"), lighter = Color.hex("caf6ab"),
        dark    = Color.hex("95e35e"), darker  = Color.hex("76c93b"),
      )
      // Complementary
      val AntiMintSalmon = ColorTints(
        base    = Color.hex("ffb08c"),
        light   = Color.hex("ffcab2"), lighter = Color.hex("ffe6da"),
        dark    = Color.hex("ff986a"), darker  = Color.hex("e47543"),
      )
      val AntiMintPink = ColorTints(
        base    = Color.hex("ce7194"),
        light   = Color.hex("e5a0ba"), lighter = Color.hex("f2cbd9"),
        dark    = Color.hex("ab4369"), darker  = Color.hex("8a2248"),
      )
      val AntiMintRed = ColorTints(
        base    = Color.hex("dc798c"),
        light   = Color.hex("f1a9b7"), lighter = Color.hex("f8d0d8"),
        dark    = Color.hex("cc3c58"), darker  = Color.hex("93253a"),
      )
      val AntiMintPeach = ColorTints(
        base    = Color.hex("ffcf8c"),
        light   = Color.hex("ffdfb2"), lighter = Color.hex("fff0da"),
        dark    = Color.hex("ffc16a"), darker  = Color.hex("d18f31"),
      )
      val AntiMintPurple = ColorTints(
        base    = Color.hex("ab71ce"),
        light   = Color.hex("cb9ee5"), lighter = Color.hex("e9d1f7"),
        dark    = Color.hex("8c4cb2"), darker  = Color.hex("74309b"),
      )

      tlSiteHeliumConfig.value
        .site.topNavigationBar(
          homeLink = IconLink.internal(Root / "index.md", HeliumIcon.home),
          navLinks = tlSiteApiUrl.value.toList.map { url =>
            IconLink.external(url.toString, HeliumIcon.api)
          } ++ Seq(
            IconLink.external(scmInfo.value.fold("https://github.com/j-mie6/parsley")(_.browseUrl.toString), HeliumIcon.github),
            // IconLink.internal(Root / "downloads.gen", HeliumIcon.download), // TODO: why is this not vertically aligned?
          ),
        )
        .site.darkMode.themeColors(
          primary = Mint.base,
          secondary = AntiMintPeach.base,
          primaryMedium = CharcoalLightGrey, // was Mint.lighter
          primaryLight = CharcoalDarkGrey,
          text = OffWhite,
          background = CharcoalGrey,
          bgGradient = (CharcoalGrey, CharcoalLightGrey) // 007c99
        )
        .site.darkMode.messageColors(
          info = AdjBlue.light, //Color.hex("ebf6f7")
          infoLight = AdjBlue.dark, //Color.hex("007c99")
          warning = AntiMintPeach.light, //Color.hex("fcfacd")
          warningLight = AntiMintPeach.darker, //Color.hex("b1a400")
          error = AntiMintRed.light, //Color.hex("ffe9e3")
          errorLight = AntiMintRed.dark, //DarkRed
        )
        .site.darkMode.syntaxHighlightingColors(
          // TODO: make these the ones from "material" highlighting theme
          base = ColorQuintet(
            CharcoalLightGrey, Color.hex("8c878e"), Color.hex("b2adb4"), Color.hex("bddcee"), Color.hex("e8e8e8")
          ),
          wheel = ColorQuintet(
            Color.hex("e28e93"), Color.hex("ef9725"), Color.hex("ffc66d"), Color.hex("7fb971"), Color.hex("4dbed4")
          )
        )
        .site.downloadPage(
          title = "Documentation Downloads",
          description = None,
        )
        .site.markupEditLinks(
          text = "Source for this page",
          baseURL = s"${scmInfo.value.fold("https://github.com/j-mie6/parsley")(_.browseUrl.toString)}/docs",
        )
        .build.extend(tlSiteHeliumExtensions.value)
    },
    tlSiteRelatedProjects := Seq(
      "parsley-cats" -> url("https://github.com/j-mie6/parsley-cats"),
    ),
  )
  .dependsOn(parsley.jvm)
  .enablePlugins(TypelevelSitePlugin)

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
