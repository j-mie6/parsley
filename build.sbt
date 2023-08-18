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
  .dependsOn(parsley.jvm)
  .enablePlugins(TypelevelSitePlugin)
  .settings(
    laikaConfig := {
      import laika.rewrite.link._

      LaikaConfig.defaults
      .withConfigValue(LinkConfig(apiLinks = Seq(
          ApiLinks(baseUri = "https://www.javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/")
      )))
    },
    tlSiteHelium := {
      import laika.ast.Path.Root
      import laika.ast.{InlineSVGIcon, Styles}
      import laika.helium.config.{ColorQuintet, HeliumIcon, IconLink, TextLink, ThemeNavigationSection}
      import laika.theme.config.{Color}

      case class ColorTints(base: Color, light: Color, lighter: Color, dark: Color, darker: Color)

      val CharcoalLightGrey = Color.hex("616365")
      val CharcoalGrey      = Color.hex("2e2f30")
      val CharcoalDarkGrey  = Color.hex("242526")
      val CharcoalBlack     = Color.hex("202025")
      val Mint              = ColorTints(
        base    = Color.hex("96dec4"), //71ceac
        light   = Color.hex("baeedb"), lighter = Color.hex("d4f7ea"), //light a0e5cb
        dark    = Color.hex("4ab28a"), darker  = Color.hex("3d9a71"),
      )
      val MintCream         = Color.hex("fbfefb")
      val OffWhite          = Color.hex("f3f3f3")
      val LightYellow       = Color.hex("ffe562")
      val PaleYellow        = Color.hex("fff7d0") //ffee97
      val DarkPink          = Color.hex("8b2257")
      val ForestGreen       = ColorTints(
        base    = Color.hex("3ca43c"),
        light   = Color.hex("63c263"), lighter = Color.hex("e3f2e3"), // afdcaf, (d7edd7; 94db94) lighter
        dark    = Color.hex("228b22"), darker  = Color.hex("095609"), // darker 0b6c0b
      )

      // Mint Secondaries
      val DarkGrayishBrown = Color.hex("ada39c") // 20% brightening of 998c83
      val DarkGrayishMagenta = Color.hex("9f7e99")

      val AntiMintRed = ColorTints(
        base    = Color.hex("dc798c"),
        light   = Color.hex("f1a9b7"), lighter = Color.hex("f8d0d8"),
        dark    = Color.hex("cc3c58"), darker  = Color.hex("93253a"),
      )
      val AntiForestRed = ColorTints(
        base    = Color.hex("cd4b4b"),
        light   = Color.hex("f37c7c"), lighter = Color.hex("ffcbcb"), //ffadad
        dark    = Color.hex("ae2b2b"), darker  = Color.hex("870d0d"),
      )

      // TODO: Move this to a more sensible home
      val leafSVG =
        """<svg class="svg-icon" width="100%" height="100%" viewBox="0 0 128 128" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" xmlns:serif="http://www.serif.com/" style="fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;">
          |  <g class="svg-shape">
          |    <path d="M101.39,86.21c-14.25-5.91-22.34-4.3-28.47-2.5c-6.5,1.91-12.31,8.58-12.31,8.58s-1.93-0.58-5.04-0.51 c-1.49,0.04-2.67,0.28-3.54,0.54c2.64-5.99,4.51-12.08,7.57-18.76c1.95-0.81,4.63-1.86,6.35-2.23c2.96-0.64,6.45-0.84,6.45-0.84 c0.82,0.64,2,1.39,3.69,2.23c9.56,4.78,21.5,2.16,27.35-0.16c8.13-3.23,20.21-12.95,20.21-13.69s-5.5-3.38-13.32-5.71 c-7.82-2.33-18.27-2.44-26.21,0.95c-7.28,3.11-10.67,8.58-12.54,11.86c0,0-2.9,0.46-5,0.82c-1.56,0.27-2.96,0.64-4.07,0.99 c0.22-0.4,0.45-0.81,0.68-1.21C67.77,58.61,72.83,52.5,77,48.2c5.87,0.55,17.89,0.35,27.62-8.46 c15.64-14.15,18.46-35.92,18.07-36.35S97.35,3.34,84.54,14.91c-12.19,11.01-11.97,25.4-11.4,30.69c-3.97,3.71-8.81,9.17-14.2,17.12 c-0.04-2.12-0.29-4.73-0.29-4.73s7.19-6.73,7.72-17.18c0.42-8.24-0.46-15.89-5-24.14c-3.56-6.43-6.53-8.03-7.06-7.93 c-0.93,0.19-5.33,7.06-8.03,13.63c-2.13,5.17-4.26,18.38-0.45,26.52s8.22,9.68,8.22,9.68s0.54,2.37,0.48,5.55 c-0.02,1.36-0.13,4.51-0.18,5.98c-3.57,6.32-5.96,12.07-8.23,17.22c-0.34-0.99-0.73-1.96-1.11-2.65c-1.16-2.11-1.59-2.85-1.59-2.85 s2.57-15.99-1.8-29.17C36.45,37,27.57,31.08,19.33,27.59C11.09,24.1,6.54,24.11,5.8,24.95c-1.08,1.23,1.37,4.33,1.59,11.31 c0.17,5.71,1.16,25.05,6.87,34.98s13.63,12.37,16.17,13.21c2.54,0.85,8.03,0.95,8.03,0.95s2.33,2.01,3.17,4.86 c0.39,1.31,0.78,2.92,1.09,4.27c-1.95,3.71-4.17,7-7.21,9.86c-5.01,4.71-13.09,7.94-13.09,7.94s-6.47-0.15-6.91,0.15 c-0.44,0.29-1.73,3.53,0.69,7.51c2,3.29,5.05,4.12,6.08,4.26c1.03,0.15,2.94-4.27,3.68-4.86c0.74-0.59,9.43-3.93,15.6-10.74 c2.83-3.13,5.02-6.16,6.81-9.17c1.65-1.01,4.07-2.17,6.72-2.31c2.64-0.15,4.94,1.12,4.94,1.12s2.22,9.16,12.15,13 c12.5,4.83,25.83,0.61,31.64-1.4c5.81-2,19.83-8.42,19.94-10.22C123.86,97.87,115.66,92.13,101.39,86.21z"/>
          |  </g>
          |</svg>""".stripMargin
      val leaf = InlineSVGIcon(leafSVG, Some("Home"), Styles("leaf"))

      tlSiteHelium.value.site.topNavigationBar(
        homeLink = IconLink.internal(Root / "index.md", leaf),
      )
      .site.themeColors(
        primary = ForestGreen.darker,
        secondary = DarkPink,
        primaryMedium = ForestGreen.base,
        primaryLight = ForestGreen.lighter,
        text = CharcoalLightGrey,
        background = MintCream,
        bgGradient = (OffWhite, Color.hex("ffffff")),
      )
      .site.messageColors(
        info = DarkPink,
        infoLight = ForestGreen.lighter,
        warning = DarkPink,
        warningLight = PaleYellow,
        error = DarkPink,
        errorLight = AntiForestRed.lighter,
      )
      .site.syntaxHighlightingColors(
        base = ColorQuintet(
          CharcoalGrey,
          Color.hex("8c878e"),  // comments, xml-cdata, markup-quote
          Color.hex("b2adb4"),  // tag-punctuation
          Mint.light,           // identifier
          Color.hex("e8e8e8"),  // base colour
        ),
        wheel = ColorQuintet(
          Color.hex("7eacbf"),  // substitution, xml-processing-instruction, markup-emphasized, annotation
          Color.hex("dc799d"),  // keyword, escape-sequence, markup-headline
          Color.hex("e7a1bb"),  // attribute-name, markup-link-target, declaration-name
          Color.hex("b582c1"),  // number-literal, string-literal, literal-value, boolean-literal, char-literal, symbol-literal, regex-literal, markup-link-text
          Color.hex("7fb971"),  // type-name, tag-name, xml-dtd-tagname, markup-fence
        )
      )
      .site.darkMode.themeColors(
        primary = Mint.base,
        secondary = DarkGrayishBrown,
        primaryMedium = CharcoalGrey,
        primaryLight = CharcoalBlack,
        text = OffWhite,
        background = CharcoalDarkGrey,
        bgGradient = (CharcoalDarkGrey, CharcoalGrey),
      )
      .site.darkMode.messageColors(
        info = Mint.dark,
        infoLight = CharcoalGrey,
        warning = LightYellow,
        warningLight = CharcoalGrey,
        error = AntiMintRed.dark,
        errorLight = CharcoalGrey,
      )
      /*.site.darkMode.syntaxHighlightingColors(
        base = ColorQuintet(CharcoalGrey, Color.hex("8c878e"), Color.hex("b2adb4"), Color.hex("baeedb"), Color.hex("e8e8e8")),
        wheel = ColorQuintet(Color.hex("7eacbf"), Color.hex("dc799d"), Color.hex("e7a1bb"), Color.hex("b582c1"), Color.hex("7fb971"))
      )*/
      .site.downloadPage(
        title = "Documentation Downloads",
        description = None,
      )
      .site.mainNavigation(appendLinks = Seq(
        ThemeNavigationSection(
          "Related Projects",
          TextLink.external("https://github.com/j-mie6/parsley-cats", "parsley-cats")a
        )
      ))
      .site.pageNavigation(
        sourceBaseURL = Some(s"${scmInfo.value.fold(homepage.value.get.toString)(_.browseUrl.toString)}/blob/master/docs"),
      )
    }
  )

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
