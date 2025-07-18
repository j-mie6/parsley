import _root_.parsley.build.mima

val projectName = "parsley"
val Scala213 = "2.13.16"
val Scala212 = "2.12.18"
val Scala3 = "3.3.1"
val Java8 = JavaSpec.temurin("8")
val JavaLTS = JavaSpec.temurin("11")
val JavaLatest = JavaSpec.temurin("17")

val mainBranch = "master"

Global / onChangedBuildSource := ReloadOnSourceChanges

val releaseFlags = Seq("-Xdisable-assertions", "-opt:l:method,inline", "-opt-inline-from", "parsley.**", "-opt-warnings:at-inline-failed")
val noReleaseFlagsScala3 = true // maybe some day this can be turned off...

inThisBuild(List(
  tlBaseVersion := "4.6",
  organization := "com.github.j-mie6",
  organizationName := "Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>",
  startYear := Some(2020), // true start is 2018, but license is from 2020
  licenses := List("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  versionScheme := Some("early-semver"),
  crossScalaVersions := Seq(Scala213, Scala212, Scala3),
  scalaVersion := Scala213,
  mimaBinaryIssueFilters ++= mima.issueFilters,
  // CI Configuration
  tlCiReleaseBranches := Seq(mainBranch),
  tlCiScalafmtCheck := false,
  tlCiHeaderCheck := true,
  githubWorkflowJavaVersions := Seq(Java8, JavaLTS, JavaLatest),
  // FIXME: codeclimate has been changed
  //githubWorkflowAddedJobs += testCoverageJob(githubWorkflowGeneratedCacheSteps.value.toList),
  //githubWorkflowConcurrency := None,
  // Website Configuration
  tlSitePublishBranch := Some(mainBranch),
))

lazy val root = tlCrossRootProject.aggregate(parsley, parsleyDebug)

// These settings are shared between all projects.
lazy val commonSettings = Seq(
  headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
  headerEmptyLine := false,

  //resolvers ++= Opts.resolver.sonatypeOssReleases, // Will speed up MiMA during fast back-to-back releases
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test,
  ),

  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oI"),

  scalacOptions ++= {
    if (!isSnapshot.value && !(noReleaseFlagsScala3 && scalaBinaryVersion.value == "3")) releaseFlags else Seq.empty
  },
)

lazy val parsley = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("parsley"))
  .settings(
    name := projectName,
    commonSettings,

    tlVersionIntroduced := Map(
      "2.13" -> "1.5.0",
      "2.12" -> "1.5.0",
      "3"    -> "3.1.2",
    ),

    Compile / doc / scalacOptions ++= Seq("-groups", "-doc-root-content", s"${baseDirectory.value.getParentFile.getPath}/rootdoc.md"),
  )
  .jsSettings(
    // JS lacks the IO module, so has its own rootdoc
    Compile / doc / scalacOptions ++= Seq("-doc-root-content", s"${baseDirectory.value.getPath}/rootdoc.md"),
  )
  // 4.6.0 bumped to 0.5, which means the old versions are unfindable
  .nativeSettings(
    tlVersionIntroduced := Map(
      "2.13" -> "4.6.0",
      "2.12" -> "4.6.0",
      "3"    -> "4.6.0",
    ),
  )

lazy val docs = project
  .in(file("site"))
  .dependsOn(parsley.jvm)
  .enablePlugins(ParsleySitePlugin)
  .settings(
    tlSiteApiModule := Some((parsley.jvm / projectID).value),
    libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.13.0",
        "com.github.j-mie6" %% "parsley-cats" % "1.4.0"
    ),
  )

lazy val parsleyDebug = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("parsley-debug"))
  .dependsOn(parsley % "compile->compile;test->test") // Forwards the test classes to this project. Needed for ParsleyTest.
  .settings(
    name := "parsley-debug",
    commonSettings,

    tlVersionIntroduced := Map(
      "2.13" -> "4.5.0",
      "2.12" -> "4.5.0",
      "3"    -> "4.5.0",
    ),
  )
  .jvmSettings(
    libraryDependencies ++= {
      // Reflection library choice per Scala version.
      CrossVersion.partialVersion(Keys.scalaVersion.value) match {
        case Some((2, 12)) =>
          Seq("org.scala-lang" % "scala-reflect" % Scala212)
        case Some((2, 13)) =>
          Seq("org.scala-lang" % "scala-reflect" % Scala213)
        case _             =>
          // No Scala library for any other version (2.11, 3, etc.).
          Seq()
      }
    }
  )
    // 4.6.0 bumped to 0.5, which means the old versions are unfindable
  .nativeSettings(
    tlVersionIntroduced := Map(
      "2.13" -> "4.6.0",
      "2.12" -> "4.6.0",
      "3"    -> "4.6.0",
    ),
  )

def testCoverageJob(cacheSteps: List[WorkflowStep]) = WorkflowJob(
    id = "coverage",
    name = "Run Test Coverage and Upload",
    cond = Some(s"github.ref == 'refs/heads/$mainBranch' || (github.event_name == 'pull_request' && github.base_ref == '$mainBranch')"),
    steps =
        WorkflowStep.Checkout ::
        WorkflowStep.SetupSbt ::
        WorkflowStep.SetupJava(List(JavaLTS)) :::
        cacheSteps ::: List(
            WorkflowStep.Sbt(name = Some("Generate coverage report"), commands = List("coverage", "parsley / test", "parsleyDebug / test", "coverageReport")),
            WorkflowStep.Use(
                name = Some("Upload coverage to Code Climate"),
                ref = UseRef.Public(owner = "paambaati", repo = "codeclimate-action", ref = "v3.2.0"),
                env = Map("CC_TEST_REPORTER_ID" -> "c1f669dece75a1d69bf0dc45a682d64837badc112b8098271ccc0dca1bbc7a09"),
                // FIXME: Surely, there's a better method for multiple report locations than a multiline string (or using \n as a separator).
                params = Map("coverageLocations" ->
                  """${{github.workspace}}/parsley/jvm/target/scala-2.13/coverage-report/cobertura.xml:cobertura
                    |${{github.workspace}}/parsley-debug/jvm/target/scala-2.13/coverage-report/cobertura.xml:cobertura""".stripMargin),
            )
        )
)
