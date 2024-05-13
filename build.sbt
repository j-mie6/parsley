import _root_.parsley.build.mima

val projectName = "parsley"
val Scala213 = "2.13.12"
val Scala212 = "2.12.18"
val Scala3 = "3.3.3"
val Java8 = JavaSpec.temurin("8")
val JavaLTS = JavaSpec.temurin("11")
val JavaLatest = JavaSpec.temurin("17")

val mainBranch = "staging/5.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

val releaseFlags = Seq("-Xdisable-assertions", "-opt:l:method,inline", "-opt-inline-from", "parsley.**", "-opt-warnings:at-inline-failed")
val noReleaseFlagsScala3 = true // maybe some day this can be turned off...

inThisBuild(List(
  tlBaseVersion := "5.0",
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
  githubWorkflowAddedJobs += testCoverageJob(githubWorkflowGeneratedCacheSteps.value.toList),
  githubWorkflowConcurrency := None, // this allows us to not fail the pipeline on double commit
  // Website Configuration
  tlSitePublishBranch := Some(mainBranch),
))

lazy val root = tlCrossRootProject.aggregate(parsley, parsleyDebug)

// These settings are shared between all projects.
lazy val commonSettings = Seq(
  headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
  headerEmptyLine := false,

  resolvers ++= Opts.resolver.sonatypeOssReleases, // Will speed up MiMA during fast back-to-back releases
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.2.17" % Test,
    "org.scalacheck" %%% "scalacheck" % "1.17.0" % Test,
    "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.15.0" % Test,
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

lazy val parsleyDebug = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("parsley-debug"))
  .dependsOn(parsley % "compile->compile;test->test") // Forwards the test classes to this project. Needed for ParsleyTest.
  .settings(
    name := "parsley-debug",
    commonSettings,
    scalacOptions ++= {
        scalaVersion.value match {
            case Scala213 => Seq("-Ymacro-annotations")
            case Scala3   => Seq.empty
            case Scala212 => Seq.empty
        }
    },
    libraryDependencies ++= {
      // Reflection library choice per Scala version.
      scalaVersion.value match {
        case v@Scala212 => Seq(
            "org.scala-lang" % "scala-reflect" % v,
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
        )
        case v@Scala213 => Seq("org.scala-lang" % "scala-reflect" % v)
        case _          => Seq()
      }
    },

    tlVersionIntroduced := Map(
      "2.13" -> "4.5.0",
      "2.12" -> "4.5.0",
      "3"    -> "4.5.0",
    ),
  )

lazy val docs = project
  .in(file("site"))
  .dependsOn(parsley.jvm)
  .enablePlugins(ParsleySitePlugin)
  .settings(
    tlSiteApiModule := Some((parsley.jvm / projectID).value),
    libraryDependencySchemes ++= Seq(
        // this helps us when parsley-cats is trailing behind us
        "com.github.j-mie6" %% "parsley" % VersionScheme.Always,
    ),
    libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.10.0",
        "com.github.j-mie6" %% "parsley-cats" % "1.3.0"
    ),
  )

def testCoverageJob(cacheSteps: List[WorkflowStep]) = WorkflowJob(
    id = "coverage",
    name = "Run Test Coverage and Upload",
    cond = Some(s"github.ref == 'refs/heads/$mainBranch' || (github.event_name == 'pull_request' && github.base_ref == '$mainBranch')"),
    steps =
        WorkflowStep.Checkout ::
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
