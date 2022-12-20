import org.scalajs.linker.interface.ESVersion
import com.typesafe.tools.mima.core._

val projectName = "parsley"
val Scala213 = "2.13.10"
val Scala212 = "2.12.17"
val Scala3 = "3.2.1"

Global / onChangedBuildSource := ReloadOnSourceChanges

val isInPublish = Option(System.getenv("GITHUB_JOB")).contains("publish")
val releaseFlags = Seq("-Xdisable-assertions", "-opt:l:method,inline", "-opt-inline-from", "parsley.**", "-opt-warnings:at-inline-failed")

inThisBuild(List(
  tlBaseVersion := "4.0",
  organization := "com.github.j-mie6",
  startYear := Some(2018),
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
  ),
  // CI Configuration
  tlCiReleaseBranches := Seq("master", "new-sbt"),
  tlSonatypeUseLegacyHost := true, // this needs to be switched off when we migrate parsley to the other server too
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"), JavaSpec.temurin("11"), JavaSpec.temurin("17")),
  // We need this because our release uses different flags
  githubWorkflowArtifactUpload := false,
))

//Compile / bloopGenerate := None
//Test / bloopGenerate := None

lazy val root = tlCrossRootProject.aggregate(parsley)

lazy val parsley = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("parsley"))
  .settings(
    name := projectName,

    libraryDependencies ++= Seq(
        "org.scalatest" %%% "scalatest" % "3.2.14" % Test,
    ),

    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oI"),

    //scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    /*scalacOptions ++= {
        if (scalaBinaryVersion.value == "3") Seq("-source:3.0-migration") else Seq.empty
    },*/
    scalacOptions ++= {
        if (isInPublish) releaseFlags else Seq.empty
    },

    Compile / doc / scalacOptions ++= Seq("-groups", "-doc-root-content", s"${baseDirectory.value.getParentFile.getPath}/rootdoc.md"),
    Compile / doc / scalacOptions ++= {
        if (scalaBinaryVersion.value == "3") Seq("-comment-syntax:wiki") else Seq.empty
    },
  )
  .jvmSettings(
    //crossScalaVersions := List(Scala212, Scala213, Scala3),
    //versionPolicyPreviousVersions := previousStableVersion.value.toSeq           // This needs to be set to ensure that sbt-version-policy doesn't try handling the new SNAPSHOT versions.
  )
  .jsSettings(
    //crossScalaVersions := List(Scala212, Scala213, Scala3),
    Compile / bloopGenerate := None,
    Test / bloopGenerate := None,
    Test / scalaJSLinkerConfig := scalaJSLinkerConfig.value.withESFeatures(_.withESVersion(ESVersion.ES2018))
  )
  .nativeSettings(
    //crossScalaVersions := List(Scala212, Scala213, Scala3),
    Compile / bloopGenerate := None,
    Test / bloopGenerate := None
  )
