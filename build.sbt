import sbtversionpolicy.Compatibility.BinaryCompatible
import sbtversionpolicy.Compatibility.BinaryAndSourceCompatible
import sbtdynver.GitDescribeOutput
import scala.collection.mutable
import sbtcrossproject.Platform
import org.scalajs.linker.interface.ESVersion

val projectName = "parsley"

inThisBuild(List(
  organization := "com.github.j-mie6",
  homepage := Some(url("https://github.com/j-mie6/parsley")),
  licenses := List("BSD 3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  developers := List(
    Developer(
      "j-mie6",
      "Jamie Willis",
      "j.willis19@imperial.ac.uk",
      url("https://github.com/j-mie6")
    )
  ),
  versionScheme := Some("early-semver"),
  versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+\\+\\d+".r),
  version := dynverGitDescribeOutput.value.mkVersion(determineVersion(version.value, versionPolicyIntention.value, _), version.value),
  dynver := {
      val d = new java.util.Date
      sbtdynver.DynVer.getGitDescribeOutput(d).mkVersion(determineVersion(dynver.value, versionPolicyIntention.value, _), dynver.value)
  }
))

def parseVersion(version: String): (Int, Int, Int, String) = {
    val (ver, postfix) = version.splitAt(version.indexOf('+'))
    val Array(majorBin, majorSrc, patch) = ver.split('.').map(_.toInt)
    (majorBin, majorSrc, patch, postfix)
}

def determineVersion(currentVersion: String, compat: Compatibility, out: GitDescribeOutput): String = {
    if (!out.isSnapshot) currentVersion
    else {
        // The new SNAPSHOT version must be set accounting for the versionPolicyIntention
        val (majorBin, majorSrc, patch, postfix) = parseVersion(currentVersion)
        val (newMajorBin, newMajorSrc, newPatch) = compat match {
            case Compatibility.BinaryAndSourceCompatible => (majorBin,     majorSrc,     patch + 1)
            case Compatibility.BinaryCompatible          => (majorBin,     majorSrc + 1, 0        )
            case Compatibility.None                      => (majorBin + 1, 0,            0        )
        }
        s"$newMajorBin.$newMajorSrc.$newPatch$postfix"
    }
}

val scala212Version = "2.12.15"
val scala213Version = "2.13.7"
val scala3Version = "3.1.1"

def usesLib213(major: Long, minor: Long): Boolean = major > 2 || minor >= 13
def extraSources(rootSrcFile: File, base: String, major: Long, minor: Long): Seq[File] = {
    val rootSrc = rootSrcFile.getPath
    val srcs = mutable.ListBuffer.empty[File]
    srcs += file(s"$rootSrc/src/$base/scala-$major.x")
    srcs += file(s"$rootSrc/src/$base/scala-2.${if (usesLib213(major, minor)) "13+" else "12"}")
    srcs.toList
}
def extraSources(rootSrcFile: File, base: String, version: String): Seq[File] = CrossVersion.partialVersion(version) match {
    case Some((major, minor)) => extraSources(rootSrcFile, base, major, minor)
    case None => Seq.empty
}

def scalaTestDependency(version: String): String =
    Map()
    .getOrElse(version, "3.2.9")

val PureVisible: CrossType = new CrossType {
    def projectDir(crossBase: File, projectType: String): File =
      crossBase / projectType

    def projectDir(crossBase: File, platform: Platform): File =
      crossBase / platform.identifier

    def sharedSrcDir(projectBase: File, conf: String): Option[File] =
      Some(projectBase.getParentFile / "src" / conf / "scala")
  }

Global / onChangedBuildSource := ReloadOnSourceChanges

// See https://github.com/sbt/sbt/issues/1224
Global / onLoad ~= (_ andThen ("project parsley" :: _))
//scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2018)) }

Compile / bloopGenerate := None
Test / bloopGenerate := None

lazy val parsley = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(PureVisible)
  .in(file("."))
  .settings(
    name := projectName,
    scalaVersion := scala213Version,

    libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestDependency(scalaVersion.value) % Test,

    Compile / unmanagedSourceDirectories ++= extraSources(baseDirectory.value.getParentFile, "main", scalaVersion.value),
    Compile / unmanagedSourceDirectories ++= extraSources(baseDirectory.value, "main", scalaVersion.value),
    Test / unmanagedSourceDirectories ++= extraSources(baseDirectory.value.getParentFile, "test", scalaVersion.value),
    Test / unmanagedSourceDirectories ++= extraSources(baseDirectory.value, "test", scalaVersion.value),

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    scalacOptions ++= {
        if (scalaBinaryVersion.value == "3") Seq("-source:3.0-migration") else Seq.empty
    },

    Compile / doc / scalacOptions ++= Seq("-groups", "-doc-root-content", s"${baseDirectory.value.getParentFile.getPath}/rootdoc.md"),
    Compile / doc / scalacOptions ++= {
        if (scalaBinaryVersion.value == "3") Seq("-comment-syntax", "wiki") else Seq.empty
    },
  )
  .jvmSettings(
    crossScalaVersions := List(scala212Version, scala213Version, scala3Version),
    versionPolicyPreviousVersions := previousStableVersion.value.toSeq           // This needs to be set to ensure that sbt-version-policy doesn't try handling the new SNAPSHOT versions.
  )
  .jsSettings(
    crossScalaVersions := List(scala212Version, scala213Version, scala3Version),
    Compile / bloopGenerate := None,
    Test / bloopGenerate := None,
    Test / scalaJSLinkerConfig := scalaJSLinkerConfig.value.withESFeatures(_.withESVersion(ESVersion.ES2018))
  )
  .nativeSettings(
    crossScalaVersions := List(scala212Version, scala213Version),
    Compile / bloopGenerate := None,
    Test / bloopGenerate := None
  )
