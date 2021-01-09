import scala.collection.mutable

val projectName = "parsley"

inThisBuild(List(
  organization := "com.github.j-mie6",
  homepage := Some(url("https://github.com/j-mie6/parsley")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "j-mie6",
      "Jamie Willis",
      "j.willis19@imperial.ac.uk",
      url("https://github.com/j-mie6")
    )
  )
))

val scala212Version = "2.12.12"
val scala213Version = "2.13.4"
val scala3Version = "3.0.0-M3"
val dottyVersion = "0.27.0-RC1"

def usesLib213(major: Long, minor: Long): Boolean = major > 2 || minor >= 13
def adjustForDotty(major: Long): Long = major match {
    case 3 | 0 => 3
    case 2     => 2
}
def extraSources(rootSrcFile: File, base: String, major: Long, minor: Long): Seq[File] = {
    val rootSrc = rootSrcFile.getPath
    val srcs = mutable.ListBuffer.empty[File]
    srcs += file(s"$rootSrc/src/$base/scala-${adjustForDotty(major)}.x")
    srcs += file(s"$rootSrc/src/$base/scala-2.${if (usesLib213(major, minor)) "13+" else "12"}")
    srcs.toList
}
def extraSources(rootSrcFile: File, base: String, version: String): Seq[File] = CrossVersion.partialVersion(version) match {
    case Some((major, minor)) => extraSources(rootSrcFile, base, major, minor)
    case None => Seq.empty
}

def scalaTestDependency(version: String): String = Map("0.27.0-RC1" -> "3.2.2").getOrElse(version, "3.2.3")

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := projectName,
    scalaVersion := scala213Version,

    libraryDependencies ++=
      Seq(
        "org.scalatest" %% "scalatest" % scalaTestDependency(scalaVersion.value) % Test
      ),

    crossScalaVersions := List(scala212Version, scala213Version, scala3Version, dottyVersion),
    Compile / unmanagedSourceDirectories ++= extraSources(baseDirectory.value, "main", scalaVersion.value),
    Test / unmanagedSourceDirectories ++= extraSources(baseDirectory.value, "test", scalaVersion.value),

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    scalacOptions ++= (if (isDotty.value) Seq("-source:3.0-migration") else Seq.empty),

    // Trick from sbt-spiewak: disable dottydoc, which is struggling
    // with our package object.
    Compile / doc / sources := {
      val old = (Compile / doc / sources).value
      if (scalaVersion.value == dottyVersion) Seq() else old
    }
  )
