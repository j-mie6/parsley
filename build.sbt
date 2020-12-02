val projectName = "parsley"

inThisBuild(List(
  organization := "parsley",
  homepage := Some(url("https://github.com/J-mie6/parsley")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "J-mie6",
      "Jamie Willis",
      "j.willis19@imperial.ac.uk",
      url("https://github.com/J-mie6")
    )
  )
))

val scala212Version = "2.12.12"
val scala213Version = "2.13.4"
val dottyVersion = "0.27.0-RC1"
val scala3Version = "3.0.0-M1" // This doesn't appear to be picking up yet

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := projectName,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.2" % Test
    ),
    scalaVersion := scala213Version,
    crossScalaVersions := List(scala212Version, scala213Version, dottyVersion/*, scala3Version*/),

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    scalacOptions ++= {
      if (isDotty.value)
        Seq(
          "-source:3.0-migration"
        )
      else Seq.empty
    },

    // Trick from sbt-spiewak: disable dottydoc, which is struggling
    // with our package object.
    Compile / doc / sources := {
      val old = (Compile / doc / sources).value
      if (isDotty.value)
        Seq()
      else
        old
    }
  )
