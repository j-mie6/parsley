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
val scala213Version = "2.13.3"
val scala3Version = "0.27.0-RC1"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := projectName,

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.2" % Test,
      "org.scalatest" %% "scalatest" % "3.2.2" % Test
    ),
    scalaVersion := scala213Version,
    crossScalaVersions := List(scala212Version, scala213Version, scala3Version),

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    scalacOptions ++= {
      if (isDotty.value)
        Seq(
          "-source:3.0-migration"
        )
      else Seq.empty
    }
  )
