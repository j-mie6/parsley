val scala2Version = "2.13.3"
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

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := projectName,

    libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.0.8" % "test",
                                "org.scalatest" %% "scalatest" % "3.0.8" % "test"),
    scalaVersion := scala2Version,
    crossScalaVersions := List(scala2Version, "2.12.12"),

    scalacOptions ++= Seq("-deprecation", "-unchecked"),
  )
