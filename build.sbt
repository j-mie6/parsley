val projectName = "parsley"
val parsleyVersion = "1.5.0"

val scala212Version = "2.12.12"
val scala213Version = "2.13.3"
val scala3Version = "0.27.0-RC1"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := parsleyVersion

ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := List(scala212Version, scala213Version, scala3Version)

ThisBuild / scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")
ThisBuild / scalacOptions ++= {
  if (isDotty.value) Seq("-source:3.0-migration")
  else Seq.empty
}

lazy val core = project.in(file("core"))
  .settings(
    name := projectName,
    target in Compile in doc := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.2" % Test,
      "org.scalatest" %% "scalatest" % "3.2.2" % Test
    ),
  )

lazy val bench = project
  .enablePlugins(JmhPlugin)
  .settings(
    name := "bench",
    crossScalaVersions := (ThisBuild / crossScalaVersions).value.filterNot(_.startsWith("0.")),
    skip in publish := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "jawn-ast" % "1.0.0"
    )
  )
  .dependsOn(core)
