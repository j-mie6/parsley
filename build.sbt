val scala2Version = "2.13.3"
val projectName = "parsley"
val parsleyVersion = "1.5.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := projectName,
    version := parsleyVersion,
    target in Compile in doc := baseDirectory.value / "docs",

    libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.0.8" % "test",
                                "org.scalatest" %% "scalatest" % "3.0.8" % "test"),
    scalaVersion := scala2Version,
    crossScalaVersions := List(scala2Version, "2.12.12"),

    scalacOptions ++= Seq("-deprecation", "-unchecked"),
  )
