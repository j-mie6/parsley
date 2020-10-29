val projectName = "parsley"
val parsleyVersion = "1.5.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .settings(
    name := projectName,
    version := parsleyVersion,
    target in Compile in doc := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.2" % Test,
      "org.scalatest" %% "scalatest" % "3.2.2" % Test
    ),
    scalaVersion := "2.13.3",
    crossScalaVersions := (scalaVersion.value :: List("2.12.12", "0.27.0-RC1")),

    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    scalacOptions ++= {
      if (isDotty.value)
        Seq(
          "-language:implicitConversions",
          "-source:3.0-migration"
        )
      else Seq.empty
    }
  )
