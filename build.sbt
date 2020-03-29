val scala2Version = "2.13.1"
val projectName = "parsley"
val parsleyVersion = "1.3.0"

lazy val root = project.in(file("."))
  .settings(
    name := projectName,
    version := parsleyVersion,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",

    libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-xml" % "1.2.0",
                                "org.scalactic" %% "scalactic" % "3.0.8" % "test",
                                "org.scalatest" %% "scalatest" % "3.0.8" % "test"),
    scalaVersion := scala2Version,

    scalacOptions ++= Seq("-deprecation", "-unchecked"),

    crossPaths := false, // We can't cross compile this, because of binary differences?

    artifactPath in packageBin in Compile := baseDirectory.value / "%s_%s-%s.jar".format(projectName, scala2Version, parsleyVersion)
  )


