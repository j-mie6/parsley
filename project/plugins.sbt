val sbtTypelevelVersion = "0.4.20"

libraryDependencySchemes ++= Seq(
  "org.scala-native" % "sbt-scala-native" % VersionScheme.Always,
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always,
)

//addSbtPlugin("org.typelevel" % "sbt-typelevel" % sbtTypelevelVersion) // don't want header and formatting checks
addSbtPlugin("org.typelevel" % "sbt-typelevel-settings" % sbtTypelevelVersion)
addSbtPlugin("org.typelevel" % "sbt-typelevel-ci-release" % sbtTypelevelVersion)

// CI Stuff
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.12.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.9")

// This is here purely to enable the niceness settings
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.6")
addSbtPlugin("com.beautiful-scala" % "sbt-scalastyle" % "1.5.1")
addSbtPlugin("org.jmotor.sbt" % "sbt-dependency-updates" % "1.2.7")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.5")
addSbtPlugin("com.timushev.sbt" % "sbt-rewarn" % "0.1.3")
