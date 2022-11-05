// CI Stuff
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.11")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.5")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "2.0.1")
// Cross Building
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.11.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.7")
// Cross Project Setup
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
// Other
//addSbtPlugin("com.beautiful-scala" % "sbt-scalastyle" % "1.5.1") // This can only be enabled when both scoverage and dependency-updates are turned off
addSbtPlugin("org.jmotor.sbt" % "sbt-dependency-updates" % "1.2.7")
// This is here purely to enable the niceness settings
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.8")
