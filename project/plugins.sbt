// CI Stuff
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.7")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "2.0.1")
// Cross Building
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.10.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.3") // TODO: 0.4.4 bump waiting on scalatest, but can enable scala 3
// Cross Project Setup
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
// Other
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.8") // This is here purely to enable the niceness settings