enablePlugins(JmhPlugin)
enablePlugins(MicrositesPlugin)

import microsites._

lazy val buildSettings = Seq(
	organization := "parsley.io",
	licenses ++= Seq(
		("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
	),
	crossScalaVersions := Seq("2.10.6", "2.11.12", scalaVersion.value),
)

lazy val commonSettings = Seq(
	scalaVersion := "2.12.7",
	organization := "parsley.io",
	version := "0.1.0-SNAPSHOT",
	javacOptions ++= Seq(
		"-target", "1.8",
		"-source", "1.8",
		"-Xlint:deprecation"),
	scalacOptions ++= Seq(
		"-target:jvm-1.8",
		"-encoding", "UTF-8",
		"-unchecked",
		"-deprecation",
		"-explaintypes",
		"-feature",
		"-Xfuture",

		"-language:existentials",
		"-language:experimental.macros",
		"-language:higherKinds",
		"-language:postfixOps",
		"-language:implicitConversions",

		"-Xlint:adapted-args",
		"-Xlint:by-name-right-associative",
		"-Xlint:constant",
		"-Xlint:delayedinit-select",
		"-Xlint:doc-detached",
		"-Xlint:inaccessible",
		"-Xlint:infer-any",
		"-Xlint:missing-interpolator",
		"-Xlint:nullary-override",
		"-Xlint:nullary-unit",
		"-Xlint:option-implicit",
		//		"-Xlint:package-object-classes", // too widespread
		"-Xlint:poly-implicit-overload",
		"-Xlint:private-shadow",
		"-Xlint:stars-align",
		"-Xlint:type-parameter-shadow",
		"-Xlint:unsound-match",

		"-Yno-adapted-args",
		"-Ywarn-dead-code",
		"-Ywarn-extra-implicit",
		"-Ywarn-inaccessible",
		"-Ywarn-infer-any",
		"-Ywarn-nullary-override",
		"-Ywarn-nullary-unit",
		"-Ywarn-numeric-widen",
		"-Ywarn-unused:implicits",
		//		"-Ywarn-unused:imports",
		"-Ywarn-unused:locals",
		//		"-Ywarn-unused:params", // too widespread
		"-Ywarn-unused:patvars",
		"-Ywarn-unused:privates",
		"-Ywarn-value-discard",
		"-Ypartial-unification",
	),
)

lazy val JmhVersion = "1.21"


lazy val core = project.in(file("core")).settings(
	name := "parsley-core",
	buildSettings, commonSettings,
	libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.1" % Test),
)

lazy val bench = project.in(file("bench")).settings(
	name := "parsley-bench",
	commonSettings,
	libraryDependencies ++= Seq(
		"org.openjdk.jmh" % "jmh-core" % JmhVersion,
		"org.openjdk.jmh" % "jmh-generator-annprocess" % JmhVersion % Compile,

		// benchmark dependencies
		"org.tpolecat" %% "atto-core" % "0.6.3",
		"com.lihaoyi" %% "fastparse" % "1.0.0",
		"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",

		// test dependencies
		"org.typelevel" %% "cats-core" % "1.4.0" % Test,
		"org.scalatest" %% "scalatest" % "3.0.1" % Test,
	),
).dependsOn(core).enablePlugins(JmhPlugin)

lazy val docs = project.in(file("docs")).settings(
	name := "parsley-docs",
	commonSettings,
	scalacOptions in Tut --= Seq(
		"-Ywarn-unused:imports",
		"-Yno-imports"
	)
).dependsOn(core)
	.enablePlugins(MicrositesPlugin)
	.settings(
		micrositeName := "Parsley",
		micrositeDescription := "An exceptionally fast parser combinator library for Scala",
		micrositeAuthor := "Jamie Wills",
		micrositeGithubOwner := "J-mie6",
		micrositeGithubRepo := "Parsley",
		micrositeGitterChannel := false, // no me gusta
		micrositeBaseUrl := "/parsley",
		micrositeDocumentationUrl := "./docs/intro.html",
//		micrositeHighlightTheme := "monokai",
		micrositePalette := Map(
			"brand-primary" -> "#E05236",
			"brand-secondary" -> "#3F3242",
			"brand-tertiary" -> "#2D232F",
			"gray-dark" -> "#453E46",
			"gray" -> "#837F84",
			"gray-light" -> "#E3E2E3",
			"gray-lighter" -> "#F4F3F4",
			"white-color" -> "#FFFFFF"),
		micrositeConfigYaml := ConfigYml(
			yamlCustomProperties = Map(
				"parsleyVersion" -> version.value,
				"scalaVersions" -> crossScalaVersions.value.map(CrossVersion.partialVersion).flatten.map(_._2).mkString("2.", "/", "") // 2.11/12
			)
		)
	)

