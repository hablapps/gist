name := "hablapps"

scalaVersion := "2.11.8"

scalaBinaryVersion := "2.11"

organization := "org.hablapps"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.7.0",
  "org.scalatest" %% "scalatest" % "2.2.6"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  // "-Xprint:typer",
  // "-Xlog-implicit-conversions",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")

initialCommands in console := """
  |import org.hablapps.gist._
  """.stripMargin
