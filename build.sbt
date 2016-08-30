name := "hablapps"

scalaVersion := "2.11.8"

scalaBinaryVersion := "2.11"

organization := "org.hablapps"

libraryDependencies ++= Seq(
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
