name := "hablapps"

scalaVersion := "2.11.8"

scalaBinaryVersion := "2.11"

organization := "org.hablapps"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6"
)

initialCommands in console := """
  |import org.hablapps.gist._
  """.stripMargin
