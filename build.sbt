name := "hablapps"

scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel"

scalaBinaryVersion := "2.11"

organization := "org.hablapps"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

resolvers ++= Seq(
  "Speech repo - releases" at "http://repo.hablapps.com/releases")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1",
  "org.scalaz" %% "scalaz-core" % "7.3.0-M4-HABLAPPS",
  "org.scalatest" %% "scalatest" % "3.0.0",
  "com.typesafe.akka" %% "akka-http-experimental" % "2.4.11",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "2.4.11",
  "com.lihaoyi" %% "sourcecode" % "0.1.2",
  "com.github.julien-truffaut"  %%  "monocle-core"    % "1.3.2",
  "com.github.julien-truffaut"  %%  "monocle-generic" % "1.3.2",
  "com.github.julien-truffaut"  %%  "monocle-macro"   % "1.3.2",
  "com.github.julien-truffaut"  %%  "monocle-state"   % "1.3.2",
  "com.github.julien-truffaut"  %%  "monocle-refined" % "1.3.2",
  "com.github.julien-truffaut"  %%  "monocle-unsafe"  % "1.3.2",
  "com.github.julien-truffaut"  %%  "monocle-law"     % "1.3.2" % "test",
  "org.atnos"                   %% "eff-cats"         % "2.0.0-RC26"

)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Ypartial-unification",
  // "-Xprint:typer",
  // "-Xlog-implicit-conversions",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")

initialCommands in console := """
  |import org.hablapps.gist._
  """.stripMargin
