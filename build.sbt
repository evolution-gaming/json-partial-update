name := "json-partial-update"

organization := "com.evolutiongaming"

homepage := Some(new URL("http://github.com/evolution-gaming/json-partial-update"))

startYear := Some(2016)

publishMavenStyle := true

publishTo := Some(Resolver.evolutionReleases)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

scalaVersion := crossScalaVersions.value.last

crossScalaVersions := Seq("2.12.17", "2.13.10")

releaseCrossBuild := true

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
)

Compile / doc / scalacOptions += "-no-link-warnings"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.10.1",
  "org.scalatest" %% "scalatest" % "3.0.8"  % Test)

licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))
