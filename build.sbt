name := "json-partial-update"

organization := "com.evolutiongaming"

homepage := Some(new URL("http://github.com/evolution-gaming/json-partial-update"))

startYear := Some(2016)

publishMavenStyle := true

organizationName := "Evolution Gaming"

resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")

organizationHomepage := Some(url("http://evolutiongaming.com"))

bintrayOrganization := Some("evolutiongaming")

scalaVersion := crossScalaVersions.value.last

crossScalaVersions := Seq("2.11.12", "2.12.17", "2.13.0")

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

scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits", "-no-link-warnings")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.7.4",
  "org.scalatest" %% "scalatest" % "3.0.8"  % Test)

licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))
