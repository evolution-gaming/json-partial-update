name := "json-partial-update"

organization := "com.evolutiongaming"

homepage := Some(url("https://github.com/evolution-gaming/json-partial-update"))

startYear := Some(2016)

publishMavenStyle := true

publishTo := Some(Resolver.evolutionReleases)

organizationName := "Evolution"

organizationHomepage := Some(url("https://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.18", "3.3.8")

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xfatal-warnings",
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq(
      "-Xlint",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
    )
    case _ => Seq.empty
  }
}

Compile / doc / scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq("-no-link-warnings")
    case _            => Seq.empty
  }
}

libraryDependencies ++= Seq(
  "org.playframework" %% "play-json" % "3.0.6",
  "org.scalatest" %% "scalatest" % "3.2.20"  % Test)

licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

//addCommandAlias("check", "all versionPolicyCheck Compile/doc")
addCommandAlias("check", "show version")
addCommandAlias("build", "+all compile test")
