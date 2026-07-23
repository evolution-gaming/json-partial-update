# json-partial-update [![Build Status](https://travis-ci.org/evolution-gaming/json-partial-update.svg)](https://travis-ci.org/evolution-gaming/json-partial-update) [![Coverage Status](https://coveralls.io/repos/evolution-gaming/json-partial-update/badge.svg)](https://coveralls.io/r/evolution-gaming/json-partial-update) [ ![version](https://api.bintray.com/packages/evolutiongaming/maven/json-partial-update/images/download.svg) ](https://bintray.com/evolutiongaming/maven/json-partial-update/_latestVersion) [![License: MIT](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

PlayJson extension for partial updates

Cross-built for Scala 2.13 and Scala 3.

## Usage

```scala
import com.evolutiongaming.json.PartialUpdater
import com.evolutiongaming.json.PartialUpdater._

case class Entity(a: String, b: Option[String])

implicit val entityUpdater: PartialUpdater[Entity] = PartialUpdater.updater[Entity]

val updated = entity updated json // only fields present in json are updated
```

On Scala 3 the updater can also be derived:

```scala
case class Entity(a: String, b: Option[String]) derives PartialUpdater
```

## Installation

```scala
libraryDependencies += "com.evolutiongaming" %% "json-partial-update" % "0.1.11"
```
