package com.evolutiongaming.json

import org.scalatest.matchers.must.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

import play.api.libs.json.*
import com.evolutiongaming.json.PartialUpdater.*

/**
  * Parameterized case classes are supported on Scala 3 only:
  * the Scala 2 macro reads field types without substituting type parameters
  * and fails to expand for such types.
  * See https://github.com/evolution-gaming/json-partial-update/pull/132/changes/4fde29b66c12423786a398e0a596c5a01cf8a28c
  */
class PartialUpdaterGenericSpec extends AnyWordSpec {
  import PartialUpdaterGenericSpec.*

  implicit val boxUpdater: PartialUpdater[Box[String]] = PartialUpdater.updater[Box[String]]
  val box = Box[String](value = "value", tag = Some("tag"), name = "name")

  "PartialUpdater of generic entity" must {
    "not affect entity if json is empty" in {
      (box updated Json.obj()) mustBe box
    }
    "affect field of parameterized type" in {
      (box updated Json.obj("value" -> "updated")) mustBe box.copy(value = "updated")
    }
    "affect optional field of parameterized type" in {
      (box updated Json.obj("tag" -> "updated")) mustBe box.copy(tag = Some("updated"))
      (box updated Json.obj("tag" -> JsNull)) mustBe box.copy(tag = None)
    }
    "affect plain field" in {
      (box updated Json.obj("name" -> "updated")) mustBe box.copy(name = "updated")
    }
  }
}

object PartialUpdaterGenericSpec {
  case class Box[A](value: A, tag: Option[A], name: String)
}
