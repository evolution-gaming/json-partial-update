package com.evolutiongaming.json

import org.scalatest.matchers.must.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

import play.api.libs.json.*
import com.evolutiongaming.json.PartialUpdater.*

class PartialUpdaterDerivedSpec extends AnyWordSpec {
  import PartialUpdaterDerivedSpec.*

  "derives PartialUpdater" must {
    "not affect entity if json is empty" in {
      (account updated Json.obj()) mustBe account
    }
    "not affect field marked as skip" in {
      (account updated Json.obj("id" -> "updated")) mustBe account
    }
    "affect plain field" in {
      (account updated Json.obj("name" -> "updated")) mustBe account.copy(name = "updated")
    }
    "affect optional field" in {
      (account updated Json.obj("alias" -> "updated")) mustBe account.copy(alias = Some("updated"))
      (account updated Json.obj("alias" -> JsNull)) mustBe account.copy(alias = None)
    }
  }
}

object PartialUpdaterDerivedSpec {
  case class Account(@skip id: String, name: String, alias: Option[String]) derives PartialUpdater

  val account = Account(id = "id", name = "name", alias = Some("alias"))
}
