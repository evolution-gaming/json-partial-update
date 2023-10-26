package com.evolutiongaming.json

import org.scalatest.MustMatchers._
import org.scalatest.WordSpec

import play.api.libs.json._
import com.evolutiongaming.json.PartialUpdater._

class PartialUpdaterSpec extends WordSpec {
  import PartialUpdaterSpec._

  implicit val phoneReads: Reads[Phone]                = Json.reads[Phone]
  implicit val phoneUpdater: PartialUpdater[Phone]     = PartialUpdater.updater[Phone]
  implicit val addressUpdater: PartialUpdater[Address] = PartialUpdater.updater[Address]
  implicit val profileUpdater: PartialUpdater[Profile] = PartialUpdater.updater[Profile]

  s"PartialUpdater" must {
    "not affect entity if json is empty" in new Scope {
      (profile updated json"""{}""") mustBe profile
    }
    "not affect entity if json is not empty, but there is no related fields" in new Scope {
      (profile updated json"""{"foo": "bar"}""") mustBe profile
    }
    "not affect entity's 'id' if json contains 'id' but it is marked as 'skip'" in new Scope {
      (profile updated json"""{"id": "updated"}""") mustBe profile
    }
    "affect entity's 'name' if json contains 'name' property" in new Scope {
      (profile updated json"""{"name": "updated"}""") mustBe profile.copy(name = "updated")
    }
    "affect entity's 'alias' if json contains 'alias' property" in new Scope {
      (profile updated json"""{"alias": "updated"}""") mustBe profile.copy(alias = Some("updated"))
    }
    "affect entity's 'alias' if json contains 'alias' property with value null" in new Scope {
      (profile updated json"""{"alias": null}""") mustBe profile.copy(alias = None)
    }
    "affect entity's 'address/street' if json contains 'address/street' property" in new Scope {
      (profile updated json"""{"address": { "street": "updated" }}""") mustBe profile.copy(address = profile.address.copy(street = "updated"))
    }
    "affect entity's 'phone/area' if json contains 'phone/area' property" in new Scope {
      (profile updated json"""{"phone": { "area": "updated" }}""") mustBe profile.copy(
        phone = profile.phone.map(_.copy(area = "updated"))
      )
    }
    "affect entity's 'phone' if json contains 'phone' property" in new Scope {
      val noPhoneProfile = profile.copy(phone = None)
      (noPhoneProfile updated json"""{"phone": { "area": "updated", "number": "updated" }}""") mustBe profile.copy(
        phone = Some(Phone("updated", "updated"))
      )
    }

    "affect entity's 'phone' if json contains 'phone' property with null" in new Scope {
      (profile updated json"""{"phone": null}""") mustBe profile.copy(phone = None)
    }

    "affect entity's 'type' if json contains 'type' property" in new Scope {
      (profile updated json"""{"type": "premium"}""") mustBe profile.copy(`type` = Some(ProfileType.Premium))
    }
  }

  trait Scope {
    val profile = Profile(
      id = "id",
      name = "name",
      address = Address(
        city = "city",
        street = "street",
        building = 1),
      alias = Some("alias"),
      phone = Some(Phone(
        area = "area",
        number = "number")),
      `type` = Some(ProfileType.Free),
    )
  }
}

object PartialUpdaterSpec {
  implicit class JsonHelper(val sc: StringContext) extends AnyVal {
    def json(args: Any*): JsValue = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuilder(strings.next())
      while(strings.hasNext) {
        buf append expressions.next()
        buf append strings.next()
      }
      Json parse buf.toString
    }
  } 
}