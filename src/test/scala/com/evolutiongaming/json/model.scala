package com.evolutiongaming.json

import play.api.libs.json._

case class Phone(area: String, number: String)
case class Address(city: String, street: String, building: Int)
case class Profile(
  @skip id: String,
  name: String,
  address: Address,
  alias: Option[String],
  phone: Option[Phone],
  `type`: Option[ProfileType],
)

case class Version(value: Int) extends AnyVal

object Version {
  // the overload makes value-class specs also cover companion `apply` overload resolution
  def apply(value: String): Version = Version(value.toInt)
}

case class Doc(rev: Version, tag: Option[Version], name: String)

case class Box[A](value: A, name: String)

sealed abstract case class ProfileType(value: String)

object ProfileType {
  val Free: ProfileType = new ProfileType("free") {}
  val Premium: ProfileType = new ProfileType("premium") {}

  def of(code: String): Either[String, ProfileType] = code match {
    case Free.value => Right(Free)
    case Premium.value => Right(Premium)
    case _ => Left(s"Unknown profile type: $code")
  }

  implicit val JsonFormat: Format[ProfileType] = new Format[ProfileType] {
    def reads(json: JsValue): JsResult[ProfileType] = {
      for {
        str <- json.validate[String]
        tpe <- ProfileType.of(str) match {
          case Right(tpe) => JsSuccess(tpe)
          case Left(err) => JsError(err)
        }
      } yield tpe
    }

    override def writes(o: ProfileType): JsValue = JsString(o.value)
  }
}
