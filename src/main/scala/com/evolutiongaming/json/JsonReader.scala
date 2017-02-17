package com.evolutiongaming.json

import play.api.libs.json._

case class JsonReader(json: JsValue) {
  private def get(field: String): Option[JsValue] = (json \ field).asOpt[JsValue]
  private def getOpt(field: String): Option[Option[JsValue]] = get(field) match {
    case None         => None
    case Some(JsNull) => Some(None)
    case Some(x)      => Some(Some(x))
  }

  def reader(field: String): Option[JsonReader] = get(field) map JsonReader.apply
  def readerOpt(field: String): Option[Option[JsonReader]] = getOpt(field) map {_ map JsonReader.apply}
  def opt[T: Reads](field: String): Option[T] = get(field) map {_.as[T]}
  def optOpt[T: Reads](field: String): Option[Option[T]] = getOpt(field) map {_ map {_.as[T]}}
}
