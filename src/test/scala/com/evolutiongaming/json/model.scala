package com.evolutiongaming.json

case class Phone(area: String, number: String)
case class Address(city: String, street: String, building: Int)
case class Profile(@skip id: String, name: String, address: Address, alias: Option[String], phone: Option[Phone])