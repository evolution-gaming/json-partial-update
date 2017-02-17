package com.evolutiongaming.json

import org.scalatest.MustMatchers._
import org.scalatest.WordSpec

class MacroUtilSpec extends WordSpec {
  import MacroUtilSpec._

  "Util" must {
    "enumerate fields" in {
      val fields = MacroUtil.fieldMap[MacroUtilSpec.Ex1]
      fields mustBe resultingMap
    }
    "not enumerate fields to skip" in {
      val fields = MacroUtil.fieldMap[MacroUtilSpec.Ex2]
      fields mustBe resultingMap
    }
  }

}

object MacroUtilSpec {
  case class Ex1(foo: String, bar: Int)
  case class Ex2(@skip baz: Long, foo: String, bar: Int)

  val resultingMap = Map("foo" -> classOf[String], "bar" -> classOf[Int])
}
