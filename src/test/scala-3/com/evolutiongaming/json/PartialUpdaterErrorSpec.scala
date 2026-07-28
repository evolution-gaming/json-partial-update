package com.evolutiongaming.json

import org.scalatest.matchers.must.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

import scala.compiletime.testing.{Error, typeCheckErrors}

class PartialUpdaterErrorSpec extends AnyWordSpec {

  "PartialUpdater derivation" must {
    "report every missing implicit in a single aggregated error" in {
      // the explicit type annotation matters: without it the macro's implicit
      // search inside typeCheckErrors trips over the val being defined
      // (CyclicReference: recursive value needs type)
      val errors: List[Error] = typeCheckErrors(
        """
        class NoReadsA
        class NoReadsB
        case class Broken(a: NoReadsA, b: NoReadsB, name: String)
        com.evolutiongaming.json.PartialUpdater.updater[Broken]
        """)

      errors.map(_.message) match {
        case List(message) =>
          message must include("Can not derive PartialUpdater")
          message must include("Reads[NoReadsA] (required for field 'a')")
          message must include("Reads[NoReadsB] (required for field 'b')")
        case other =>
          fail(s"expected exactly one aggregated error, got: $other")
      }
    }

    "list both missing requirements of an optional case class field" in {
      val errors: List[Error] = typeCheckErrors(
        """
        case class Inner(x: Int)
        case class Outer(inner: Option[Inner])
        com.evolutiongaming.json.PartialUpdater.updater[Outer]
        """)

      errors.map(_.message) match {
        case List(message) =>
          message must include("PartialUpdater[Inner] (required for field 'inner')")
          message must include("Reads[Inner] (required for field 'inner')")
        case other =>
          fail(s"expected exactly one aggregated error, got: $other")
      }
    }

    "reject types that are not concrete case classes" in {
      val errors: List[Error] = typeCheckErrors(
        """
        class NotACaseClass
        com.evolutiongaming.json.PartialUpdater.updater[NotACaseClass]
        """)

      errors.map(_.message) match {
        case List(message) => message must include("is not a concrete case class")
        case other         => fail(s"expected exactly one error, got: $other")
      }
    }
  }
}
