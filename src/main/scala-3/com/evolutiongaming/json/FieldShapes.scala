package com.evolutiongaming.json

import scala.quoted.*

/**
  * Classifies a field's (non-Option) type into one of three shapes,
  * each of which needs different generated code:
  *
  *  - `ValueClass` - a case class extending AnyVal, e.g. `Version(value: Int)`:
  *    json holds the inner value (`42`, not `{"value": 42}`), so the generated code
  *    parses the inner type and re-wraps it via the companion's `apply`.
  *  - `CaseClass` - a concrete case class, e.g. `Address`: updated recursively, so
  *    `{"address": {"street": "x"}}` only touches `street`; requires a
  *    `PartialUpdater[Address]` in implicit scope.
  *  - `Generic` - everything else (String, Int, sealed hierarchies, abstract case
  *    classes): treated as an opaque value and parsed whole with a play-json `Reads`.
  *
  * The `Q <: Quotes & Singleton` type parameter keeps the path-dependent reflect
  * types (`TypeRepr` etc.) of this class compatible with the caller's `Quotes`
  * instance.
  */
private[json] class FieldShapes[Q <: Quotes & Singleton](using val quotes: Q) {
  import quotes.reflect.*

  sealed trait Shape { def tpe: TypeRepr }

  object Shape {
    case class ValueClass(tpe: TypeRepr, innerType: TypeRepr) extends Shape
    case class CaseClass(tpe: TypeRepr) extends Shape
    case class Generic(tpe: TypeRepr) extends Shape
  }

  def of(fieldType: TypeRepr): Shape = {
    val symbol = fieldType.typeSymbol
    if (symbol.isClassDef && symbol.flags.is(Flags.Case)) {
      if (fieldType <:< TypeRepr.of[AnyVal]) {
        val innerField = symbol.caseFields.head
        Shape.ValueClass(fieldType, fieldType.memberType(innerField).widenByName)
      } else if (symbol.flags.is(Flags.Abstract)) {
        Shape.Generic(fieldType)
      } else {
        Shape.CaseClass(fieldType)
      }
    } else {
      Shape.Generic(fieldType)
    }
  }
}
