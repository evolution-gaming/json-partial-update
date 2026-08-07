package com.evolutiongaming.json

import scala.quoted.*

/**
  * The only purpose of this object is to enumerate case class fields
  * which are not marked by @skip annotation.
  *
  * This list is used by PartialUpdater macro.
  */
private[json] object MacroUtil {

  /**
    * Enumerates case class fields not marked by @skip, in declaration order.
    *
    * Depending on where the annotation ends up (Scala 3 keeps it on the
    * constructor parameter unless meta-annotated), both the field symbol
    * and the corresponding primary constructor parameter are checked.
    */
  private[json] def fieldMap(using Quotes)(tpe: quotes.reflect.TypeRepr): List[(String, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*

    val symbol = tpe.typeSymbol
    val skipTpe = TypeRepr.of[skip]
    val ctorParams = symbol.primaryConstructor.paramSymss.flatten.filter(_.isTerm)
    val ctorAnnotations = ctorParams.map(p => p.name -> p.annotations).toMap

    def shouldSkip(field: Symbol): Boolean = {
      val annotations = field.annotations ++ ctorAnnotations.getOrElse(field.name, Nil)
      annotations.exists(_.tpe <:< skipTpe)
    }

    symbol.caseFields.filterNot(shouldSkip) map { field =>
      val fieldTpe = tpe.memberType(field) match {
        case ByNameType(result)       => result
        case MethodType(_, _, result) => result
        case t                        => t
      }
      (field.name, fieldTpe)
    }
  }
}
