package com.evolutiongaming.json

import scala.quoted.*

/**
  * The only purpose of this class is to provide the ability to
  * extract Map[String, Class[_]] of case class fields which are
  * not marked by @skip annotation.
  *
  * This map is used by PartialUpdater macro.
  */
private[json] object MacroUtil {
  inline def fieldMap[T]: Map[String, Class[?]] = ${ fieldMapImpl[T] }

  private def fieldMapImpl[T: Type](using Quotes): Expr[Map[String, Class[?]]] = {
    import quotes.reflect.*
    val pairs = fieldMap(TypeRepr.of[T]) map { case (name, tpe) =>
      val clazz = Literal(ClassOfConstant(tpe)).asExprOf[Class[?]]
      '{ ${ Expr(name) } -> $clazz }
    }
    '{ Map(${ Varargs(pairs) }*) }
  }

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
