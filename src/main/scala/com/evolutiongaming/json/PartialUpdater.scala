package com.evolutiongaming.json

import play.api.libs.json.JsValue

import scala.language.experimental.macros
import scala.reflect.macros.{Universe, blackbox}

/**
  * The trait can apply json value to specified object
  * and it do it partially only if corresponding fields is defined in json.
  *
  * So you could have a
  * {{{
  *   case class Entity(a: String, b: Option[String])
  * }}}
  *
  * And then you can expect that:
  * - application of empty js object won't change the state
  * - application of {{{ { "a": "x" } }}} will change
  *   only {{{entity.a}}} to {{{"x"}}}. {{{entity.b}}} will stay unchanged
  * - application of {{{ { "b": "x" } }}} will change
  *   only {{{entity.b}}} to {{{Some("x")}}}. {{{entity.a}}} will stay unchanged
  * - application of {{{ { "b": null } }}} will change
  *   only {{{entity.b}}} to {{{None}}}. {{{entity.a}}} will stay unchanged
  *
  * Dealing with partial updaters is as easy as it is in case of Play formats.
  * The only thing you have to do is declare
  * {{{
  *   import com.evolutiongaming.json.PartialUpdater._
  *
  *   implicit val entityUpdater: PartialUpdater[Entity] = PartialUpdater.updater[Entity]
  *
  *   val entity: Entity = ...
  *   val json: JsValue = ...
  *   val updatedEntity = entity updated json
  * }}}
  *
  * If you use embedded entities, you have to make sure that all
  * needed [[play.api.libs.json.Reads]] are accessible in implicit scope.
  *
  * For more examples please take a look at [[com.evolutiongaming.json.PartialUpdaterSpec]].
  *
  * @tparam T a case class type
  */
trait PartialUpdater[T] {
  def apply(entity: T, reader: JsonReader): T
}

object PartialUpdater {

  implicit class Ops[T](val updatee: T) extends AnyVal {
    def updated(json: JsValue)(implicit updater: PartialUpdater[T]): T = {
      updater.apply(updatee, JsonReader(json))
    }
  }

  private sealed trait ResultingType {def tpe: Universe#Type}
  private object ResultingType {

    case class ValueClass(tpe: Universe#Type, innerType: Universe#Type) extends ResultingType
    case class CaseClass(tpe: Universe#Type) extends ResultingType
    case class Generic(tpe: Universe#Type) extends ResultingType

    def apply(t: Universe#Type): ResultingType = {
      val symbol = t.typeSymbol
      require(symbol.isClass)

      val c = symbol.asClass
      if (c.isCaseClass) {
        if (c.isDerivedValueClass) {
          val innerArg = c.primaryConstructor.asMethod.paramLists.head.head
          val innerType = innerArg.typeSignature
          ValueClass(t, innerType)
        } else if (c.isAbstract) {
          Generic(t)
        } else {
          CaseClass(t)
        }
      } else {
        Generic(t)
      }
    }
  }

  def updater[T]: PartialUpdater[T] = macro updaterImpl[T]

  def updaterImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[PartialUpdater[T]] = {
    import ResultingType._
    import c.universe._

    val tpe = weakTypeOf[T]
    val optionTpe = weakTypeOf[Option[_]]

    val fields = MacroUtil.fieldMap(c)(tpe) map { case (n, t) =>
      val path          = n.decodedName.toString
      val isOption      = t <:< optionTpe
      val resultingType = if (isOption) ResultingType(t.typeArgs.head) else ResultingType(t)

      val expr = (isOption, resultingType) match {
        case (false, Generic(ft: Type))             => q"val $n: $t = u.opt[$ft]($path) getOrElse entity.$n"
        case (true, Generic(ft: Type))              => q"val $n: $t = u.optOpt[$ft]($path) getOrElse entity.$n"
        case (false, ValueClass(ft: Type, i: Type)) => q"val $n: $t = u.opt[$i]($path) map ${ft.typeSymbol.companion}.apply getOrElse entity.$n"
        case (true, ValueClass(ft: Type, i: Type))  => q"val $n: $t = u.optOpt[$i]($path) map {_ map ${ft.typeSymbol.companion}.apply} getOrElse entity.$n"
        case (false, CaseClass(ft: Type))           => q"val $n: $t = u.reader($path) map {x => implicitly[PartialUpdater[$ft]].apply(entity.$n, x)} getOrElse entity.$n"
        case (true, CaseClass(ft: Type))            => q"""
                                 val $n: $t = {
                                   val r: Option[Option[JsonReader]] = u.readerOpt($path)
                                   (r, entity.$n) match {
                                     case (None, None)              => None
                                     case (Some(None), None)        => None
                                     case (Some(Some(_)), None)     => u.opt[$ft]($path)
                                     case (None, Some(a))           => Some(a)
                                     case (Some(None), Some(_))     => None
                                     case (Some(Some(r)), Some(a))  => Some(implicitly[PartialUpdater[$ft]].apply(a, r))
                                   }
                                 }
                                """
        case _ => c.abort(c.enclosingPosition, s"Unsupported type: $t")
      }

      (n, expr)
    }

    val definitions = fields map (_._2)
    val expressions = fields map { case (n, _) => q"$n = $n"}
    val copy = q"entity.copy( ..$expressions )"

    val out =
      q"""
        new PartialUpdater[$tpe] {
          def apply(entity: $tpe, u: JsonReader): $tpe = {
            ..$definitions
            $copy
          }
        }
       """

    c.Expr[PartialUpdater[T]](out)
  }

}
