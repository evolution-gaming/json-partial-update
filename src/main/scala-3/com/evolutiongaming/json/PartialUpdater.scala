package com.evolutiongaming.json

import play.api.libs.json.{JsValue, Reads}

import scala.quoted.*

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
  *   import com.evolutiongaming.json.PartialUpdater.*
  *
  *   implicit val entityUpdater: PartialUpdater[Entity] = PartialUpdater.updater[Entity]
  *
  *   val entity: Entity = ...
  *   val json: JsValue = ...
  *   val updatedEntity = entity updated json
  * }}}
  *
  * or, on Scala 3, simply
  * {{{
  *   case class Entity(a: String, b: Option[String]) derives PartialUpdater
  * }}}
  *
  * If you use embedded entities, you have to make sure that all
  * needed [[play.api.libs.json.Reads]] are accessible in implicit scope.
  *
  * For more examples please take a look at `com.evolutiongaming.json.PartialUpdaterSpec`.
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

  inline def updater[T]: PartialUpdater[T] = ${ updaterImpl[T] }

  inline def derived[T]: PartialUpdater[T] = ${ updaterImpl[T] }

  private def updaterImpl[T: Type](using quotes: Quotes): Expr[PartialUpdater[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    if (!(symbol.isClassDef && symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Abstract)))
      report.errorAndAbort(s"${tpe.show} is not a concrete case class")

    val shapes = new FieldShapes[quotes.type]
    import shapes.Shape

    def summonOrAbort[A: Type](field: String): Expr[A] =
      Expr.summon[A].getOrElse(
        report.errorAndAbort(s"No implicit ${TypeRepr.of[A].show} found (required for field '$field' of ${tpe.show})"))

    val updatable = MacroUtil.fieldMap(tpe).toMap

    def fieldValue(entity: Expr[T], reader: Expr[JsonReader], field: Symbol): Term = {
      val name = field.name
      val sel = Select.unique(entity.asTerm, name)

      updatable.get(name) match {
        // field is marked with @skip: keep the current value
        case None => sel

        case Some(fieldType) =>
          val path = Expr(name)
          val isOption = fieldType <:< TypeRepr.of[Option[Any]]
          val innerType = if (isOption) {
            fieldType.dealias match {
              case AppliedType(_, List(arg)) => arg
              case _ => report.errorAndAbort(s"Unsupported type of field '$name': ${fieldType.show}")
            }
          } else fieldType

          val expr = (isOption, shapes.of(innerType)) match {
            case (false, Shape.Generic(ft)) => ft.asType match {
              case '[f] =>
                val reads = summonOrAbort[Reads[f]](name)
                '{ $reader.opt[f]($path)(using $reads) getOrElse ${ sel.asExprOf[f] } }
            }
            case (true, Shape.Generic(ft)) => ft.asType match {
              case '[f] =>
                val reads = summonOrAbort[Reads[f]](name)
                '{ $reader.optOpt[f]($path)(using $reads) getOrElse ${ sel.asExprOf[Option[f]] } }
            }
            case (false, Shape.ValueClass(ft, inner)) => (ft.asType, inner.asType) match {
              case ('[v], '[i]) =>
                val reads = summonOrAbort[Reads[i]](name)
                def wrap(x: Expr[i]): Expr[v] =
                  Select.overloaded(Ref(ft.typeSymbol.companionModule), "apply", Nil, List(x.asTerm)).asExprOf[v]
                '{ $reader.opt[i]($path)(using $reads) map { (x: i) => ${ wrap('x) } } getOrElse ${ sel.asExprOf[v] } }
            }
            case (true, Shape.ValueClass(ft, inner)) => (ft.asType, inner.asType) match {
              case ('[v], '[i]) =>
                val reads = summonOrAbort[Reads[i]](name)
                def wrap(x: Expr[i]): Expr[v] =
                  Select.overloaded(Ref(ft.typeSymbol.companionModule), "apply", Nil, List(x.asTerm)).asExprOf[v]
                '{ $reader.optOpt[i]($path)(using $reads) map { _ map { (x: i) => ${ wrap('x) } } } getOrElse ${ sel.asExprOf[Option[v]] } }
            }
            case (false, Shape.CaseClass(ft)) => ft.asType match {
              case '[f] =>
                val updater = summonOrAbort[PartialUpdater[f]](name)
                '{ $reader.reader($path) map { x => $updater.apply(${ sel.asExprOf[f] }, x) } getOrElse ${ sel.asExprOf[f] } }
            }
            case (true, Shape.CaseClass(ft)) => ft.asType match {
              case '[f] =>
                val updater = summonOrAbort[PartialUpdater[f]](name)
                val reads = summonOrAbort[Reads[f]](name)
                '{
                  val nested: Option[Option[JsonReader]] = $reader.readerOpt($path)
                  (nested, ${ sel.asExprOf[Option[f]] }) match {
                    case (None, None)                        => None
                    case (Some(None), None)                  => None
                    case (Some(Some(_)), None)               => $reader.opt[f]($path)(using $reads)
                    case (None, Some(existing))              => Some(existing)
                    case (Some(None), Some(_))               => None
                    case (Some(Some(inner)), Some(existing)) => Some($updater.apply(existing, inner))
                  }
                }
            }
            case _ => report.errorAndAbort(s"Unsupported type of field '$name': ${fieldType.show}")
          }
          expr.asTerm
      }
    }

    // entity.copy(...) with every field passed positionally: updatable fields get
    // their computed value, @skip fields keep the current value
    def copy(entity: Expr[T], reader: Expr[JsonReader]): Expr[T] = {
      val args = symbol.caseFields.map(field => fieldValue(entity, reader, field))
      val copySel = Select.unique(entity.asTerm, "copy")
      val copyFn = tpe.dealias match {
        case AppliedType(_, targs) => TypeApply(copySel, targs.map(Inferred(_)))
        case _                     => copySel
      }
      Apply(copyFn, args).asExprOf[T]
    }

    '{
      new PartialUpdater[T] {
        def apply(entity: T, reader: JsonReader): T = ${ copy('entity, 'reader) }
      }
    }
  }

}
