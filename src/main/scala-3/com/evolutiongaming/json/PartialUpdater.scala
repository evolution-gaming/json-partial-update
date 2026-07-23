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

  private def updaterImpl[T: Type](using Quotes): Expr[PartialUpdater[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    if (!(symbol.isClassDef && symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Abstract)))
      report.errorAndAbort(s"${tpe.show} is not a concrete case class")

    val optionTpe = TypeRepr.of[Option[Any]]

    sealed trait ResultingType { def tpe: TypeRepr }
    object ResultingType {

      case class ValueClass(tpe: TypeRepr, innerType: TypeRepr) extends ResultingType
      case class CaseClass(tpe: TypeRepr) extends ResultingType
      case class Generic(tpe: TypeRepr) extends ResultingType

      def apply(t: TypeRepr): ResultingType = {
        val s = t.typeSymbol
        if (s.isClassDef && s.flags.is(Flags.Case)) {
          if (t <:< TypeRepr.of[AnyVal]) {
            val innerField = s.caseFields.head
            ValueClass(t, t.memberType(innerField).widenByName)
          } else if (s.flags.is(Flags.Abstract)) {
            Generic(t)
          } else {
            CaseClass(t)
          }
        } else {
          Generic(t)
        }
      }
    }

    def summonOrAbort[A: Type](field: String): Expr[A] =
      Expr.summon[A].getOrElse(
        report.errorAndAbort(s"No implicit ${TypeRepr.of[A].show} found (required for field '$field' of ${tpe.show})"))

    val updatable = MacroUtil.fieldMap(tpe).toMap

    def fieldValue(entity: Expr[T], u: Expr[JsonReader], field: Symbol): Term = {
      import ResultingType.*

      val name = field.name
      val sel = Select.unique(entity.asTerm, name)

      updatable.get(name) match {
        // field is marked with @skip: keep the current value
        case None => sel

        case Some(t) =>
          val path = Expr(name)
          val isOption = t <:< optionTpe
          val innerTpe = if (isOption) {
            t.dealias match {
              case AppliedType(_, List(arg)) => arg
              case _ => report.errorAndAbort(s"Unsupported type of field '$name': ${t.show}")
            }
          } else t

          val expr = (isOption, ResultingType(innerTpe)) match {
            case (false, Generic(ft)) => ft.asType match {
              case '[f] =>
                val reads = summonOrAbort[Reads[f]](name)
                '{ $u.opt[f]($path)(using $reads) getOrElse ${ sel.asExprOf[f] } }
            }
            case (true, Generic(ft)) => ft.asType match {
              case '[f] =>
                val reads = summonOrAbort[Reads[f]](name)
                '{ $u.optOpt[f]($path)(using $reads) getOrElse ${ sel.asExprOf[Option[f]] } }
            }
            case (false, ValueClass(ft, inner)) => (ft.asType, inner.asType) match {
              case ('[v], '[i]) =>
                val reads = summonOrAbort[Reads[i]](name)
                def wrap(x: Expr[i]): Expr[v] =
                  Select.overloaded(Ref(ft.typeSymbol.companionModule), "apply", Nil, List(x.asTerm)).asExprOf[v]
                '{ $u.opt[i]($path)(using $reads) map { (x: i) => ${ wrap('x) } } getOrElse ${ sel.asExprOf[v] } }
            }
            case (true, ValueClass(ft, inner)) => (ft.asType, inner.asType) match {
              case ('[v], '[i]) =>
                val reads = summonOrAbort[Reads[i]](name)
                def wrap(x: Expr[i]): Expr[v] =
                  Select.overloaded(Ref(ft.typeSymbol.companionModule), "apply", Nil, List(x.asTerm)).asExprOf[v]
                '{ $u.optOpt[i]($path)(using $reads) map { _ map { (x: i) => ${ wrap('x) } } } getOrElse ${ sel.asExprOf[Option[v]] } }
            }
            case (false, CaseClass(ft)) => ft.asType match {
              case '[f] =>
                val updater = summonOrAbort[PartialUpdater[f]](name)
                '{ $u.reader($path) map { x => $updater.apply(${ sel.asExprOf[f] }, x) } getOrElse ${ sel.asExprOf[f] } }
            }
            case (true, CaseClass(ft)) => ft.asType match {
              case '[f] =>
                val updater = summonOrAbort[PartialUpdater[f]](name)
                val reads = summonOrAbort[Reads[f]](name)
                '{
                  val r: Option[Option[JsonReader]] = $u.readerOpt($path)
                  (r, ${ sel.asExprOf[Option[f]] }) match {
                    case (None, None)              => None
                    case (Some(None), None)        => None
                    case (Some(Some(_)), None)     => $u.opt[f]($path)(using $reads)
                    case (None, Some(xx))          => Some(xx)
                    case (Some(None), Some(_))     => None
                    case (Some(Some(r)), Some(xx)) => Some($updater.apply(xx, r))
                  }
                }
            }
            case _ => report.errorAndAbort(s"Unsupported type of field '$name': ${t.show}")
          }
          expr.asTerm
      }
    }

    // entity.copy(...) with every field passed positionally: updatable fields get
    // their computed value, @skip fields get the current value
    def copy(entity: Expr[T], u: Expr[JsonReader]): Expr[T] = {
      val args = symbol.caseFields.map(f => fieldValue(entity, u, f))
      val copySel = Select.unique(entity.asTerm, "copy")
      val copyFn = tpe.dealias match {
        case AppliedType(_, targs) => TypeApply(copySel, targs.map(t => Inferred(t)))
        case _                     => copySel
      }
      Apply(copyFn, args).asExprOf[T]
    }

    '{
      new PartialUpdater[T] {
        def apply(entity: T, u: JsonReader): T = ${ copy('entity, 'u) }
      }
    }
  }

}
