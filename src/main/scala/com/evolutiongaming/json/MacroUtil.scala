package com.evolutiongaming.json

import scala.language.experimental.macros
import scala.language.postfixOps
import scala.reflect.macros.blackbox

/**
  * The only purpose of this class is to provide the ability to
  * extract Map[String, Class[_]] of case class fields which are
  * not marked by @skip annotation.
  *
  * This map is used by PartialUpdater macro.
  */
private[json] object MacroUtil {
  def fieldMap[T]: Map[String, Class[_]] = macro fieldMapImpl[T]

  def fieldMapImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Map[String, Class[_]]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val fields = MacroUtil.fieldMap(c)(tpe) map { case (n, t) =>
      val name = n.decodedName.toString
      val tpe = TypeName(t.typeSymbol.name.decodedName.toString)
      q"$name -> classOf[$tpe]"
    }
    val code = q"Map( ..$fields )"
    c.Expr[Map[String, Class[_]]](code)
  }

  def fieldMap(c: blackbox.Context)(tpe: c.universe.Type): Seq[(c.universe.TermName, c.universe.Type)] = {
    import c.universe._

    val annotations = tpe.decls.collect {
      case s: MethodSymbol if s.isCaseAccessor =>
        // workaround: force loading annotations
        s.accessed.info

        s.name.toString.trim -> s.accessed.annotations
    }.toMap

    def shouldSkip(name: String): Boolean = {
      val fieldAnnotations = annotations.getOrElse(name, List.empty)
      fieldAnnotations.exists(_.tree.tpe <:< typeOf[skip])
    }

    object UpdatableField {
      def unapply(s: TermSymbol): Option[(TermName, Type)] = {
        val name = s.name.toString.trim
        if ( s.isVal
          && s.isCaseAccessor
          && !shouldSkip(name)) Some((TermName(name), s.typeSignature)) else None
      }
    }

    tpe.decls.collect {case UpdatableField(n, t) => (n, t)} toSeq
  }
}
