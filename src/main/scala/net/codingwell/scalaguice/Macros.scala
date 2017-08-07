package net.codingwell.scalaguice

import scala.reflect.macros.whitebox

object Macros {
  def defineManifest[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    defineManifestForType(c)(weakTypeOf[T])
  }

  private def defineManifestForType(c: whitebox.Context)(cl: c.universe.Type): c.Tree = {
    import c.universe._

    // the point is to convert `=> T` -> `Function0[T]`
    val finalTypes = cl.typeArgs.map { arg =>
      isByNameType(c)(arg) match {
        case Some(byNameArg) => extractType(c)(s"???.asInstanceOf[scala.Function0[$byNameArg]]")
        case None => arg
      }
    }

    val createManifest = finalTypes.size match {
      case 0 => q"ManifestFactory.classType(classOf[$cl])"
      case 1 => q"ManifestFactory.classType(classOf[$cl], ${defineManifestForType(c)(finalTypes.head)})"
      case _ =>
        q"""
           ManifestFactory.classType(
              classOf[$cl],
              ${defineManifestForType(c)(finalTypes.head)},
              ..${finalTypes.tail.map(cl => defineManifestForType(c)(cl))}
           )

         """
    }

    q"""
         {
            import scala.reflect.ManifestFactory

            $createManifest
         }
       """
  }

  private def isByNameType(c: whitebox.Context)(t: c.universe.Type): Option[c.universe.Type] = {
    import c.universe._

    // cannot be global, uses the universe context
    val d = definitions.ByNameParamClass

    t match {
      case TypeRef(_, `d`, args) => args.headOption
      case _ => None
    }
  }

  private def extractType(c: whitebox.Context)(q: String): c.universe.Type = {
    c.typecheck(c.parse(q)).tpe
  }
}
