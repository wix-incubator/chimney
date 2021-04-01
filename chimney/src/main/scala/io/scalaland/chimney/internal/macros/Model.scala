package io.scalaland.chimney.internal.macros

import scala.reflect.macros.blackbox

trait Model extends TransformerConfigSupport {

  val c: blackbox.Context

  import c.universe._

  case class Target(name: String, tpe: Type) {
    val annotations: Option[Seq[Annotation]] = None //so it doesn't get used in equals/hashCode
  }
  object Target {
    def fromJavaBeanSetter(ms: MethodSymbol, site: Type): Target =
      Target(ms.canonicalName, ms.beanSetterParamTypeIn(site))

    def fromField(ms: MethodSymbol, site: Type): Target =
      Target(ms.canonicalName, ms.resultTypeIn(site))

    def fromField(ms: MethodSymbol, site: Type, annotationsOpt: Option[Seq[Annotation]]): Target =
      new Target(ms.canonicalName, ms.resultTypeIn(site)) {
        override val annotations: Option[Seq[Annotation]] = annotationsOpt
      }
  }

  case class TransformerBodyTree(tree: Tree, isWrapped: Boolean)

  sealed trait AccessorResolution extends Product with Serializable {
    def isResolved: Boolean
  }
  object AccessorResolution {
    case object NotFound extends AccessorResolution {
      override def isResolved: Boolean = false
    }
    case class Resolved(symbol: MethodSymbol, wasRenamed: Boolean) extends AccessorResolution {
      override def isResolved: Boolean = true
    }
    case object DefAvailable extends AccessorResolution {
      override def isResolved: Boolean = false
    }
  }
}
