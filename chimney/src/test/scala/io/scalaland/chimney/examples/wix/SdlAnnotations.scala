package com.wixpress.infra.sdl.api.id //important, checked by the macro guards

import scala.annotation.StaticAnnotation

case class id(idGeneration: IdGeneration = IdGeneration.Auto) extends StaticAnnotation

sealed trait IdGeneration
object IdGeneration {
  case object Auto extends IdGeneration
  case object Manual extends IdGeneration
  case object ManualUUID extends IdGeneration
  case class ManualWithCustomLength(length: Int) extends IdGeneration
  case object ManualWixDataCompatible extends IdGeneration

  case object SomeNewIdGenerationType extends IdGeneration //not in SDL, used to test the possible addition of new types
}
