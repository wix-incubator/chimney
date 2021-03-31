package com.wixpress.infra.sdl.api.id //important, checked by the macro guards

import scala.annotation.StaticAnnotation


case class id(length: IdLength = UUIDCompatible, idGeneration: IdGeneration = IdGeneration.Auto) extends StaticAnnotation

sealed trait IdLength
case object UUIDCompatible extends IdLength
case object WixDataCompatible extends IdLength

sealed trait IdGeneration
object IdGeneration {
  case object Auto extends IdGeneration
  case object Manual extends IdGeneration

  case object SomeNewIdGenerationType extends IdGeneration //not in SDL, used to test the possible addition of new types
}

