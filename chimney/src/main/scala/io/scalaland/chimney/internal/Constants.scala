package io.scalaland.chimney.internal

import java.util.UUID

object Constants {
  //proto enum
  val ScalaPBEnumUnrecognizedInstanceName = "Unrecognized"
  val ScalaPBGeneratedEnumBaseClassName = "scalapb.GeneratedEnum"

  //proto oneof
  val ScalaPBOneofEmptyInstanceName = "Empty"
  val ScalaPBGeneratedOneofBaseClassName = "scalapb.GeneratedOneof"

  //SDL @id
  val SdlIdAnnotationFullName = "com.wixpress.infra.sdl.api.id.id"
  val SdlIdGenerationTypeName = "IdGeneration"
  val SdlIdGenerationManual = "Manual"
  val SdlIdGenerationAuto = "Auto"
  val SdlMissingIdPlaceholderString = "ID_WILL_BE_GENERATED_BY_SDL"
  val SdlMissingIdPlaceholderUUID = "00000000-0000-0000-0000-000000000000"
}
