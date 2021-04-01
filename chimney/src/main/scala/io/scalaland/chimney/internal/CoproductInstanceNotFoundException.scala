package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.wix.WixException

case class CoproductInstanceNotFoundException(sourceTypeName: String, targetTypeName: String)
  extends WixException(
    sourceTypeName,
    targetTypeName,
    "No corresponding instance in the target enum."
  )
