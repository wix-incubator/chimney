package io.scalaland.chimney.internal.wix

sealed class WixException(sourceTypeName: String, targetTypeName: String, msg: String)
    extends Exception(
      s"Can't map $sourceTypeName to $targetTypeName. " + msg
    )

case class CoproductInstanceNotFoundException(sourceTypeName: String, targetTypeName: String)
    extends WixException(
      sourceTypeName,
      targetTypeName,
      "No corresponding instance in the target enum."
    )

sealed case class SdlIdNotProvidedException(sourceTypeName: String, targetTypeName: String)
    extends WixException(
      sourceTypeName,
      targetTypeName,
      s"Source `id` field is None, but should be provided, as specified by the target @id annotation."
    )
