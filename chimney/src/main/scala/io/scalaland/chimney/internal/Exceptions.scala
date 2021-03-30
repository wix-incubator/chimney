package io.scalaland.chimney.internal

case class CoproductInstanceNotFoundException(sourceTypeName: String, targetTypeName: String)
    extends Exception(
      s"Can't map $sourceTypeName to $targetTypeName. No corresponding instance in the target enum."
    )
