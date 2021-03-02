package io.scalaland.chimney.internal

case class EnumUnrecognizedInstanceException(sourceTypeName: String, targetTypeName: String) extends Exception(
  s"mapping for 'Unrecognized' $sourceTypeName proto enum instance is abscent in $targetTypeName subtypes"
)
