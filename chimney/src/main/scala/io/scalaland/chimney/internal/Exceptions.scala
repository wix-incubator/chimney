package io.scalaland.chimney.internal


case class EnumUnrecognizedInstanceException(sourceTypeName: String, targetTypeName: String) extends Exception(
  s"mapping for 'Unrecognized' $sourceTypeName proto enum instance is absent in $targetTypeName subtypes"
)

case class OneofEmptyCaseException(sourceTypeName: String, targetTypeName: String) extends Exception(
  s"mapping for 'Empty' $sourceTypeName proto enum instance is absent in $targetTypeName subtypes"
)
