package io.scalaland.chimney.examples

package colors1 {
  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
}

package colors2 {
  sealed trait Color
  case object Blue extends Color
  case object Green extends Color
  case object Red extends Color
  case object Black extends Color
}

package colors3 {
  sealed trait Color
  sealed trait SimpleColor extends Color
  sealed trait ComplexedColor extends Color
  case object Red extends SimpleColor
  case object Green extends SimpleColor
  case object Blue extends SimpleColor
  case object Black extends ComplexedColor
}

package colorsnested1 {
  package scalapb {
    trait GeneratedOneof
  }
  sealed trait Color extends scalapb.GeneratedOneof
  case class Red(value: RedInfo) extends Color
  case class Green(value: GreenInfo) extends Color
  case class Blue(value: BlueInfo) extends Color
  case object Empty extends Color

  case class RedInfo(code: String, name: String)
  case class GreenInfo(code: String)
  case class BlueInfo(code: String)
}

package colorsnested2 {
  sealed trait Color
  case class Red(code: String, name: String) extends Color
  case class Green(code: String) extends Color
  case class Blue(code: String) extends Color
}

package colors4 {
  sealed trait Color
  case object RED extends Color
  case object GREEN extends Color
  case object BLUE extends Color
  case object BLACK extends Color
}
