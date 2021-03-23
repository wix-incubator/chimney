package io.scalaland.chimney.examples.wix

package colors4 {
  sealed trait Color
  case object RED extends Color
  case object GREEN extends Color
  case object BLUE extends Color
  case object BLACK extends Color
}

package colorsUpperUndescore {
  sealed trait Color
  case object BLOODY_RED extends Color
  case object DARK_GREEN extends Color
  case object SKY_BLUE extends Color
  case object SNOW_WHITE extends Color
}

package colorsUpperCamel {
  sealed trait Color
  case object BloodyRed extends Color
  case object DarkGreen extends Color
  case object SkyBlue extends Color
  case object SnowWhite extends Color
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

package colors5 {
  object Color extends Enumeration {
    type Color = Value
    val Red, Green, Blue, Black = Value
  }
}

package colors6 {
  object Color extends Enumeration {
    type Color = Value
    val Red, Green, Blue, Black = Value
  }

  object ColorUpper extends Enumeration {
    type ColorUpper = Value
    val RED, GREEN, BLUE, BLACK = Value
  }

  object ColorUpperUnderscore extends Enumeration {
    type ColorUpperUnderscore = Value
    val BLOODY_RED, DARK_GREEN, SKY_BLUE, SNOW_WHITE = Value
  }

  object ColorUpperCamel extends Enumeration {
    type ColorUpperCamel = Value
    val BloodyRed, DarkGreen, SkyBlue, SnowWhite = Value
  }
}
