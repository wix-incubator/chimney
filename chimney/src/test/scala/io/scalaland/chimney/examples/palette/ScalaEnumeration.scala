package io.scalaland.chimney.examples.palette

object ScalaEnumeration {
  object Rgb extends Enumeration {
    val Red, Green, Blue = Value
  }

  object Rgbb extends Enumeration {
    val Red, Green, Blue, Black = Value
  }

  object FancyRgbb extends Enumeration {
    val SalmonRed, SeawaveGreen, SkyBlue, JetBlack = Value
  }

  object RgbUpper extends Enumeration {
    val RED, GREEN, BLUE = Value
  }

  object FancyRgbbUpper extends Enumeration {
    val SALMON_RED, SEAWAVE_GREEN, SKY_BLUE, JET_BLACK = Value
  }
}
