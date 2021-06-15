package io.scalaland.chimney

import io.scalaland.chimney.examples.palette.{rgb, JavaEnum => je, ScalaEnumeration => se, _}
import utest._

import scala.util.control.NoStackTrace

/**
  * Test-cases for transformation between co-products encoded using different techniques
  */
object TransformCoproductSpec extends TestSuite {

  val tests = Tests {

    val ValueNotMappedError = new RuntimeException("expected") with NoStackTrace

    "from scala enumeration" - {
      "to scala enumeration" - {
        "derive ignoring case" - {
          val t = Transformer.derive[se.Rgb.Value, se.RgbUpper.Value]

          t.transform(se.Rgb.Red) ==> se.RgbUpper.RED
          t.transform(se.Rgb.Green) ==> se.RgbUpper.GREEN
          t.transform(se.Rgb.Blue) ==> se.RgbUpper.BLUE
        }

        "derive to smaller domain FAILs" - {
          compileError("Transformer.derive[se.Rgbb.Value, se.Rgb.Value]")
            .check(
              "",
              "Chimney can't derive transformation from io.scalaland.chimney.examples.palette.ScalaEnumeration.Rgbb.Value"
            )
        }

        "derive to smaller domain with customization" - {
          val t = Transformer
            .define[se.Rgbb.Value, se.Rgb.Value]
            .withEnumValue(se.Rgbb.Black, throw ValueNotMappedError)
            .buildTransformer

          t.transform(se.Rgbb.Red) ==> se.Rgb.Red
          t.transform(se.Rgbb.Green) ==> se.Rgb.Green
          t.transform(se.Rgbb.Blue) ==> se.Rgb.Blue
          intercept[RuntimeException] {
            t.transform(se.Rgbb.Black)
          } ==> ValueNotMappedError
        }

        "derive to same domain with override" - {
          val t = Transformer
            .define[se.Rgb.Value, se.RgbUpper.Value]
            .withEnumValue(se.Rgb.Blue, se.RgbUpper.GREEN)
            .buildTransformer

          t.transform(se.Rgb.Red) ==> se.RgbUpper.RED
          t.transform(se.Rgb.Green) ==> se.RgbUpper.GREEN
          // override takes precedence over derivation
          t.transform(se.Rgb.Blue) ==> se.RgbUpper.GREEN
        }

        "`withCoproductInstance` per type" - {
          val t = Transformer
            .define[se.Rgbb.Value, se.FancyRgbb.Value]
            .withCoproductInstance[se.Rgbb.Value] {
              case se.Rgbb.Red   => se.FancyRgbb.SalmonRed
              case se.Rgbb.Green => se.FancyRgbb.SeawaveGreen
              case se.Rgbb.Blue  => se.FancyRgbb.SkyBlue
              // do not map black
            }
            .buildTransformer

          t.transform(se.Rgbb.Red) ==> se.FancyRgbb.SalmonRed
          t.transform(se.Rgbb.Green) ==> se.FancyRgbb.SeawaveGreen
          t.transform(se.Rgbb.Blue) ==> se.FancyRgbb.SkyBlue
          val me = intercept[MatchError] {
            t.transform(se.Rgbb.Black)
          }
          assert(me.getMessage().contains("Black"))
        }
      }

      "to java enum" - {
        "derive" - {
          val t = Transformer.derive[se.Rgb.Value, je.Rgb]

          t.transform(se.Rgb.Red) ==> je.Rgb.Red
          t.transform(se.Rgb.Green) ==> je.Rgb.Green
          t.transform(se.Rgb.Blue) ==> je.Rgb.Blue
        }
      }

      "to case object" - {
        "derive" - {
          val t = Transformer.derive[se.Rgb.Value, rgb.Color]

          t.transform(se.Rgb.Red) ==> rgb.Red
          t.transform(se.Rgb.Green) ==> rgb.Green
          t.transform(se.Rgb.Blue) ==> rgb.Blue
        }
      }
    }

    "from java enum" - {
      "to java enum" - {
        "derive" - {
          val t = Transformer.derive[je.Rgb, je.RgbUpper]

          t.transform(je.Rgb.Red) ==> je.RgbUpper.RED
          t.transform(je.Rgb.Green) ==> je.RgbUpper.GREEN
          t.transform(je.Rgb.Blue) ==> je.RgbUpper.BLUE
        }
        "derive to smaller domain should FAIL" - {
          val t = Transformer
            .define[je.Rgbb, je.Rgb]
            .withEnumValue(je.Rgbb.Black, throw ValueNotMappedError)
            .buildTransformer

          t.transform(je.Rgbb.Red) ==> je.Rgb.Red
          t.transform(je.Rgbb.Green) ==> je.Rgb.Green
          t.transform(je.Rgbb.Blue) ==> je.Rgb.Blue
          intercept[RuntimeException] {
            t.transform(je.Rgbb.Black)
          } ==> ValueNotMappedError
        }
        "`withCoproductInstance` per type" - {
          val t = Transformer
            .define[je.Rgb, je.FancyRgbb]
            .withCoproductInstance[je.Rgb] {
              case je.Rgb.Red   => je.FancyRgbb.SalmonRed
              case je.Rgb.Green => je.FancyRgbb.SeawaveGreen
              case je.Rgb.Blue  => je.FancyRgbb.SkyBlue
            }
            .buildTransformer
          t.transform(je.Rgb.Red) ==> je.FancyRgbb.SalmonRed
          t.transform(je.Rgb.Green) ==> je.FancyRgbb.SeawaveGreen
          t.transform(je.Rgb.Blue) ==> je.FancyRgbb.SkyBlue
        }
        "`withEnumValue`" - {
          val t = Transformer
            .define[je.Rgb, je.FancyRgbb]
            .withEnumValue(je.Rgb.Red, je.FancyRgbb.SalmonRed)
            .withEnumValue(je.Rgb.Green, je.FancyRgbb.SeawaveGreen)
            .withEnumValue(je.Rgb.Blue, je.FancyRgbb.SkyBlue)
            .buildTransformer
          t.transform(je.Rgb.Red) ==> je.FancyRgbb.SalmonRed
          t.transform(je.Rgb.Green) ==> je.FancyRgbb.SeawaveGreen
          t.transform(je.Rgb.Blue) ==> je.FancyRgbb.SkyBlue
        }
      }
      "to scala enum" - {
        "derive" - {
          val t = Transformer.derive[je.Rgb, se.Rgb.Value]

          t.transform(je.Rgb.Red) ==> se.Rgb.Red
          t.transform(je.Rgb.Green) ==> se.Rgb.Green
          t.transform(je.Rgb.Blue) ==> se.Rgb.Blue
        }
      }
      "to sealed hierarchy" - {
        "derive" - {
          val t = Transformer.derive[je.Rgb, rgb.Color]

          t.transform(je.Rgb.Blue) ==> rgb.Blue
          t.transform(je.Rgb.Green) ==> rgb.Green
          t.transform(je.Rgb.Red) ==> rgb.Red
        }
      }
    }

    "from sealed hierarchy" - {
      "to sealed hierarchy" - {
        "derive using canonical name" - {
          val t1 = Transformer.derive[fancy_rgbb.Color, fancy_rgbb_upper.Color]
          t1.transform(fancy_rgbb.SalmonRed) ==> fancy_rgbb_upper.SALMON_RED
          t1.transform(fancy_rgbb.SeawaveGreen) ==> fancy_rgbb_upper.SEAWAVE_GREEN
          t1.transform(fancy_rgbb.SkyBlue) ==> fancy_rgbb_upper.SKY_BLUE
          t1.transform(fancy_rgbb.JetBlack) ==> fancy_rgbb_upper.JET_BLACK

          val t2 = Transformer.derive[fancy_rgbb_upper.Color, fancy_rgbb.Color]
          t2.transform(fancy_rgbb_upper.SALMON_RED) ==> fancy_rgbb.SalmonRed
          t2.transform(fancy_rgbb_upper.SEAWAVE_GREEN) ==> fancy_rgbb.SeawaveGreen
          t2.transform(fancy_rgbb_upper.SKY_BLUE) ==> fancy_rgbb.SkyBlue
          t2.transform(fancy_rgbb_upper.JET_BLACK) ==> fancy_rgbb.JetBlack
        }
      }
      "to java enum" - {
        "derive" - {
          val t = Transformer.derive[fancy_rgbb.Color, je.FancyRgbb]

          t.transform(fancy_rgbb.SalmonRed) ==> je.FancyRgbb.SalmonRed
          t.transform(fancy_rgbb.SeawaveGreen) ==> je.FancyRgbb.SeawaveGreen
          t.transform(fancy_rgbb.SkyBlue) ==> je.FancyRgbb.SkyBlue
          t.transform(fancy_rgbb.JetBlack) ==> je.FancyRgbb.JetBlack
        }
      }
      "to scala enumeration" - {
        "derive" - {
          val t = Transformer.derive[fancy_rgbb.Color, se.FancyRgbb.Value]

          t.transform(fancy_rgbb.SalmonRed) ==> se.FancyRgbb.SalmonRed
          t.transform(fancy_rgbb.SeawaveGreen) ==> se.FancyRgbb.SeawaveGreen
          t.transform(fancy_rgbb.SkyBlue) ==> se.FancyRgbb.SkyBlue
          t.transform(fancy_rgbb.JetBlack) ==> se.FancyRgbb.JetBlack
        }
      }
    }
  }
}
