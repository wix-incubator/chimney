package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.wix._
import io.scalaland.chimney.examples.wix.JavaEnums
import io.scalaland.chimney.internal.{CoproductInstanceNotFoundException, TransformerCfg, TransformerFlags}
import utest._

/**
  * Specification for changes introduced by Wix
  *
  * Features added by Wix engineers should go here or to a dedicated files to simplify merging and to help tracking
  * changes in Wix-owned fork
  */
object WixSpec extends TestSuite {

  val tests = Tests {

    "PBTranformation" - {
      "handle transformation of proto with 'Unrecognized' instance properly" - {
        (pb.addressbook.PhoneType.HOME: pb.addressbook.PhoneType).transformInto[addressbook.PhoneType] ==> addressbook.HOME

        val ex = intercept[CoproductInstanceNotFoundException](
          (pb.addressbook.PhoneType.Unrecognized(1): pb.addressbook.PhoneType).transformInto[addressbook.PhoneType]
        )
        ex.sourceTypeName ==> "io.scalaland.chimney.examples.pb.addressbook.PhoneType.Unrecognized"
        ex.targetTypeName ==> "io.scalaland.chimney.examples.addressbook.PhoneType"
      }
    }

    "Dsl" - {
      "support scalapb-generated proto oneof" - {
        "oneof -> sealed trait family" - {
          val redCode = "dc143c"
          val redName = "crimson"
          val greenCode = "00ff00"
          val blueCode = "0000ff"

          (colorsnested1.Red(colorsnested1.RedInfo(redCode, redName)): colorsnested1.Color)
            .transformInto[colorsnested2.Color] ==> colorsnested2.Red(redCode, redName)
          (colorsnested1.Green(colorsnested1.GreenInfo(greenCode)): colorsnested1.Color)
            .transformInto[colorsnested2.Color] ==> colorsnested2.Green(greenCode)
          (colorsnested1.Blue(colorsnested1.BlueInfo(blueCode)): colorsnested1.Color)
            .transformInto[colorsnested2.Color] ==> colorsnested2.Blue(blueCode)
        }

        "throw an exception if oneof Empty -> sealed trait family without Empty" - {
          val ex = intercept[CoproductInstanceNotFoundException]((colorsnested1.Empty: colorsnested1.Color).transformInto[colorsnested2.Color])

          ex.sourceTypeName ==> colorsnested1.Empty.getClass.getName.stripSuffix("$")
          ex.targetTypeName ==> classOf[colorsnested2.Color].getName
        }

        "sealed trait family -> oneof" - {
          val redCode = "dc143c"
          val redName = "crimson"
          val greenCode = "00ff00"
          val blueCode = "0000ff"

          (colorsnested2.Red(redCode, redName): colorsnested2.Color)
            .transformInto[colorsnested1.Color] ==> colorsnested1.Red(colorsnested1.RedInfo(redCode, redName))
          (colorsnested2.Green(greenCode): colorsnested2.Color)
            .transformInto[colorsnested1.Color] ==> colorsnested1.Green(colorsnested1.GreenInfo(greenCode))
          (colorsnested2.Blue(blueCode): colorsnested2.Color)
            .transformInto[colorsnested1.Color] ==> colorsnested1.Blue(colorsnested1.BlueInfo(blueCode))
        }
      }

      "support sealed hierarchies" - {
        "transforming enum ignoring case" - {
          (colors2.Red: colors2.Color).transformInto[colors4.Color] ==> colors4.RED
          (colors2.Black: colors2.Color).transformInto[colors4.Color] ==> colors4.BLACK

          (colors4.BLUE: colors4.Color).transformInto[colors2.Color] ==> colors2.Blue
          (colors4.GREEN: colors4.Color).transformInto[colors2.Color] ==> colors2.Green
        }

        "transforming enum ignoring underscore" - {
          (colorsUpperUndescore.BLOODY_RED: colorsUpperUndescore.Color).transformInto[colorsUpperCamel.Color] ==> colorsUpperCamel.BloodyRed
          (colorsUpperUndescore.DARK_GREEN: colorsUpperUndescore.Color).transformInto[colorsUpperCamel.Color] ==> colorsUpperCamel.DarkGreen

          (colorsUpperCamel.SkyBlue: colorsUpperCamel.Color).transformInto[colorsUpperUndescore.Color] ==> colorsUpperUndescore.SKY_BLUE
          (colorsUpperCamel.SnowWhite: colorsUpperCamel.Color).transformInto[colorsUpperUndescore.Color] ==> colorsUpperUndescore.SNOW_WHITE
        }
      }

      "supports scala enumerations and java enums" - {
        "transforming enumeration into java enum" - {
          (colors5.Color.Black: colors5.Color.Color).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Black
          (colors5.Color.Blue: colors5.Color.Color).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Blue
          (colors5.Color.Green: colors5.Color.Color).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Green
          (colors5.Color.Red: colors5.Color.Color).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Red
        }

        "transforming java enum into java enum" - {
          (JavaEnums.JavaColorsUpperCase.BLACK: JavaEnums.JavaColorsUpperCase).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Black
          (JavaEnums.JavaColorsUpperCase.BLUE: JavaEnums.JavaColorsUpperCase).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Blue
          (JavaEnums.JavaColorsUpperCase.GREEN: JavaEnums.JavaColorsUpperCase).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Green
          (JavaEnums.JavaColorsUpperCase.RED: JavaEnums.JavaColorsUpperCase).transformInto[JavaEnums.JavaColors] ==> JavaEnums.JavaColors.Red
        }

        "transforming java enum into enumeration" - {
          (JavaEnums.JavaColors.Black: JavaEnums.JavaColors).transformInto[colors5.Color.Color] ==> colors5.Color.Black
          (JavaEnums.JavaColors.Blue: JavaEnums.JavaColors).transformInto[colors5.Color.Color] ==> colors5.Color.Blue
          (JavaEnums.JavaColors.Green: JavaEnums.JavaColors).transformInto[colors5.Color.Color] ==> colors5.Color.Green
          (JavaEnums.JavaColors.Red: JavaEnums.JavaColors).transformInto[colors5.Color.Color] ==> colors5.Color.Red
        }

        "transforming enumeration into enumeration" - {
          (colors5.Color.Red: colors5.Color.Color).transformInto[colors6.Color.Color] ==> colors6.Color.Red
          (colors5.Color.Black: colors5.Color.Color).transformInto[colors6.Color.Color] ==> colors6.Color.Black
          (colors5.Color.Blue: colors5.Color.Color).transformInto[colors6.Color.Color] ==> colors6.Color.Blue
        }

        "transforming enumeration into enumeration ignoring case" - {
          (colors5.Color.Red: colors5.Color.Color).transformInto[colors6.ColorUpper.ColorUpper] ==> colors6.ColorUpper.RED
          (colors5.Color.Green: colors5.Color.Color).transformInto[colors6.ColorUpper.ColorUpper] ==> colors6.ColorUpper.GREEN
          (colors5.Color.Blue: colors5.Color.Color).transformInto[colors6.ColorUpper.ColorUpper] ==> colors6.ColorUpper.BLUE
        }

        "transforming enumeration into enumeration ignoring underscore" - {
          (colors6.ColorUpperCamel.BloodyRed: colors6.ColorUpperCamel.ColorUpperCamel)
            .transformInto[colors6.ColorUpperUnderscore.ColorUpperUnderscore] ==> colors6.ColorUpperUnderscore.BLOODY_RED
          (colors6.ColorUpperCamel.DarkGreen: colors6.ColorUpperCamel.ColorUpperCamel)
            .transformInto[colors6.ColorUpperUnderscore.ColorUpperUnderscore] ==> colors6.ColorUpperUnderscore.DARK_GREEN

          (colors6.ColorUpperUnderscore.SKY_BLUE: colors6.ColorUpperUnderscore.ColorUpperUnderscore)
            .transformInto[colors6.ColorUpperCamel.ColorUpperCamel] ==> colors6.ColorUpperCamel.SkyBlue
          (colors6.ColorUpperUnderscore.SNOW_WHITE: colors6.ColorUpperUnderscore.ColorUpperUnderscore)
            .transformInto[colors6.ColorUpperCamel.ColorUpperCamel] ==> colors6.ColorUpperCamel.SnowWhite
        }

        "transforming enumeration into STF" - {
          (colors5.Color.Blue: colors5.Color.Color).transformInto[colors4.Color] ==> colors4.BLUE
          (colors5.Color.Red: colors5.Color.Color).transformInto[colors4.Color] ==> colors4.RED
          (colors5.Color.Black: colors5.Color.Color).transformInto[colors4.Color] ==> colors4.BLACK
        }

        "transforming STF into enumeration" - {
          (colors4.BLACK: colors4.Color).transformInto[colors5.Color.Color] ==> colors5.Color.Black
          (colors4.GREEN: colors4.Color).transformInto[colors5.Color.Color] ==> colors5.Color.Green
          (colors4.BLUE: colors4.Color).transformInto[colors5.Color.Color] ==> colors5.Color.Blue
        }

        "transforming enum <-> STF ignoring underscore" - {
          (colorsUpperUndescore.BLOODY_RED: colorsUpperUndescore.Color).transformInto[colors6.ColorUpperCamel.ColorUpperCamel] ==> colors6.ColorUpperCamel.BloodyRed
          (colorsUpperUndescore.SNOW_WHITE: colorsUpperUndescore.Color).transformInto[colors6.ColorUpperCamel.ColorUpperCamel] ==> colors6.ColorUpperCamel.SnowWhite

          (colors6.ColorUpperCamel.SkyBlue: colors6.ColorUpperCamel.ColorUpperCamel).transformInto[colorsUpperUndescore.Color] ==> colorsUpperUndescore.SKY_BLUE
          (colors6.ColorUpperCamel.DarkGreen: colors6.ColorUpperCamel.ColorUpperCamel).transformInto[colorsUpperUndescore.Color] ==> colorsUpperUndescore.DARK_GREEN
        }
      }
    }

    "TransformerInto allows to override exceptions" - {
      case class CustomException(message: String) extends Exception
      val mapper: Throwable => Throwable = e => CustomException(e.getMessage)

      val source: Option[String] = None

      val transformerInto = new TransformerInto[
        Option[String],
        String,
        TransformerCfg.Empty,
        TransformerFlags.Enable[TransformerFlags.UnsafeOption, TransformerFlags.Default]](
        source,
        new TransformerDefinition(Map.empty, Map.empty),
        mapper
      ).enableUnsafeOption

      intercept[CustomException](transformerInto.transform)
    }
  }
}

