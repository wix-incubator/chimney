package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.wix._
import JavaColors.{Colors, ColorsUpperCase}
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
        (pb.addressbook.PhoneType.HOME: pb.addressbook.PhoneType)
          .transformInto[addressbook.PhoneType] ==> addressbook.HOME

        val ex = intercept[CoproductInstanceNotFoundException](
          (pb.addressbook.PhoneType.Unrecognized(1): pb.addressbook.PhoneType).transformInto[addressbook.PhoneType]
        )
        ex.sourceTypeName ==> "io.scalaland.chimney.examples.pb.addressbook.PhoneType.Unrecognized"
        ex.targetTypeName ==> "io.scalaland.chimney.examples.addressbook.PhoneType"
      }
    }

    "TransformerInto allows to override exceptions" - {
      case class CustomException(message: String) extends Exception
      val mapper: Throwable => Throwable = e => CustomException(e.getMessage)

      val source: Option[String] = None

      val transformerInto = new TransformerInto[Option[String], String, TransformerCfg.Empty, TransformerFlags.Enable[
        TransformerFlags.UnsafeOption,
        TransformerFlags.Default
      ]](
        source,
        new TransformerDefinition(Map.empty, Map.empty),
        mapper
      ).enableUnsafeOption

      intercept[CustomException](transformerInto.transform)
    }

    "support scalapb-generated proto oneof" - {
      "oneof -> sealed trait family" - {
        val redCode   = "dc143c"
        val redName   = "crimson"
        val greenCode = "00ff00"
        val blueCode  = "0000ff"

        (colorsnested1.Red(colorsnested1.RedInfo(redCode, redName)): colorsnested1.Color)
          .transformInto[colorsnested2.Color] ==> colorsnested2.Red(redCode, redName)
        (colorsnested1.Green(colorsnested1.GreenInfo(greenCode)): colorsnested1.Color)
          .transformInto[colorsnested2.Color] ==> colorsnested2.Green(greenCode)
        (colorsnested1.Blue(colorsnested1.BlueInfo(blueCode)): colorsnested1.Color)
          .transformInto[colorsnested2.Color] ==> colorsnested2.Blue(blueCode)
      }

      "throw an exception if oneof Empty -> sealed trait family without Empty" - {
        val ex = intercept[CoproductInstanceNotFoundException](
          (colorsnested1.Empty: colorsnested1.Color).transformInto[colorsnested2.Color]
        )

        ex.sourceTypeName ==> colorsnested1.Empty.getClass.getName.stripSuffix("$")
        ex.targetTypeName ==> classOf[colorsnested2.Color].getName
      }

      "sealed trait family -> oneof" - {
        val redCode   = "dc143c"
        val redName   = "crimson"
        val greenCode = "00ff00"
        val blueCode  = "0000ff"

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
        (colorsUpperUndescore.BLOODY_RED: colorsUpperUndescore.Color)
          .transformInto[colorsUpperCamel.Color] ==> colorsUpperCamel.BloodyRed
        (colorsUpperUndescore.DARK_GREEN: colorsUpperUndescore.Color)
          .transformInto[colorsUpperCamel.Color] ==> colorsUpperCamel.DarkGreen

        (colorsUpperCamel.SkyBlue: colorsUpperCamel.Color)
          .transformInto[colorsUpperUndescore.Color] ==> colorsUpperUndescore.SKY_BLUE
        (colorsUpperCamel.SnowWhite: colorsUpperCamel.Color)
          .transformInto[colorsUpperUndescore.Color] ==> colorsUpperUndescore.SNOW_WHITE
      }
    }

    "support java enum" - {
      "transform java enum into java enum" - {
        "by canonical name" - {
          implicit def t[A]: Transformer[JavaNumbers.NumScaleUppercase, JavaNumbers.NumScale] =
            Transformer.define.buildTransformer

          (JavaNumbers.NumScaleUppercase.ZERO: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Zero
          (JavaNumbers.NumScaleUppercase.MILLION: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Million
          (JavaNumbers.NumScaleUppercase.BILLION: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Billion
          (JavaNumbers.NumScaleUppercase.TRILLION: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Trillion
        }
        "with customization" - {
          implicit def t[A]: Transformer[JavaNumbers.NumScaleUppercase, JavaNumbers.NumScale] =
            Transformer.define
              .withCoproductInstance(JavaNumbers.NumScaleUppercase.TRILLION, JavaNumbers.NumScale.Zero)
              .buildTransformer

          (JavaNumbers.NumScaleUppercase.ZERO: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Zero
          (JavaNumbers.NumScaleUppercase.MILLION: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Million
          (JavaNumbers.NumScaleUppercase.BILLION: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Billion
          (JavaNumbers.NumScaleUppercase.TRILLION: JavaNumbers.NumScaleUppercase)
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Zero
        }
      }

      "transform java enum into sealed hierachy" - {
        "by exact name" - {
          implicit val t: Transformer[Colors, colors2.Color] = Transformer.define.buildTransformer

          (JavaColors.Colors.Black: Colors).transformInto[colors2.Color] ==> colors2.Black
          (JavaColors.Colors.Blue: Colors).transformInto[colors2.Color] ==> colors2.Blue
          (JavaColors.Colors.Green: Colors).transformInto[colors2.Color] ==> colors2.Green
          (JavaColors.Colors.Red: Colors).transformInto[colors2.Color] ==> colors2.Red
        }

        "by canonical name" - {
          implicit val t: Transformer[ColorsUpperCase, colors2.Color] = Transformer.define.buildTransformer

          (JavaColors.ColorsUpperCase.BLACK: ColorsUpperCase).transformInto[colors2.Color] ==> colors2.Black
          (JavaColors.ColorsUpperCase.BLUE: ColorsUpperCase).transformInto[colors2.Color] ==> colors2.Blue
          (JavaColors.ColorsUpperCase.GREEN: ColorsUpperCase).transformInto[colors2.Color] ==> colors2.Green
          (JavaColors.ColorsUpperCase.RED: ColorsUpperCase).transformInto[colors2.Color] ==> colors2.Red
        }

        "with customization" - {
          implicit val t: Transformer[Colors, colors2.Color] =
            Transformer
              .define[Colors, colors2.Color]
              .withCoproductInstance(Colors.Green, colors2.Red)
              .buildTransformer

          (JavaColors.Colors.Black: Colors).transformInto[colors2.Color] ==> colors2.Black
          (JavaColors.Colors.Blue: Colors).transformInto[colors2.Color] ==> colors2.Blue
          (JavaColors.Colors.Green: Colors).transformInto[colors2.Color] ==> colors2.Red
          (JavaColors.Colors.Red: Colors).transformInto[colors2.Color] ==> colors2.Red
        }
      }

      "transform sealed hierarchy into java enum" - {
        "objects with customizations" - {
          implicit def t[A]: Transformer[numbers.long.NumScale[A], JavaNumbers.NumScale] =
            Transformer
              .define[numbers.long.NumScale[A], JavaNumbers.NumScale]
              .withCoproductInstance((_: numbers.long.Milliard[A]) => JavaNumbers.NumScale.Zero)
              .withCoproductInstance((_: numbers.long.Billiard[A]) => JavaNumbers.NumScale.Zero)
              .buildTransformer

          (numbers.long.Zero: numbers.long.NumScale[Nothing])
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Zero
          (numbers.long.Milliard(42): numbers.long.NumScale[Int])
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Zero
          (numbers.long.Billiard(42): numbers.long.NumScale[Int])
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Zero
          (numbers.long.Million(42): numbers.long.NumScale[Int])
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Million
          (numbers.long.Billion(42): numbers.long.NumScale[Int])
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Billion
          (numbers.long.Trillion(42): numbers.long.NumScale[Int])
            .transformInto[JavaNumbers.NumScale] ==> JavaNumbers.NumScale.Trillion
        }
      }
    }
  }
}
