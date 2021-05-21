package io.scalaland.chimney

import java.util.UUID

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.wix.JavaColors.{Colors, ColorsUpperCase}
import io.scalaland.chimney.examples.wix._
import io.scalaland.chimney.internal.Constants._
import io.scalaland.chimney.internal.wix.{CoproductInstanceNotFoundException, SdlIdNotProvidedException}
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}
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
      case class EntityDTO(id: Option[String])
      case class Entity(id: String, otherField: Int)

      case class CustomException(message: String) extends Exception
      val mapper: Throwable => Throwable = e => CustomException(e.getMessage)

      val transformerInto = new TransformerInto[EntityDTO, Entity, TransformerCfg.Empty, TransformerFlags.Enable[
        TransformerFlags.UnsafeOption,
        TransformerFlags.Default
      ]](
        EntityDTO(None),
        new TransformerDefinition(Map.empty, Map.empty),
        mapper
      ).enableUnsafeOption //to trigger an exception
        .withFieldConst(_.otherField, 42) //to test that builder copies the mapper

      intercept[CustomException](transformerInto.transform)
    }

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
        val ex = intercept[CoproductInstanceNotFoundException](
          (colorsnested1.Empty: colorsnested1.Color).transformInto[colorsnested2.Color]
        )

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
      "allow `withCoproductInstance` with java enum values" - {
        implicit val t: Transformer[JavaColors.Colors, richcolors.RichColor] =
          Transformer
            .define[JavaColors.Colors, richcolors.RichColor]
            .withCoproductInstance { _: JavaColors.Colors.Black.type =>
              richcolors.JetBlack
            }
            .withCoproductInstance { _: JavaColors.Colors.Red.type =>
              richcolors.SalmonRed
            }
            .withCoproductInstance { _: JavaColors.Colors.Green.type =>
              richcolors.SeawaveGreen
            }
            .withCoproductInstance { _: JavaColors.Colors.Blue.type =>
              richcolors.SkyBlue
            }
            .buildTransformer
        t.transform(JavaColors.Colors.Black) ==> richcolors.JetBlack
        t.transform(JavaColors.Colors.Red) ==> richcolors.SalmonRed
        t.transform(JavaColors.Colors.Green) ==> richcolors.SeawaveGreen
        t.transform(JavaColors.Colors.Blue) ==> richcolors.SkyBlue
      }

      "allow `withCoproductInstance` with java enum type and total function" - {
        implicit val t: Transformer[JavaColors.Colors, richcolors.RichColor] =
          Transformer
            .define[JavaColors.Colors, richcolors.RichColor]
            .withCoproductInstance[JavaColors.Colors] {
              case JavaColors.Colors.Black => richcolors.JetBlack
              case JavaColors.Colors.Red   => richcolors.SalmonRed
              case JavaColors.Colors.Green => richcolors.SeawaveGreen
              case JavaColors.Colors.Blue  => richcolors.SkyBlue
            }
            .buildTransformer
        t.transform(JavaColors.Colors.Black) ==> richcolors.JetBlack
        t.transform(JavaColors.Colors.Red) ==> richcolors.SalmonRed
        t.transform(JavaColors.Colors.Green) ==> richcolors.SeawaveGreen
        t.transform(JavaColors.Colors.Blue) ==> richcolors.SkyBlue
      }

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
            Transformer
              .define[JavaNumbers.NumScaleUppercase, JavaNumbers.NumScale]
              .withCoproductInstance { _: JavaNumbers.NumScaleUppercase.TRILLION.type =>
                JavaNumbers.NumScale.Zero
              }
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
              .withCoproductInstance { _: Colors.Green.type =>
                colors2.Red
              }
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

    "support SDL @id annotation" - {
      import com.wixpress.infra.sdl.api.id._
      case class EntityDTO(id: Option[String])

      "use placeholder if id is None (IdGeneration.Auto)" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Auto) id: String)

        EntityDTO(None).transformInto[Entity] ==> Entity(SdlMissingIdPlaceholderString)
        EntityDTO(None).into[Entity].transform ==> Entity(SdlMissingIdPlaceholderString)
      }

      "use provided value if id is Some (IdGeneration.Auto)" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Auto) id: String)
        val str = "some_value"

        EntityDTO(Some(str)).transformInto[Entity] ==> Entity(str)
        EntityDTO(Some(str)).into[Entity].transform ==> Entity(str)
      }

      "throw an exception if id is None (IdGeneration.Manual)" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Manual) id: String)

        intercept[SdlIdNotProvidedException] { EntityDTO(None).transformInto[Entity] }
        intercept[SdlIdNotProvidedException] { EntityDTO(None).into[Entity].transform }
      }

      "use provided value if id is Some (IdGeneration.Manual)" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Manual) id: String)
        val str = "some_value"

        EntityDTO(Some(str)).transformInto[Entity] ==> Entity(str)
        EntityDTO(Some(str)).into[Entity].transform ==> Entity(str)
      }

      "use placeholder if id is None (default IdGeneration)" - {
        case class Entity(@id id: String)

        EntityDTO(None).transformInto[Entity] ==> Entity(SdlMissingIdPlaceholderString)
        EntityDTO(None).into[Entity].transform ==> Entity(SdlMissingIdPlaceholderString)
      }

      "fail compilation if id is None (some new IdGeneration type)" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.SomeNewIdGenerationType) id: String)

        compileError("EntityDTO(None).transformInto[Entity]")
          .check(
            "",
            "derivation from entitydto.id: scala.Option[String] to java.lang.String is not supported in Chimney"
          )
      }

      "use custom transformer (withFieldComputed)" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Auto) id: String)
        val customValue = "custom_placeholder_value"

        implicit val t: Transformer[EntityDTO, Entity] = Transformer
          .define[EntityDTO, Entity]
          .withFieldComputed(_.id, _.id.getOrElse(customValue))
          .buildTransformer

        EntityDTO(None).transformInto[Entity] ==> Entity(customValue)
      }

      "use expansion rules on custom transformer if there is no ID transformation rule" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Auto) id: String, otherValue: String)

        implicit val t: Transformer[EntityDTO, Entity] = Transformer
          .define[EntityDTO, Entity]
          .withFieldConst(_.otherValue, "default")
          .buildTransformer

        EntityDTO(None).transformInto[Entity] ==> Entity(SdlMissingIdPlaceholderString, "default")
      }

      "support UUID placeholder" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Auto) id: UUID)

        implicit val t: Transformer[String, UUID] = str => UUID.fromString(str)

        EntityDTO(None).transformInto[Entity] ==> Entity(UUID.fromString(SdlMissingIdPlaceholderUUID))
        EntityDTO(None).into[Entity].transform ==> Entity(UUID.fromString(SdlMissingIdPlaceholderUUID))
      }

      "support UUID exception" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Manual) id: UUID)

        implicit val t: Transformer[String, UUID] = str => UUID.fromString(str)

        intercept[SdlIdNotProvidedException] { EntityDTO(None).transformInto[Entity] }
        intercept[SdlIdNotProvidedException] { EntityDTO(None).into[Entity].transform }
      }

      "fail compilation if no UUID transformer in scope" - {
        case class Entity(@id(UUIDCompatible, IdGeneration.Auto) id: UUID)

        compileError("EntityDTO(None).transformInto[Entity]")
          .check(
            "",
            "java.lang.String to java.util.UUID is not supported in Chimney"
          )
      }
    }

    "transformer should contain field renames specified using withFieldRenamed" - {
      "empty renames" - {
        case class A(name: String, age: Int)
        case class B(name: String, age: Int)

        implicitly[Transformer[A, B]].renames ==> Map.empty
      }

      "if top level fields renamed" - {
        case class A(aName: String, aAge: Int)
        case class B(bName: String, bAge: Int)

        val t = Transformer
          .define[A, B]
          .withFieldRenamed(_.aName, _.bName)
          .withFieldRenamed(_.aAge, _.bAge)
          .buildTransformer

        t.renames ==> Map(
          "aName" -> "bName",
          "aAge" -> "bAge"
        )
      }

      "if nested fields renamed" - {
        case class A(info1: AInfo, info2: AInfo)
        case class AInfo(aName: String, aAge: Int)

        case class B(info1: BInfo, info2: BInfo)
        case class BInfo(bName: String, bAge: Int)

        implicit val t = Transformer
          .define[AInfo, BInfo]
          .withFieldRenamed(_.aName, _.bName)
          .withFieldRenamed(_.aAge, _.bAge)
          .buildTransformer

        implicitly[Transformer[A, B]].renames ==> Map(
          "info1.aName" -> "info1.bName",
          "info1.aAge" -> "info1.bAge",
          "info2.aName" -> "info2.bName",
          "info2.aAge" -> "info2.bAge"
        )
      }

      "if 2-level nested fields renamed" - {
        case class A(info1: AInfoWrapper, info2: AInfoWrapper)
        case class AInfoWrapper(info: AInfo)
        case class AInfo(aName: String, aAge: Int)

        case class B(info1: BInfoWrapper, info2: BInfoWrapper)
        case class BInfoWrapper(info: BInfo)
        case class BInfo(bName: String, bAge: Int)

        implicit val t = Transformer
          .define[AInfo, BInfo]
          .withFieldRenamed(_.aName, _.bName)
          .withFieldRenamed(_.aAge, _.bAge)
          .buildTransformer

        implicitly[Transformer[A, B]].renames ==> Map(
          "info1.info.aName" -> "info1.info.bName",
          "info1.info.aAge" -> "info1.info.bAge",
          "info2.info.aName" -> "info2.info.bName",
          "info2.info.aAge" -> "info2.info.bAge"
        )
      }

      "if both top level and nested fields renamed" - {
        case class A(aInfo1: AInfo, aInfo2: AInfo)
        case class AInfo(aName: String, aAge: Int)

        case class B(bInfo1: BInfo, bInfo2: BInfo)
        case class BInfo(bName: String, bAge: Int)

        implicit val tInfo = Transformer
          .define[AInfo, BInfo]
          .withFieldRenamed(_.aName, _.bName)
          .withFieldRenamed(_.aAge, _.bAge)
          .buildTransformer

        implicit val t = Transformer
          .define[A, B]
          .withFieldRenamed(_.aInfo1, _.bInfo1)
          .withFieldRenamed(_.aInfo2, _.bInfo2)
          .buildTransformer

        t.renames ==> Map(
          "aInfo1" -> "bInfo1",
          "aInfo1.aName" -> "bInfo1.bName",
          "aInfo1.aAge" -> "bInfo1.bAge",
          "aInfo2" -> "bInfo2",
          "aInfo2.aName" -> "bInfo2.bName",
          "aInfo2.aAge" -> "bInfo2.bAge"
        )
      }

      "if nested wrapped fields renamed" - {
        case class A(info1: Option[AInfo], info2: AInfo, info3: Option[AInfo])
        case class AInfo(aName: String, aAge: Int)

        case class B(info1: Option[BInfo], info2: Option[BInfo], info3: BInfo)
        case class BInfo(bName: String, bAge: Int)

        implicit val nestedTransformer = Transformer
          .define[AInfo, BInfo]
          .withFieldRenamed(_.aName, _.bName)
          .withFieldRenamed(_.aAge, _.bAge)
          .buildTransformer

        val t = Transformer.define[A, B].enableUnsafeOption.buildTransformer

        t.renames ==> Map(
          "info1.aName" -> "info1.bName",
          "info1.aAge" -> "info1.bAge",
          "info2.aName" -> "info2.bName",
          "info2.aAge" -> "info2.bAge",
          "info3.aName" -> "info3.bName",
          "info3.aAge" -> "info3.bAge"
        )
      }

      "if array fields renamed" - {
        case class A(info1: Seq[AInfo], info2: Seq[AInfo])
        case class AInfo(aName: String, aAge: Int)

        case class B(info1: Seq[BInfo], info2: Seq[BInfo])
        case class BInfo(bName: String, bAge: Int)

        implicit val nestedTransformer = Transformer
          .define[AInfo, BInfo]
          .withFieldRenamed(_.aName, _.bName)
          .withFieldRenamed(_.aAge, _.bAge)
          .buildTransformer

        val t = Transformer.define[A, B].enableUnsafeOption.buildTransformer

        t.renames ==> Map(
          "info1.aName" -> "info1.bName",
          "info1.aAge" -> "info1.bAge",
          "info2.aName" -> "info2.bName",
          "info2.aAge" -> "info2.bAge"
        )
      }
    }
  }
}
