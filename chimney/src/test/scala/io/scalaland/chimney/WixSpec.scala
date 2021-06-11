package io.scalaland.chimney

import java.util.UUID

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.palette.{colorsnested1, _}
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
          (colorsnested1.Empty: colorsnested1.Color).transformInto[rgbb_upper.Color]
        )

        ex.sourceTypeName ==> colorsnested1.Empty.getClass.getName.stripSuffix("$")
        ex.targetTypeName ==> "io.scalaland.chimney.examples.palette.rgbb_upper.Color"
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
