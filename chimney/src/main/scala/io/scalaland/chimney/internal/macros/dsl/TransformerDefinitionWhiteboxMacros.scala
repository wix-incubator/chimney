package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.macros.TransformerConfigSupport
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerDefinitionWhiteboxMacros(val c: whitebox.Context) extends MacroUtils with TransformerConfigSupport {

  import c.universe._
  import CfgTpes._

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selector: Tree, value: Tree): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConst` is of type: ${weakTypeOf[U]}
           |Type required by '$fieldName' field: ${weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, value)
        .refineConfig(fieldConstT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldConstFImpl[F[+_]](selector: Tree, value: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withFieldConstF($selector, $value)"
  }

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selector: Tree, map: Tree): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputed` returns type: ${weakTypeOf[U]}
           |Type required by '$fieldName' field: ${weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, map)
        .refineConfig(fieldComputedT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldComputedFImpl[F[+_]](selector: Tree, map: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withFieldComputedF($selector, $map)"
  }

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = {

    val fieldNameFromOpt = selectorFrom.extractSelectorFieldNameOpt
    val fieldNameToOpt = selectorTo.extractSelectorFieldNameOpt

    (fieldNameFromOpt, fieldNameToOpt) match {
      case (Some(fieldNameFrom), Some(fieldNameTo)) =>
        c.prefix.tree
          .refineConfig(
            fieldRelabelledT.applyTypeArgs(fieldNameFrom.toSingletonTpe, fieldNameTo.toSingletonTpe, weakTypeOf[C])
          )
      case (Some(_), None) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo")
      case (None, Some(_)) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom")
      case (None, None) =>
        val inv1 = s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom"
        val inv2 = s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo"
        c.abort(c.enclosingPosition, s"Invalid selectors:\n$inv1\n$inv2")
    }
  }

  def withCoproductInstanceImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree): Tree = {
    val To = weakTypeOf[To]
    val Inst = weakTypeOf[Inst]
    val (instType, instSymbol) = if (Inst.typeSymbol.isJavaEnum) {
      val deprecation = "Deprecated! Use `withEnumValue` to transform Java enum or Scala Enumeration".stripMargin
      val Function(List(ValDef(_, _, lhs: TypeTree, _)), _) = f
      // this Java enum support implementation is incomplete and fragile
      // TODO remove after some grace period
      lhs.original match {
        // java enum value in Scala 2.13
        case SingletonTypeTree(Literal(Constant(t: TermSymbol))) =>
          c.warning(c.enclosingPosition, deprecation)
          t.coproductType(Inst) -> t
        // java enum value in Scala 2.12
        case SingletonTypeTree(Select(t, n)) if t.isTerm =>
          c.warning(c.enclosingPosition, deprecation)
          Inst.companion.decls.filter(_.name == n).map(s => s.coproductType(Inst) -> s).head
        case _ =>
          /*
          this case is needed, when `withCoproductInstance` is used together with the complete function
          t.withCoproductInstance[MyEnum] {
            case MyEnum.A => ???
            case MyEnum.B => ???
            case _        => ???
          }
           */
          Inst -> Inst.typeSymbol
      }
    } else {
      Inst -> Inst.typeSymbol
    }

    c.prefix.tree
      .addInstance(instSymbol.fullName.toString, To.typeSymbol.fullName.toString, f)
      .refineConfig(coproductInstanceT.applyTypeArgs(instType, To, weakTypeOf[C]))
  }

  def withCoproductInstanceImpl2[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag
  ](from: Tree, to: Tree): Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val (fromType, fromSym) = from match {
      case JavaEnumSupport.Literal(tpe, sym)    => tpe -> sym
      case EnumerationSupport.Literal(tpe, sym) => tpe -> sym
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"The first argument must be a Java enum or Scala enumeration literal of type $From"
        )
    }
    // `addInstance` machinery expects From => To, lift by-name parameter to function
    val f = q"(_: $From) => $to"
    c.prefix.tree
      .addInstance(fromSym.fullName, To.typeSymbol.fullName, f)
      .refineConfig(coproductInstanceT.applyTypeArgs(fromType, To, weakTypeOf[C]))
  }

  def withCoproductInstanceFImpl[
      F[+_],
      From: WeakTypeTag,
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withCoproductInstanceF($f)"
  }

}
