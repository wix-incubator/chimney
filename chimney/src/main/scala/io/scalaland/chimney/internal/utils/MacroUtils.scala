package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox

trait MacroUtils extends CompanionUtils {

  val c: blackbox.Context

  import c.universe._

  implicit class NameOps(n: Name) {
    def toNameConstant: Constant = Constant(n.decodedName.toString)
    def toNameLiteral: Literal = Literal(toNameConstant)
    def toSingletonTpe: ConstantType = c.internal.constantType(toNameConstant)
    def toCanonicalSingletonTpe: ConstantType = c.internal.constantType(Constant(n.toCanonicalName))

    /**
      * Canonical representation of a name, allowing matches of names across domains with different naming conventions
      */
    def toCanonicalName: String = n.toString.filterNot(_ == '_').trim().toLowerCase()
  }

  type TypeConstructorTag[F[_]] = WeakTypeTag[F[Unit]]

  object TypeConstructorTag {
    def apply[F[_]: TypeConstructorTag]: Type = {
      weakTypeOf[F[Unit]].typeConstructor
    }
  }

  implicit class TypeOps(t: Type) {

    def applyTypeArg(arg: Type): Type = {
      val ee = t.etaExpand
      if (ee.typeParams.size != 1) {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, s"Type $ee must have single type parameter!")
        // $COVERAGE-ON$
      }
      ee.finalResultType.substituteTypes(ee.typeParams, List(arg))
    }

    def applyTypeArgs(args: Type*): Type = {
      val ee = t.etaExpand
      if (ee.typeParams.size != args.size) {
        // $COVERAGE-OFF$
        val een = ee.typeParams.size
        val argsn = args.size
        c.abort(c.enclosingPosition, s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
        // $COVERAGE-ON$
      }
      ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
    }

    def isValueClass: Boolean =
      t <:< typeOf[AnyVal] && !primitives.exists(_ =:= t)

    def isEnumeration: Boolean =
      t.typeSymbol.fullName == "scala.Enumeration.Value"

    def isCaseClass: Boolean =
      t.typeSymbol.isCaseClass

    def isSealedClass: Boolean =
      t.typeSymbol.classSymbolOpt.exists(_.isSealed)

    def caseClassParams: Seq[MethodSymbol] = {
      t.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
          m.asMethod
      }.toSeq
    }

    def getterMethods: Seq[MethodSymbol] = {
      t.decls.collect {
        case m: MethodSymbol if m.isPublic && (m.isGetter || m.isParameterless) =>
          m
      }.toSeq
    }

    def beanSetterMethods: Seq[MethodSymbol] = {
      t.members.collect { case m: MethodSymbol if m.isBeanSetter => m }.toSeq
    }

    def valueClassMember: Option[MethodSymbol] = {
      t.decls.collectFirst {
        case m: MethodSymbol if m.isParamAccessor =>
          m.asMethod
      }
    }

    def singletonString: String = {
      t.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
        .value
        .value
        .asInstanceOf[String]
    }

    def sealedMembers: Map[String, List[Symbol]] =
      if (t.typeSymbol.isJavaEnum) {
        t.companion.decls
          .filter(_.isJavaEnum)
          .toList
          .groupBy(_.name.toCanonicalName)
      } else if (t.isEnumeration) {
        val TypeRef(t1, _, _) = t
        t1.decls
          .collect {
            case term: TermSymbol if term.isVal => Map(term.name.toCanonicalName -> List(term))
          }
          .foldLeft(Map.empty[String, List[Symbol]])(_ ++ _)
      } else {
        t.typeSymbol.classSymbolOpt.get.subclasses
          .map(_.coproductType(t).typeSymbol)
          .groupBy(_.name.toCanonicalName)
      }

    def collectionInnerTpe: Type = {
      t.typeArgs match {
        case List(unaryInnerT) => unaryInnerT
        case List(innerT1, innerT2) =>
          c.typecheck(tq"($innerT1, $innerT2)", c.TYPEmode).tpe
        // $COVERAGE-OFF$
        case Nil =>
          c.abort(c.enclosingPosition, "Collection type must have type parameters!")
        case _ =>
          c.abort(c.enclosingPosition, "Collection types with more than 2 type arguments are not supported!")
        // $COVERAGE-ON$
      }
    }

    /**
      * Dual to [[SymbolOps.coproductType()]]
      *
      * @return a symbol that refers to a co-product instance described by the type `t`
      */
    def coproductSymbol: Symbol = t match {
      case JavaEnumSupport.CoproductType(sym)    => sym
      case EnumerationSupport.CoproductType(sym) => sym
      case _                                     => t.typeSymbol // assume sealed hierarchy
    }

    def fullNameWithTypeArgs: String = {
      val typeArgsStr = if (t.typeArgs.nonEmpty) t.typeArgs.mkString("[", ", ", "]") else ""
      t.typeSymbol.fullName + typeArgsStr
    }
  }

  implicit class SymbolOps(s: Symbol) {

    def classSymbolOpt: Option[ClassSymbol] =
      if (s.isClass) Some(s.asClass) else None

    def isCaseClass: Boolean =
      classSymbolOpt.exists(_.isCaseClass)

    lazy val caseClassDefaults: Map[String, Tree] = {
      s.typeSignature
      classSymbolOpt
        .flatMap { classSymbol =>
          val classType = classSymbol.toType
          val companionSym = companionSymbol(classType)
          val primaryFactoryMethod = companionSym.asModule.info.decl(TermName("apply")).alternatives.lastOption
          primaryFactoryMethod.foreach(_.asMethod.typeSignature)
          val primaryConstructor = classSymbol.primaryConstructor
          val headParamListOpt = primaryConstructor.asMethod.typeSignature.paramLists.headOption.map(_.map(_.asTerm))

          headParamListOpt.map { headParamList =>
            headParamList.zipWithIndex.flatMap {
              case (param, idx) =>
                if (param.isParamWithDefault) {
                  val method = TermName("apply$default$" + (idx + 1))
                  Some(param.name.toString -> q"$companionSym.$method")
                } else {
                  None
                }
            }.toMap
          }
        }
        .getOrElse {
          // $COVERAGE-OFF$
          Map.empty
          // $COVERAGE-ON$
        }
    }

    def caseClassConstructorAnnotationsByParamName: Map[String, Seq[Annotation]] =
      classSymbolOpt
        .map { classSymbol =>
          classSymbol.primaryConstructor.typeSignature.paramLists.flatten
            .map(s => s.name.toString -> s.annotations.toSeq)
            .toMap
        }
        .getOrElse(Map.empty)

    /**
      * Dual to [[TypeOps.coproductSymbol]]
      * @param parentTpe type of the coproduct parent
      * @return a `Type` that uniquely represents a coproduct instance referenced by this symbol
      */
    def coproductType(parentTpe: Type): Type = s match {
      case JavaEnumSupport.CoproductSymbol(tpe)    => tpe
      case EnumerationSupport.CoproductSymbol(tpe) => tpe
      case _ => // assume sealed hierarchy
        val sEta = s.asType.toType.etaExpand
        sEta.finalResultType.substituteTypes(
          sEta.baseType(parentTpe.typeSymbol).typeArgs.map(_.typeSymbol),
          parentTpe.typeArgs
        )
    }
  }

  implicit class MethodSymbolOps(ms: MethodSymbol) {

    def canonicalName: String = {
      val name = ms.name.decodedName.toString
      if (isBeanSetter) {
        val stripedPrefix = name.drop(3)
        val lowerizedName = stripedPrefix.toCharArray
        lowerizedName(0) = lowerizedName(0).toLower
        new String(lowerizedName)
      } else {
        name
      }
    }

    def isBeanSetter: Boolean = {
      ms.isPublic &&
      ms.name.decodedName.toString.startsWith("set") &&
      ms.name.decodedName.toString.lengthCompare(3) > 0 &&
      ms.paramLists.lengthCompare(1) == 0 &&
      ms.paramLists.head.lengthCompare(1) == 0 &&
      ms.returnType == typeOf[Unit]
    }

    def resultTypeIn(site: Type): Type = {
      ms.typeSignatureIn(site).finalResultType
    }

    def beanSetterParamTypeIn(site: Type): Type = {
      ms.paramLists.head.head.typeSignatureIn(site)
    }

    def isParameterless: Boolean = {
      ms.paramLists.isEmpty || ms.paramLists == List(List())
    }
  }

  implicit class ClassSymbolOps(cs: ClassSymbol) {

    def subclasses: List[Symbol] =
      cs.knownDirectSubclasses.toList.flatMap { subclass =>
        val asClass = subclass.asClass
        if (asClass.isTrait && asClass.isSealed) {
          asClass.subclasses
        } else {
          List(subclass)
        }
      }
  }

  // $COVERAGE-OFF$
  implicit class TreeOps(t: Tree) {

    def debug: Tree = {
      println("TREE: " + t)
      println("RAW:  " + showRaw(t))
      t
    }

    def extractBlock: (List[Tree], Tree) = t match {
      case Typed(tt, _) =>
        tt.extractBlock
      case Block(stats, expr) =>
        (stats, expr)
      case other =>
        (Nil, other)
    }

    def extractStats: List[Tree] = t match {
      case Typed(tt, _) =>
        tt.extractStats
      case Block(stats, _) =>
        stats
      case _ =>
        Nil
    }

    def insertToBlock(tree: Tree): Tree = {
      val (stats, expr) = t.extractBlock
      Block(stats :+ tree, expr)
    }

    def extractSelectorFieldName: Name = {
      extractSelectorFieldNameOpt.getOrElse {
        c.abort(c.enclosingPosition, "Invalid selector!")
      }
    }

    def extractSelectorFieldNameOpt: Option[Name] = {
      t match {
        case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>
          Some(fieldName)
        case _ =>
          None
      }
    }

    def convertCollection(TargetTpe: Type, InnerTpe: Type): Tree = {
      if (TargetTpe <:< typeOf[scala.collection.Map[_, _]] && scala.util.Properties.versionNumberString < "2.13") {
        q"$t.toMap"
      } else {
        q"$t.to(_root_.scala.Predef.implicitly[_root_.scala.collection.compat.Factory[$InnerTpe, $TargetTpe]])"
      }
    }

    def callTransform(input: Tree): Tree = {
      q"$t.transform($input)"
    }

    def getRenames: c.Expr[Map[String, String]] = {
      c.Expr(q"$t.renames")
    }
  }
  // $COVERAGE-ON$

  implicit class TransformerDefinitionTreeOps(td: Tree) {

    def accessConst(name: String, targetTpe: Type): Tree = {
      q"""
        $td
          .overrides($name)
          .asInstanceOf[$targetTpe]
      """
    }

    def accessComputed(name: String, srcPrefixTree: Tree, fromTpe: Type, targetTpe: Type): Tree = {
      q"""
        $td
          .overrides($name)
          .asInstanceOf[$fromTpe => $targetTpe]
          .apply($srcPrefixTree)
      """
    }

    def addOverride(fieldName: Name, value: Tree): Tree = {
      q"$td.__addOverride(${fieldName.toNameLiteral}, $value)"
    }

    def addInstance(fullInstName: String, fullTargetName: String, f: Tree): Tree = {
      q"$td.__addInstance($fullInstName, $fullTargetName, $f)"
    }

    def refineConfig(cfgTpe: Type): Tree = {
      q"$td.__refineConfig[$cfgTpe]"
    }

    def refineTransformerDefinition(definitionRefinementFn: Tree) = {
      q"$td.__refineTransformerDefinition($definitionRefinementFn)"
    }

    def refineTransformerDefinition_Hack(
        definitionRefinementFn: Map[String, Tree] => Tree,
        valTree: (String, Tree)
    ): Tree = {
      // normally, we would like to use refineTransformerDefinition, which works well on Scala 2.11
      // in few cases on Scala 2.12+ it ends up as 'Error while emitting XXX.scala' compiler error
      // with this hack, we can work around scalac bugs

      val (name, tree) = valTree
      val fnTermName = TermName(c.freshName(name))
      val fnMapTree = Map(name -> Ident(fnTermName))
      q"""
        {
          val ${fnTermName} = $tree
          $td.__refineTransformerDefinition(${definitionRefinementFn(fnMapTree)})
        }
      """
    }
  }

  private val primitives = Set(
    typeOf[Double],
    typeOf[Float],
    typeOf[Short],
    typeOf[Byte],
    typeOf[Int],
    typeOf[Long],
    typeOf[Char],
    typeOf[Boolean],
    typeOf[Unit]
  )

  /**
    * Co-product instances encoded as [[Enumeration]] are all visible as a parent type (type does not bear any information
    * about the instance). To overcome this, [[Enumeration]] is stored as [[ConstantType]] carrying a fully-qualified
    * name of the instance (f.ex. `com.wix.MyEnum.Instance1`).
    *
    * This object holds functions to convert between that *synthetic* type and a real [[TermSymbol]] referring to an
    * instance in [[Enumeration]]
    */
  object EnumerationSupport {
    val valueSuffixLength = ".Value".length

    object Literal {
      def unapply(t: Tree): Option[(Type, Symbol)] = t match {
        case s: Select if s.symbol.typeSignature.isEnumeration => Some(CoproductType(t) -> s.symbol)
        case _                                                 => None
      }
    }

    object CoproductType {
      def unapply(t: Type): Option[Symbol] = t match {
        case c.universe.ConstantType(Constant(se: String)) => Some(EnumerationSupport.CoproductSymbol(se))
        case _                                             => None
      }

      def apply(literal: Tree): Type = c.internal.constantType(Constant(literal.toString()))

      def apply(sym: Symbol): Type = {
        val parentName = sym.typeSignature.toString.dropRight(valueSuffixLength)
        c.internal.constantType(Constant(s"$parentName.${sym.name}"))
      }
    }

    object CoproductSymbol {
      def unapply(sym: Symbol): Option[Type] = if (sym.typeSignature.isEnumeration) Some(CoproductType(sym)) else None

      def apply(s: String): Symbol = {
        val tokens = s.split('.')
        val tpeName = tokens.dropRight(1).mkString(".")
        val termName = tokens.last
        val tpe = c.mirror.staticModule(tpeName).typeSignature
        tpe.decls.find {
          case term: TermSymbol => term.isVal && term.name.toString.trim() == termName.trim()
          case _                => false
        }.get
      }
    }
  }

  /**
    * Co-product instances encoded as Java enum are all visible as a parent type (type does not bear any information
    * about the instance). To overcome this, java enum is stored as [[ConstantType]] carrying a [[TermSymbol]]
    * referring to the specific instance.
    *
    * This object holds functions to convert between that *synthetic* type and a real [[TermSymbol]] referring to an
    * instance in Java enum
    */
  object JavaEnumSupport {
    object Literal {
      def unapply(t: Tree): Option[(Type, Symbol)] = t match {
        case c.universe.Literal(const @ Constant(sym: TermSymbol)) if sym.isJavaEnum =>
          Some(CoproductType(const) -> sym)
        case _ => None
      }
    }

    object CoproductType {
      def unapply(t: Type): Option[Symbol] = t match {
        case c.universe.ConstantType(Constant(sym: TermSymbol)) => Some(sym)
        case _                                                  => None
      }

      def apply(const: Constant): Type = c.internal.constantType(const)
      def apply(sym: Symbol): Type = c.internal.constantType(Constant(sym))
    }

    object CoproductSymbol {
      def unapply(sym: Symbol): Option[Type] = if (sym.isJavaEnum) Some(CoproductType(sym)) else None
    }
  }

}
