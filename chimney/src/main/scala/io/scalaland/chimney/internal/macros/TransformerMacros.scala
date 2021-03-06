package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import Constants._

import scala.reflect.macros.blackbox

trait TransformerMacros extends TransformerConfigSupport with MappingMacros with TargetConstructorMacros {
  this: DerivationGuards with MacroUtils with EitherUtils =>

  val c: blackbox.Context

  import c.universe._

  def buildDefinedTransformer[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](
      tfsTree: Tree = EmptyTree,
      wrapperType: Option[Type] = None
  ): Tree = {
    val config = readConfig[C, InstanceFlags, ScopeFlags](tfsTree).copy(
      definitionScope = Some((weakTypeOf[From], weakTypeOf[To])),
      wrapperErrorPathSupportInstance = findTransformerErrorPathSupport(wrapperType)
    )

    if (!config.valueLevelAccessNeeded) {
      genTransformer[From, To](config)
    } else {
      val tdName = TermName(c.freshName("td"))
      val derivedTransformer = genTransformer[From, To](config.copy(transformerDefinitionPrefix = q"$tdName"))

      q"""
        val $tdName = ${c.prefix.tree}
        $derivedTransformer
      """
    }
  }

  def expandTransform[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](
      tcTree: Tree,
      tfsTree: Tree = EmptyTree,
      wrapperType: Option[Type] = None
  ): Tree = {
    val tiName = TermName(c.freshName("ti"))

    val config = readConfig[C, InstanceFlags, ScopeFlags](tfsTree).copy(
      transformerDefinitionPrefix = q"$tiName.td",
      wrapperErrorPathSupportInstance = findTransformerErrorPathSupport(wrapperType)
    )

    val derivedTransformerTree = genTransformer[From, To](config)

    q"""
       val _ = $tcTree // hack to avoid unused warnings
       val $tiName = ${c.prefix.tree}
       _root_.scala.util.Try(${derivedTransformerTree.callTransform(q"$tiName.source")})
         .recover { case e: Throwable => throw $tiName.exceptionMapper(e) }
         .get
    """
  }

  def genTransformer[From: WeakTypeTag, To: WeakTypeTag](
      config: TransformerConfig
  ): Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName = freshTermName(From)

    genTransformerTree(srcName, config)(From, To) match {

      case Right(transformerTree) =>
        config.wrapperType match {
          case Some(f) =>
            q"""
               new _root_.io.scalaland.chimney.TransformerF[$f, $From, $To] {
                 def transform($srcName: $From): ${f.applyTypeArg(To)} = {
                   ${transformerTree.tree}
                 }
               }
            """

          case None =>
            q"""
               new _root_.io.scalaland.chimney.Transformer[$From, $To] {
                 def transform($srcName: $From): $To = {
                   ${transformerTree.tree}
                 }

                 override def renames: Map[String, String] = ${transformerTree.renamesMapTree}
               }
            """
          /* Renames field macro might produce the "Generated class io.scalaland.chimney.DslSpec$$anon$143 differs only in case
               from io.scalaland.chimney.DslSpec$$anon$143. Such classes will overwrite one another on case-insensitive
               filesystems" warnings, which occur if implicitly-converted transformer is used.
               This condition should not be a problem, since these classes are essentially the same one.
               @see Testcase#001. */
        }

      case Left(derivationErrors) =>
        val errorMessage =
          s"""Chimney can't derive transformation from $From to $To
             |
             |${DerivationError.printErrors(derivationErrors)}
             |Consult $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        c.abort(c.enclosingPosition, errorMessage)
    }
  }

  def genTransformerTree(
      srcName: TermName,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    resolveTransformerBody(srcPrefixTree, config)(From, To).map {
      case TransformerBodyTree(tree, false, renames) if config.wrapperType.isDefined =>
        TransformerTree(q"${config.wrapperSupportInstance}.pure[$To]($tree)", renames)
      case TransformerBodyTree(tree, _, renames) => TransformerTree(tree, renames)
    }
  }

  def expandTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type,
      toAnnotations: Option[Seq[Annotation]] = None
  ): Either[Seq[DerivationError], TransformerTree] = {

    resolveImplicitTransformer(config)(From, To)
      .map(localImplicitTree =>
        Right(TransformerTree(localImplicitTree.callTransform(srcPrefixTree), localImplicitTree.getRenames))
      )
      .getOrElse {
        deriveTransformerTree(srcPrefixTree, config)(From, To, toAnnotations)
      }
  }

  def deriveTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type,
      toAnnotations: Option[Seq[Annotation]] = None
  ): Either[Seq[DerivationError], TransformerTree] = {
    lazy val sdlIdAnnotationInfo =
      toAnnotations.flatMap(_.find(isSdlIdAnnotation)).map(extractSdlIdAnnotationParamValues)

    if (isSubtype(From, To)) {
      expandSubtypes(srcPrefixTree, config)
    } else if (fromValueClassToType(From, To)) {
      expandValueClassToType(srcPrefixTree, config)(From, To)
    } else if (fromTypeToValueClass(From, To)) {
      expandTypeToValueClass(srcPrefixTree, config)(From, To)
    } else if (bothOptions(From, To)) {
      expandOptions(srcPrefixTree, config)(From, To)
    } else if (isOption(To)) {
      expandTargetWrappedInOption(srcPrefixTree, config)(From, To)
    } else if (isOptionString(From) && (isString(To) || isUUID(To)) && sdlIdAnnotationInfo.exists(
                 shouldThrowExOnMissingSdlId
               )) {
      expandSourceStringWrappedInOptionWithSdlIdException(srcPrefixTree, config)(From, To)
    } else if (isOptionString(From) && isString(To) && sdlIdAnnotationInfo.exists(shouldUsePlaceholderOnMissingSdlId)) {
      expandSourceStringWrappedInOptionWithSdlIdPlaceholder(srcPrefixTree, config, SdlMissingIdPlaceholderString)(
        From,
        To
      )
    } else if (isOptionString(From) && isUUID(To) && sdlIdAnnotationInfo.exists(shouldUsePlaceholderOnMissingSdlId)) {
      expandSourceStringWrappedInOptionWithSdlIdPlaceholder(srcPrefixTree, config, SdlMissingIdPlaceholderUUID)(
        From,
        To
      )
    } else if (config.flags.unsafeOption && isOption(From)) {
      expandSourceWrappedInOptionUnsafe(srcPrefixTree, config)(From, To)
    } else if (bothEithers(From, To)) {
      expandEithers(srcPrefixTree, config)(From, To).map(TransformerTree(_))
    } else if (isMap(From)) {
      expandFromMap(srcPrefixTree, config)(From, To)
    } else if (bothOfIterableOrArray(From, To)) {
      expandIterableOrArray(srcPrefixTree, config)(From, To)
    } else if (isTuple(To)) {
      expandDestinationTuple(srcPrefixTree, config)(From, To)
    } else if (destinationCaseClass(To)) {
      expandDestinationCaseClass(srcPrefixTree, config)(From, To)
    } else if (config.flags.beanSetters && destinationJavaBean(To)) {
      expandDestinationJavaBean(srcPrefixTree, config)(From, To)
    } else if (bothSealedClasses(From, To)) {
      expandSealedClasses(srcPrefixTree, config)(From, To).map(TransformerTree(_))
    } else {
      notSupportedDerivation(srcPrefixTree, From, To)
    }

  }

  def expandSubtypes(
      srcPrefixTree: Tree,
      config: TransformerConfig
  ): Either[Seq[DerivationError], TransformerTree] = {
    Right {
      TransformerTree(mkTransformerBodyTree0(config)(srcPrefixTree))
    }
  }

  def expandValueClassToType(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type
  ): Either[Seq[DerivationError], TransformerTree] = {

    From.valueClassMember
      .map { member =>
        Right {
          TransformerTree(
            mkTransformerBodyTree0(config) {
              q"$srcPrefixTree.${member.name}"
            }
          )
        }
      }
      .getOrElse {
        // $COVERAGE-OFF$
        Left {
          Seq(CantFindValueClassMember(From.fullNameWithTypeArgs, To.fullNameWithTypeArgs))
        }
        // $COVERAGE-ON$
      }
  }

  def expandTypeToValueClass(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type
  ): Either[Seq[DerivationError], TransformerTree] = {
    Right {
      TransformerTree(
        mkTransformerBodyTree0(config) {
          q"new $To($srcPrefixTree)"
        }
      )
    }
  }

  def expandTargetWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {
    if (To <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val optFrom = c.typecheck(tq"_root_.scala.Option[$From]", c.TYPEmode).tpe
      expandOptions(q"_root_.scala.Option[$From]($srcPrefixTree)", config)(optFrom, To)
    }
  }

  def expandSourceWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      innerSrcPrefix: Tree
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {
    if (From <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fromInnerT = From.typeArgs.head
      resolveRecursiveTransformerBody(innerSrcPrefix, config.rec)(fromInnerT, To)
        .map { innerTransformerBody =>
          val fn = freshTermName(innerSrcPrefix).toString
          TransformerTree(
            mkTransformerBodyTree1(To, Target(fn, To), innerTransformerBody, config) { tree =>
              q"($tree)"
            },
            innerTransformerBody.renamesMapTree
          )
        }
    }
  }

  def expandSourceWrappedInOptionUnsafe(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] =
    expandSourceWrappedInOption(
      srcPrefixTree,
      config,
      q"$srcPrefixTree.get"
    )(From, To)

  def expandSourceStringWrappedInOptionWithSdlIdException(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {
    val fromName = From.fullNameWithTypeArgs
    val toName = To.fullNameWithTypeArgs

    expandSourceWrappedInOption(
      srcPrefixTree,
      config,
      q"$srcPrefixTree.getOrElse(throw _root_.io.scalaland.chimney.internal.wix.SdlIdNotProvidedException($fromName, $toName))"
    )(From, To)
  }

  def expandSourceStringWrappedInOptionWithSdlIdPlaceholder(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      placeholder: String
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] =
    expandSourceWrappedInOption(
      srcPrefixTree,
      config,
      q"$srcPrefixTree.getOrElse($placeholder)"
    )(From, To)

  def expandOptions(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {

    def fromInnerT = From.typeArgs.head
    def toInnerT = To.typeArgs.head

    if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fn = Ident(freshTermName(srcPrefixTree))
      resolveRecursiveTransformerBody(fn, config.rec)(fromInnerT, toInnerT)
        .map {
          case TransformerBodyTree(innerTree, false, renames) =>
            TransformerTree(
              mkTransformerBodyTree0(config) {
                q"$srcPrefixTree.map(($fn: $fromInnerT) => $innerTree)"
              },
              renames
            )

          case TransformerBodyTree(innerTree, true, renames) =>
            TransformerTree(
              q"""
                $srcPrefixTree.fold[${config.wrapperType.get.applyTypeArg(To)}](
                  ${config.wrapperSupportInstance}.pure(Option.empty[$toInnerT])
                )(
                  ($fn: $fromInnerT) => ${config.wrapperSupportInstance}.map($innerTree, Option.apply[$toInnerT])
                )
              """,
              renames
            )
        }
    }
  }

  def expandEithers(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromLeftT, fromRightT) = From.typeArgs
    val List(toLeftT, toRightT) = To.typeArgs

    val fnL = Ident(freshTermName("left"))
    val fnR = Ident(freshTermName("right"))

    if (From <:< leftTpe && !(To <:< rightTpe)) {
      resolveRecursiveTransformerBody(q"$srcPrefixTree.value", config.rec)(fromLeftT, toLeftT)
        .map { tbt =>
          mkTransformerBodyTree1(To, Target(fnL.name.toString, toLeftT), tbt, config) { leftArgTree =>
            q"new _root_.scala.util.Left($leftArgTree)"
          }
        }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      resolveRecursiveTransformerBody(q"$srcPrefixTree.value", config.rec)(fromRightT, toRightT)
        .map { tbt =>
          mkTransformerBodyTree1(To, Target(fnR.name.toString, toRightT), tbt, config) { rightArgTree =>
            q"new _root_.scala.util.Right($rightArgTree)"
          }
        }
    } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
      val leftTransformerE = resolveRecursiveTransformerBody(fnL, config.rec)(fromLeftT, toLeftT)
      val rightTransformerE = resolveRecursiveTransformerBody(fnR, config.rec)(fromRightT, toRightT)

      (leftTransformerE, rightTransformerE) match {
        case (Right(leftTbt), Right(rightTbt)) =>
          val targetTpe = config.wrapperType.map(_.applyTypeArg(To)).getOrElse(To)
          val leftN = freshTermName("left")
          val rightN = freshTermName("right")

          val leftBody = mkTransformerBodyTree1(To, Target(leftN.toString, toLeftT), leftTbt, config) { leftArgTree =>
            q"new _root_.scala.util.Left($leftArgTree)"
          }

          val rightBody = mkTransformerBodyTree1(To, Target(rightN.toString, toRightT), rightTbt, config) {
            rightArgTree =>
              q"new _root_.scala.util.Right($rightArgTree)"
          }

          Right {
            q"""
              $srcPrefixTree.fold[$targetTpe](
                ($fnL: $fromLeftT) => $leftBody,
                ($fnR: $fromRightT) => $rightBody
              )
            """
          }
        case _ =>
          Left(leftTransformerE.left.getOrElse(Nil) ++ rightTransformerE.left.getOrElse(Nil))
      }
    } else {
      notSupportedDerivation(srcPrefixTree, From, To)
    }
  }

  def expandFromMap(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {
    val ToInnerT = To.collectionInnerTpe

    (config.wrapperErrorPathSupportInstance, config.wrapperType, ToInnerT.caseClassParams.map(_.resultTypeIn(ToInnerT))) match {
      case (Some(errorPathSupport), Some(f), List(toKeyT, toValueT)) =>
        val List(fromKeyT, fromValueT) = From.typeArgs

        val fnK = Ident(freshTermName("k"))
        val fnV = Ident(freshTermName("v"))

        val keyTransformerE = resolveRecursiveTransformerBody(fnK, config.rec)(fromKeyT, toKeyT)
        val valueTransformerE = resolveRecursiveTransformerBody(fnV, config.rec)(fromValueT, toValueT)

        (keyTransformerE, valueTransformerE) match {
          case (Right(keyTransformer), Right(valueTransformer)) =>
            val wrapper = config.wrapperSupportInstance
            val WrappedToInnerT = f.applyTypeArg(ToInnerT)

            val keyTransformerWithPath =
              if (keyTransformer.isWrapped)
                TransformerTree(
                  q"""$errorPathSupport.addPath[$toKeyT](
                   ${keyTransformer.tree},
                   _root_.io.scalaland.chimney.ErrorPathNode.MapKey($fnK)
                 )""",
                  keyTransformer.renamesMapTree
                )
              else TransformerTree(q"$wrapper.pure[$toKeyT](${keyTransformer.tree})", keyTransformer.renamesMapTree)

            val valueTransformerWithPath =
              if (valueTransformer.isWrapped)
                TransformerTree(
                  q"""$errorPathSupport.addPath[$toValueT](
                    ${valueTransformer.tree},
                    _root_.io.scalaland.chimney.ErrorPathNode.MapValue($fnK)
                 )""",
                  valueTransformer.renamesMapTree
                )
              else
                TransformerTree(q"$wrapper.pure[$toValueT](${valueTransformer.tree})", valueTransformer.renamesMapTree)

            Right(
              TransformerTree(
                q"""$wrapper.traverse[$To, $WrappedToInnerT, $ToInnerT](
                  $srcPrefixTree.iterator.map[$WrappedToInnerT] {
                    case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
                      $wrapper.product[$toKeyT, $toValueT](
                        ${keyTransformerWithPath.tree},
                        ${valueTransformerWithPath.tree}
                      )
                  },
                  _root_.scala.Predef.identity[$WrappedToInnerT]
                )
             """
              )
            )
          case _ =>
            Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
        }

      case _ => expandIterableOrArray(srcPrefixTree, config)(From, To)
    }
  }

  def expandIterableOrArray(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {

    val FromInnerT = From.collectionInnerTpe
    val ToInnerT = To.collectionInnerTpe

    val fn = Ident(freshTermName(srcPrefixTree))

    resolveRecursiveTransformerBody(fn, config.rec)(FromInnerT, ToInnerT)
      .map {
        case TransformerBodyTree(innerTransformerTree, true, renames) =>
          if (config.wrapperType.isDefined) {
            config.wrapperErrorPathSupportInstance match {
              case Some(errorPathSupport) =>
                val idx = Ident(freshTermName("idx"))
                TransformerTree(
                  q"""${config.wrapperSupportInstance}.traverse[$To, ($FromInnerT, _root_.scala.Int), $ToInnerT](
                    $srcPrefixTree.iterator.zipWithIndex,
                    { case (${fn.name}: $FromInnerT, ${idx.name}: _root_.scala.Int) =>
                      $errorPathSupport.addPath[$ToInnerT](
                        $innerTransformerTree,
                        _root_.io.scalaland.chimney.ErrorPathNode.Index($idx)
                      )
                    }
                   )
                  """,
                  renames
                )
              case None =>
                TransformerTree(
                  q"""${config.wrapperSupportInstance}.traverse[$To, $FromInnerT, $ToInnerT](
                    $srcPrefixTree.iterator,
                    ($fn: $FromInnerT) => $innerTransformerTree
                  )
                  """,
                  renames
                )
            }

          } else {
            // this case is not possible due to resolveRecursiveTransformerBody semantics: it will not
            // even search for lifted inner transformer when wrapper type is not requested
            // $COVERAGE-OFF$
            c.abort(c.enclosingPosition, "Impossible case!")
            // $COVERAGE-ON$
          }

        case TransformerBodyTree(innerTransformerTree, false, renames) =>
          def isTransformationIdentity = fn == innerTransformerTree
          def sameCollectionTypes = From.typeConstructor =:= To.typeConstructor

          val transformedCollectionTree: Tree = (isTransformationIdentity, sameCollectionTypes) match {
            case (true, true) =>
              // identity transformation, same collection types
              srcPrefixTree

            case (true, false) =>
              // identity transformation, different collection types
              srcPrefixTree.convertCollection(To, ToInnerT)

            case (false, true) =>
              // non-trivial transformation, same collection types
              q"$srcPrefixTree.map(($fn: $FromInnerT) => $innerTransformerTree)"

            case (false, false) =>
              q"$srcPrefixTree.iterator.map(($fn: $FromInnerT) => $innerTransformerTree)"
                .convertCollection(To, ToInnerT)
          }

          if (config.wrapperType.isDefined) {
            TransformerTree(q"${config.wrapperSupportInstance}.pure($transformedCollectionTree)", renames)
          } else {
            TransformerTree(transformedCollectionTree, renames)
          }
      }
  }

  def expandSealedClasses(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    resolveCoproductInstance(srcPrefixTree, From, To, config)
      .map { instanceTree =>
        Right(instanceTree)
      }
      .getOrElse {
        val fromInstances = From.sealedMembers
        val toInstances = To.sealedMembers

        val instanceClauses = fromInstances.flatMap {
          case (canonicalName, instSymbols) =>
            instSymbols.map { instSymbol =>
              val instName = instSymbol.name.toString
              val instTpe = instSymbol.coproductType(From)

              // in simple scenarios left-hand side of the case expression is flat (`case _: $type` or `case $value`)
              val flatPatternMatch: Tree =
                (instSymbol, instTpe) match {
                  case CaseClassPattern(patternMatch)  => patternMatch
                  case CaseObjectPattern(patternMatch) => patternMatch
                  case JavaEnumPattern(patternMatch)   => patternMatch
                  case ScalaEnumPattern(patternMatch)  => patternMatch
                  case _                               => c.abort(c.enclosingPosition, "BUG: Can't derive left-hand side of the case expression")
                }

              resolveCoproductInstance(srcPrefixTree, instTpe, To, config)
                .map { instanceTree =>
                  Right(cq"$flatPatternMatch => $instanceTree")
                }
                .getOrElse {
                  toInstances.getOrElse(canonicalName, Nil) match {
                    case List(JavaEnum(symbol))  => Right(cq"$flatPatternMatch => $symbol")
                    case List(ScalaEnum(symbol)) => Right(cq"$flatPatternMatch => ${c.parse(symbol.fullName)}")
                    case List(CaseObject(symbol)) =>
                      Right(cq"$flatPatternMatch => ${mkTransformerBodyTree0(config)(q"${symbol.asClass.module}")}")
                    // `case class → case class` needs recursive derivation
                    case List(CaseClass(symbol)) if instSymbol.isCaseClass =>
                      val fn = freshTermName(instName)
                      val explodedPatternMatch = pq"$fn: $instTpe"
                      expandDestinationCaseClass(Ident(fn), config.rec)(
                        instTpe,
                        symbol.coproductType(To)
                      ).map { innerTransformerTree =>
                        cq"$explodedPatternMatch => ${innerTransformerTree.tree}"
                      }
                    // if there's no target symbol, but the source is special proto enum value, throw an infrastructure exception
                    case Nil
                        if (isScalaPBEnum(instTpe) && instName == ScalaPBEnumUnrecognizedInstanceName && instSymbol.isCaseClass) ||
                          (isScalaPBOneof(instTpe) && instName == ScalaPBOneofEmptyInstanceName && instSymbol.isModuleClass) =>
                      Right(
                        cq"$flatPatternMatch => throw _root_.io.scalaland.chimney.internal.wix.CoproductInstanceNotFoundException(${instSymbol.fullName}, ${To.typeSymbol.fullName})"
                      )
                    case _ :: _ :: _ =>
                      Left {
                        Seq(
                          AmbiguousCoproductInstance(
                            instName,
                            From.fullNameWithTypeArgs,
                            To.fullNameWithTypeArgs
                          )
                        )
                      }
                    case _ =>
                      Left {
                        Seq(
                          CantFindCoproductInstanceTransformer(
                            instSymbol.fullName,
                            From.fullNameWithTypeArgs,
                            To.fullNameWithTypeArgs
                          )
                        )
                      }
                  }
                }
            }
        }.toList

        buildMatchingBlockFromClauses(instanceClauses, srcPrefixTree)
      }
  }

  private def buildMatchingBlockFromClauses(
      instanceClauses: List[Either[Seq[DerivationError], Tree]],
      srcPrefixTree: Tree
  ): Either[List[DerivationError], Tree] = {
    if (instanceClauses.forall(_.isRight)) {
      val clauses = instanceClauses.collect { case Right(clause) => clause }
      Right {
        q"$srcPrefixTree match { case ..$clauses }"
      }
    } else {
      Left {
        instanceClauses.collect { case Left(derivationErrors) => derivationErrors }.flatten
      }
    }
  }

  def resolveCoproductInstance(
      srcPrefixTree: Tree,
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Option[Tree] = {
    val coproductSymbol = From.coproductSymbol
    if (config.wrapperType.isDefined && config.coproductInstancesF.contains((coproductSymbol, To))) {
      Some(
        mkCoproductInstance(
          config.transformerDefinitionPrefix,
          srcPrefixTree,
          coproductSymbol,
          To,
          config.wrapperType
        )
      )
    } else if (config.coproductInstances.contains((coproductSymbol, To))) {
      Some(
        mkTransformerBodyTree0(config) {
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            srcPrefixTree,
            coproductSymbol,
            To,
            None
          )
        }
      )
    } else {
      None
    }
  }

  def expandDestinationTuple(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {

    resolveSourceTupleAccessors(From, To)
      .flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      }
      .map { transformerBodyPerTarget =>
        val targets = To.caseClassParams.map(Target.fromField(_, To))
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

        TransformerTree(
          mkTransformerBodyTree(To, targets, bodyTreeArgs, config) { args =>
            mkNewClass(To, args)
          }
        )
      }
  }

  def expandDestinationCaseClass(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {

    val oneofValueField = TermName("value")
    def getValueField(t: Type) = t.member(oneofValueField).typeSignature
    def hasValueField(t: Type) = t.member(oneofValueField) != NoSymbol

    val targets = {
      val adjustedTo = if (isScalaPBOneof(To) && !hasValueField(From)) getValueField(To) else To
      val annotations = adjustedTo.typeSymbol.caseClassConstructorAnnotationsByParamName

      adjustedTo.caseClassParams.map(p => Target.fromField(p, adjustedTo, annotations.get(p.name.toString)))
    }

    val targetTransformerBodiesMapping = if (isTuple(From)) {
      resolveSourceTupleAccessors(From, To).flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      }
    } else {
      val overridesMapping = resolveOverrides(srcPrefixTree, From, targets, config)
      val notOverridenTargets = targets.diff(overridesMapping.keys.toSeq)

      def mkTransformer(From: Type, srcPrefixTree: Tree, To: Type) = {
        val accessorsMapping = resolveAccessorsMapping(From, notOverridenTargets, config)

        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
          .map(_ ++ overridesMapping)
      }

      /** Proto oneOf to sealed trait enum support
        *  @see https://github.com/wix-private/server-infra/issues/14909
        */
      if (isScalaPBOneof(From) && !hasValueField(To))
        mkTransformer(getValueField(From), q"$srcPrefixTree.value", To)
      else if (isScalaPBOneof(To) && !hasValueField(From))
        mkTransformer(From, srcPrefixTree, getValueField(To))
      else
        mkTransformer(From, srcPrefixTree, To)
    }

    targetTransformerBodiesMapping.map { transformerBodyPerTarget =>
      val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

      val tree = if (isScalaPBOneof(To) && !hasValueField(From)) {
        mkTransformerBodyTree(getValueField(To), targets, bodyTreeArgs, config) { args =>
          mkNewClass(To, Seq(mkNewClass(getValueField(To), args)))
        }
      } else {
        mkTransformerBodyTree(To, targets, bodyTreeArgs, config) { args =>
          mkNewClass(To, args)
        }
      }

      TransformerTree(tree, calculateRenames(transformerBodyPerTarget, config.fieldOverrides))
    }
  }

  def expandDestinationJavaBean(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerTree] = {

    val beanSetters = To.beanSetterMethods
    val targets = beanSetters.map(Target.fromJavaBeanSetter(_, To))

    val accessorsMapping = resolveAccessorsMapping(From, targets, config)

    resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      .map { transformerBodyPerTarget =>
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

        TransformerTree(
          mkTransformerBodyTree(To, targets, bodyTreeArgs, config) { args =>
            mkNewJavaBean(To, targets zip args)
          },
          calculateRenames(transformerBodyPerTarget, config.fieldOverrides)
        )
      }
  }

  def resolveTransformerBodyTreeFromAccessorsMapping(
      srcPrefixTree: Tree,
      accessorsMapping: Map[Target, AccessorResolution],
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Either[Seq[DerivationError], Map[Target, TransformerBodyTree]] = {

    val (erroredTargets, resolvedBodyTrees) = accessorsMapping.map {
      case (target, accessor: AccessorResolution.Resolved) =>
        target -> resolveTransformerBodyTreeFromAccessor(srcPrefixTree, target, accessor, From, config)
      case (target, accessor) =>
        target -> Left(
          Seq(
            MissingAccessor(
              fieldName = target.name,
              fieldTypeName = target.tpe.fullNameWithTypeArgs,
              sourceTypeName = From.fullNameWithTypeArgs,
              targetTypeName = To.fullNameWithTypeArgs,
              defAvailable = accessor == AccessorResolution.DefAvailable
            )
          )
        )
    }.partitionEitherValues

    if (erroredTargets.isEmpty) {
      Right(resolvedBodyTrees)
    } else {
      val targetsToFallback = erroredTargets.collect {
        case (target, _) if !accessorsMapping(target).isResolved => target
      }
      val fallbackTransformerBodies = resolveFallbackTransformerBodies(targetsToFallback, To, config)
      val unresolvedTargets = accessorsMapping.keys.toList
        .diff(resolvedBodyTrees.keys.toList)
        .diff(fallbackTransformerBodies.keys.toList)

      if (unresolvedTargets.isEmpty) {
        Right(resolvedBodyTrees ++ fallbackTransformerBodies)
      } else {
        val errors = unresolvedTargets.flatMap { target =>
          accessorsMapping(target) match {
            case AccessorResolution.Resolved(symbol: MethodSymbol, _) =>
              erroredTargets(target) :+ MissingTransformer(
                fieldName = target.name,
                sourceFieldTypeName = symbol.resultTypeIn(From).fullNameWithTypeArgs,
                targetFieldTypeName = target.tpe.fullNameWithTypeArgs,
                sourceTypeName = From.fullNameWithTypeArgs,
                targetTypeName = To.fullNameWithTypeArgs
              )
            case _ => erroredTargets(target)
          }
        }
        Left(errors)
      }
    }
  }

  def resolveTransformerBodyTreeFromAccessor(
      srcPrefixTree: Tree,
      target: Target,
      accessor: AccessorResolution.Resolved,
      From: Type,
      config: TransformerConfig
  ): Either[Seq[DerivationError], TransformerBodyTree] = {
    val resolved = resolveRecursiveTransformerBody(
      q"$srcPrefixTree.${accessor.symbol.name}",
      config
    )(
      accessor.symbol.resultTypeIn(From),
      target.tpe,
      target.annotations
    )

    (resolved, config.wrapperErrorPathSupportInstance) match {
      case (Right(bodyTree), Some(errorPathSupport)) if bodyTree.isWrapped =>
        Right {
          TransformerBodyTree(
            q"""$errorPathSupport.addPath[${target.tpe}](
                 ${bodyTree.tree},
                 _root_.io.scalaland.chimney.ErrorPathNode.Accessor(${accessor.symbol.name.toString})
               )""",
            isWrapped = true,
            bodyTree.renamesMapTree
          )
        }
      case _ => resolved
    }
  }

  def resolveRecursiveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type,
      toAnnotations: Option[Seq[Annotation]] = None
  ): Either[Seq[DerivationError], TransformerBodyTree] = {
    resolveTransformerBody(srcPrefixTree, config.rec)(From, To, toAnnotations)
  }

  def resolveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type,
      toAnnotations: Option[Seq[Annotation]] = None
  ): Either[Seq[DerivationError], TransformerBodyTree] = {
    if (config.wrapperType.isDefined) {
      val implicitTransformerF = resolveImplicitTransformer(config)(From, To)
      val implicitTransformer = findLocalImplicitTransformer(From, To, None)

      (implicitTransformerF, implicitTransformer) match {
        case (Some(localImplicitTreeF), Some(localImplicitTree)) =>
          c.abort(
            c.enclosingPosition,
            s"""Ambiguous implicits while resolving Chimney recursive transformation:
               |
               |TransformerF[${config.wrapperType.get}, $From, $To]: $localImplicitTreeF
               |Transformer[$From, $To]: $localImplicitTree
               |
               |Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used
               |""".stripMargin
          )
        case (Some(localImplicitTreeF), None) =>
          Right(
            TransformerBodyTree(
              localImplicitTreeF.callTransform(srcPrefixTree),
              isWrapped = true,
              localImplicitTreeF.getRenames
            )
          )
        case (None, Some(localImplicitTree)) =>
          Right(
            TransformerBodyTree(
              localImplicitTree.callTransform(srcPrefixTree),
              isWrapped = false,
              localImplicitTree.getRenames
            )
          )
        case (None, None) =>
          deriveTransformerTree(srcPrefixTree, config)(From, To)
            .map(tree => TransformerBodyTree(tree.tree, isWrapped = true, tree.renamesMapTree))
      }
    } else {
      expandTransformerTree(srcPrefixTree, config)(From, To, toAnnotations)
        .map(tree => TransformerBodyTree(tree.tree, isWrapped = false, tree.renamesMapTree))
    }
  }

  def resolveImplicitTransformer(config: TransformerConfig)(From: Type, To: Type): Option[Tree] = {
    if (config.definitionScope.contains((From, To))) {
      None
    } else {
      findLocalImplicitTransformer(From, To, config.wrapperType)
    }
  }

  def findLocalTransformerConfigurationFlags: Tree = {
    val searchTypeTree =
      tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[_ <: io.scalaland.chimney.internal.TransformerFlags]]}"
    inferImplicitTpe(searchTypeTree, macrosDisabled = true)
      .getOrElse {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, "Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
  }

  private def findLocalImplicitTransformer(From: Type, To: Type, wrapperType: Option[Type]): Option[Tree] = {
    val searchTypeTree: Tree = wrapperType match {
      case Some(f) =>
        tq"_root_.io.scalaland.chimney.TransformerF[$f, $From, $To]"
      case None =>
        tq"_root_.io.scalaland.chimney.Transformer[$From, $To]"
    }

    inferImplicitTpe(searchTypeTree, macrosDisabled = false).filterNot(isDeriving)
  }

  def findTransformerErrorPathSupport(wrapperType: Option[Type]): Option[Tree] = {
    wrapperType.flatMap(tpe =>
      inferImplicitTpe(tq"_root_.io.scalaland.chimney.TransformerFErrorPathSupport[$tpe]", macrosDisabled = true)
    )
  }

  private def inferImplicitTpe(tpeTree: Tree, macrosDisabled: Boolean): Option[Tree] = {
    val typedTpeTree = c.typecheck(
      tree = tpeTree,
      silent = true,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = macrosDisabled
    )

    scala.util
      .Try(c.inferImplicitValue(typedTpeTree.tpe, silent = true, withMacrosDisabled = macrosDisabled))
      .toOption
      .filterNot(_ == EmptyTree)
  }

  private def isDeriving(tree: Tree): Boolean = {
    tree match {
      case TypeApply(Select(qualifier, name), _) =>
        qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.Transformer.type] && name.toString == "derive"
      case Apply(TypeApply(Select(qualifier, name), _), _) =>
        qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.TransformerF.type] && name.toString == "derive"
      case _ => false
    }
  }

  private def notSupportedDerivation(
      srcPrefixTree: Tree,
      fromTpe: Type,
      toTpe: Type
  ): Left[Seq[NotSupportedDerivation], Nothing] =
    Left {
      Seq(
        NotSupportedDerivation(
          toFieldName(srcPrefixTree),
          fromTpe.fullNameWithTypeArgs,
          toTpe.fullNameWithTypeArgs
        )
      )
    }

  private def freshTermName(srcPrefixTree: Tree): c.universe.TermName = {
    freshTermName(toFieldName(srcPrefixTree))
  }

  private def freshTermName(tpe: Type): c.universe.TermName = {
    freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
  }

  private def freshTermName(prefix: String): c.universe.TermName = {
    c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$")
  }

  private def toFieldName(srcPrefixTree: Tree): String = {
    // undo the encoding of freshTermName
    srcPrefixTree
      .toString()
      .replaceAll("\\$\\d+", "")
      .replace("$u002E", ".")
  }

  private def extractSdlIdAnnotationParamValues(annotation: Annotation): Set[String] =
    annotation.tree.children.tail.map(_.tpe.typeSymbol.name.toString).toSet

  private def calculateRenames(
      targets: Map[Target, TransformerBodyTree],
      fieldOverrides: Map[String, FieldOverride]
  ): c.Expr[Map[String, String]] = {
    val trees = targets.map {
      case (target, transformer) =>
        val lookupName = fieldOverrides.get(target.name) match {
          case Some(FieldOverride.RenamedFrom(sourceName)) => sourceName
          case _                                           => target.name
        }

        val nestedRenames =
          q"""_root_.io.scalaland.chimney.TransformerUtils.__addPrefix(
          ${transformer.renamesMapTree},
          $lookupName,
          ${target.name})
        """

        if (lookupName == target.name) nestedRenames else q"$nestedRenames + ($lookupName -> ${target.name})"
    }

    c.Expr(q"Seq(..$trees).flatten.toMap")
  }

  object CaseObject {
    def unapply(sym: Symbol): Option[Symbol] = if (sym.isModuleClass) Some(sym) else None
  }

  object CaseClass {
    def unapply(sym: Symbol): Option[Symbol] = if (sym.isCaseClass) Some(sym) else None
  }

  object ScalaEnum {
    def unapply(sym: Symbol): Option[Symbol] = if (sym.typeSignature.isEnumeration) Some(sym) else None
  }

  object JavaEnum {
    def unapply(sym: Symbol): Option[Symbol] = if (sym.isJavaEnum) Some(sym) else None
  }

  trait SymPatternExtractor {
    def doUnapply(sym: Symbol, typeInSealedParent: Type): Option[Tree]
    def unapply(arg: (Symbol, Type)): Option[Tree] = arg match {
      case (sym: Symbol, typeInSealedParent: Type) => doUnapply(sym, typeInSealedParent)
    }
  }

  object ScalaEnumPattern extends SymPatternExtractor {
    def doUnapply(sym: Symbol, typeInSealedParent: Type): Option[Tree] =
      typeInSealedParent match {
        case c.universe.ConstantType(Constant(_: String)) => Some(pq"${c.parse(sym.fullName)}")
        case _                                            => None
      }
  }

  object JavaEnumPattern extends SymPatternExtractor {
    override def doUnapply(sym: Symbol, typeInSealedParent: Type): Option[Tree] =
      if (sym.isJavaEnum) Some(pq"_: $sym") else None
  }

  object CaseClassPattern extends SymPatternExtractor {
    def doUnapply(sym: Symbol, typeInSealedParent: Type): Option[Tree] =
      if (sym.isCaseClass) Some(pq"_: $typeInSealedParent") else None
  }

  object CaseObjectPattern extends SymPatternExtractor {
    def doUnapply(sym: Symbol, typeInSealedParent: Type): Option[Tree] =
      if (sym.isModuleClass) Some(pq"_: $typeInSealedParent") else None
  }

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
