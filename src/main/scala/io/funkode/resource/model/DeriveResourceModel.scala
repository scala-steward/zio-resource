/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.compiletime.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.string.*
import scala.deriving.Mirror
import scala.quoted.*

object DeriveResourceModel:

  import CollectionModelDerivation.given
  import DerivationUtils.*

  transparent inline def decapitalize(inline str: String): String =
    inline str match
      case null | "" => str
      case nonEmpty  => s"${nonEmpty.head.toLower}${nonEmpty.tail}"

  inline given gen[R](using m: Mirror.Of[R]): ResourceModel =
    inline val resourceModelLabel = constValue[m.MirroredLabel]

    inline m match
      case sum: Mirror.SumOf[R] =>
        val collectionLabels = deriveCollectionNames[sum.MirroredElemTypes]
        val collectionTypes = getTypeStringFromTuple[sum.MirroredElemTypes]

        val collections: List[CollectionModel] = deriveCollections[sum.MirroredElemTypes](collectionTypes)

        val collectionsMap = collectionLabels.zip(collections).toMap

        ResourceModel(decapitalize(resourceModelLabel), collectionsMap)
      case _: Mirror.ProductOf[R] =>
        error("ResourceModel derivation only supported for sealed traits, found: " + resourceModelLabel)

  inline def deriveCollectionNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val identifiable: Resource.Identifiable[t] = summonInline[Resource.Identifiable[t]]
        identifiable.nid +: deriveCollectionNames[ts]

  inline def deriveCollections[T <: Tuple](collectionTypes: List[String]): List[CollectionModel] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        CollectionModelDerivation.gen[t](collectionTypes)(using
          summonInline[Mirror.Of[t]]
        ) :: deriveCollections[ts](collectionTypes)

object CollectionModelDerivation:

  import DerivationUtils.*

  val CollectionPattern = """^.*\.collection\..*\[(.*)\]$""".r

  inline def gen[R](collectionTypes: List[String])(using m: Mirror.Of[R]): CollectionModel =
    inline m match
      case _: Mirror.SumOf[R] =>
        error(
          "CollectionModel derivation only supported for case classes, found: " + constValue[m.MirroredLabel]
        )
      case p: Mirror.ProductOf[R] =>
        val collectionType = getTypeString[R]

        val attributesNames = deriveMirrorElementNames[p.MirroredElemLabels].map(_.toString)
        val attributesTypes = getTypeStringFromTuple[p.MirroredElemTypes]

        val attributesNameType = attributesNames.zip(attributesTypes)

        val relAttributes = attributesNameType
          .map((name, relType) =>
            relType match
              case CollectionPattern(baseType) => (name, baseType, true)
              case _                           => (name, relType, false)
          )
          .map(RelModel.apply)
          .filter(rel => collectionTypes.contains(rel.targetType))

        CollectionModel(collectionType, relAttributes)
