/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.compiletime.*
import scala.deriving.Mirror

object DeriveResourceModel:

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

        val nameForTypeMap = collectionTypes.zip(collectionLabels).toMap
        val collections: List[CollectionModel] = deriveCollections[sum.MirroredElemTypes](nameForTypeMap)

        val collectionsMap = collectionLabels.zip(collections).toMap
        ResourceModel(decapitalize(resourceModelLabel), collectionsMap)
      case _: Mirror.ProductOf[R] =>
        error("ResourceModel derivation only supported for sealed traits, found: " + resourceModelLabel)

  inline def deriveCollectionNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val addressable: Resource.Addressable[t] = summonInline[Resource.Addressable[t]]
        addressable.resourceNid +: deriveCollectionNames[ts]

  inline def deriveCollections[T <: Tuple](nameForTypeMap: Map[String, String]): List[CollectionModel] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        CollectionModelDerivation.gen[t](nameForTypeMap)(using
          summonInline[Mirror.Of[t]]
        ) :: deriveCollections[ts](nameForTypeMap)

object CollectionModelDerivation:

  import DerivationUtils.*

  val CollectionPattern = """^.*\.collection\..*\[(.*)\]$""".r

  inline def gen[R](nameForTypeMap: Map[String, String])(using m: Mirror.Of[R]): CollectionModel =
    inline m match
      case _: Mirror.SumOf[R] =>
        error(
          "CollectionModel derivation only supported for case classes, found: " + constValue[m.MirroredLabel]
        )
      case p: Mirror.ProductOf[R] =>
        val collectionType = getTypeString[R]

        val collectionName = nameForTypeMap.get(collectionType).get // this .get should never fail

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
          .filter(rel => nameForTypeMap.contains(rel.targetType))

        CollectionModel(collectionName, collectionType, relAttributes)
