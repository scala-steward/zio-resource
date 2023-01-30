/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

import zio.*
import zio.schema.*
import zio.schema.Schema.Record
import zio.schema.meta.MetaSchema

import ops.any.*
import ops.string.*

case class ResourceModel(name: String, collections: Map[String, CollectionModel] = Map.empty)
case class CollectionModel(resourceType: String, rels: List[RelModel] = List.empty)
case class RelModel(rel: String, targetType: String, oneToMany: Boolean = false)

object ResourceModelDerivation:

  import CollectionModelDerivation.given
  import DerivationUtils.*

  transparent inline def decapitalize(inline str: String): String =
    inline str match
      case null | "" => str
      case nonEmpty  => s"${nonEmpty.head.toLower}${nonEmpty.tail}"

  inline given gen[R](using m: Mirror.Of[R]): ResourceModel =
    inline val resourceModelLabel = constValue[m.MirroredLabel]

    inline m match
      case _: Mirror.SumOf[R] =>
        error("ResourceModel derivation only supported for case classes, found: " + resourceModelLabel)
      case p: Mirror.ProductOf[R] =>
        val collectionLabels = deriveMirrorElementNames[p.MirroredElemLabels].map(_.toString)
        val collectionTypes = getTypeStringFromTuple[p.MirroredElemTypes]
        val collections: List[CollectionModel] = deriveCollections[p.MirroredElemTypes](collectionTypes)

        val collectionsMap = collectionLabels.zip(collections).toMap

        ResourceModel(decapitalize(resourceModelLabel), collectionsMap)

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

  /*
  given deriveMacro[R](p: Exp[Mirror.Product])(using t: Type[R])(using Quotes): Expr[ResourceModel] =
    import quotes.reflect.*

    Expr.summon[Mirror.Of[R]].get match
      case '{
            $m: Mirror.ProductOf[R] {
              type MirroredLabel = mirrorLabel
              type MirroredElemTypes = mirrorElemTypes
              type MirroredElemLabels = mirrorElemLabels
            }
          } =>
        val resourceModelLabel = decapitalize(Type.valueOfConstant[mirrorLabel].map(_.toString()).get)
        val elemLabels = Type.of[mirrorElemLabels]
        println(s"elemLabels: $elemLabels")

        val resourceModelName = Expr.apply(resourceModelLabel)

        val typeRepr = TypeRepr.of[R].toString

        val exp = '{ ResourceModel(${ resourceModelName }) }
        println("ResourceModel derivation: \n" + exp.show)

        exp

      case _ =>
        throw new NotImplementedError(
          s"Only case class supported for ResourceModel derivation: " + TypeTree.of[R].show
        )

   */
  /*
  inline given derived[R](using m: Mirror.Of[R]): ResourceModel =
    inline m match
      case _: Mirror.SumOf[R] =>
        error("Resource model derivation only supported for case classes")
      case p: Mirror.ProductOf[R] =>
        val mirrorElementsTypes = deriveMirrorElementTypes[m.MirroredElemTypes]

        val mirrorElementsNames = deriveMirrorElementNames[m.MirroredElemLabels].map(_.toString)
        println("Element names: " + mirrorElementsNames)

        val collections = mirrorElementsTypes.map(_.toString).map(name => CollectionModel(name))

        val collectionsWithRel = collections.map(collection => {
          val colElementNames = deriveMirrorElementNames[m.MirroredElemLabels].map(_.toString)
          println("Element names: " + mirrorElementsNames)
        })

        val collectionsMap = mirrorElementsNames.zip(collections).toMap
        println("collections: " + collections)

        ResourceModel(decapitalize(p.toString), collectionsMap)

  inline given deriveRelTypes[R](using m: Mirror.Of[R]): ResourceModel =
    inline m match
      case _: Mirror.SumOf[R] =>
        error("Resource model derivation only supported for case classes")
      case p: Mirror.ProductOf[R] =>

  inline def deriveMirrorElementNames[T <: Tuple]: List[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => typeName[t](using summonInline[ValueOf[t]]) :: deriveMirrorElementNames[ts]

  inline def typeName[R](using M: ValueOf[R]): R =
    valueOf[R]

  inline def deriveMirrorElementTypes[T <: Tuple]: List[Mirror] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Mirror.Of[t]] :: deriveMirrorElementTypes[ts]


  extension (typeId: TypeId)
    def fullName: String = typeId match
      case TypeId.Structural => TypeId.Structural.name
      case n: TypeId.Nominal => n.fullyQualified

  private def collectionFromSchema(colSchema: MetaSchema): CollectionModel =
    colSchema match
      case colMetaSchema: MetaSchema.Product =>
        val colType = colSchema.toSchema.schemaType

        val rels = colMetaSchema.fields
          .filter((_, schema) => schema.isList || schema.isProduct)
          .map((name, schema) => RelModel(name, schema.toSchema.schemaType))
        CollectionModel(colType, rels.toList)

      case other =>
        throw RuntimeException(
          "Collection model derivation not supported for current type: " + other
        )

  extension (metaSchema: MetaSchema)
    def isProduct: Boolean = metaSchema match
      case _: MetaSchema.Product => true
      case _                     => false
    def isList: Boolean = metaSchema match
      case _: MetaSchema.ListNode => true
      case _                      => false

  extension (schema: Schema[?])
    def schemaType: String = schema match
      case e: Schema.Enum[?]   => e.id.fullName
      case r: Schema.Record[?] => r.id.fullName
      case c: Schema.Collection[?, ?] =>
        c match
          case Schema.Sequence(elementSchema, _, _, _, _) =>
            s"Sequence(${elementSchema.schemaType})"
          case Schema.Map(keySchema, valueSchema, _) =>
            s"Map(${keySchema.schemaType}, ${valueSchema.schemaType}"
          case Schema.Set(elementSchema, _) =>
            s"Set(${elementSchema.schemaType})"

      case _: Schema.Transform[?, ?, ?] => "Transform"
      case p: Schema.Primitive[?]       => p.standardType.tag
      case o: Schema.Optional[?]        => s"Option(${o.schema})"
      case _: Schema.Fail[?]            => "Fail"
      case t: Schema.Tuple2[?, ?]       => s"Tuple2(${t.left.schemaType}, ${t.right.schemaType})"
      case e: Schema.Either[?, ?]       => s"Either(${e.left.schemaType}, ${e.right.schemaType})"
      case l: Schema.Lazy[?]            => s"Lazy${l.schema.schemaType}"
      case _: Schema.Dynamic            => "Dynamic"
   */
