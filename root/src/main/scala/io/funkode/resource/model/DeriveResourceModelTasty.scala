/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.quoted.*

object DeriveResourceModelTasty:

  import DerivationUtils.*

  inline given gen[T]: ResourceModel =
    ${ genImpl[T] }

  def genImpl[T](using t: Type[T])(using Quotes): Expr[ResourceModel] =
    import quotes.reflect.*

    given Printer[Tree] = Printer.TreeStructure

    def reportError(error: String) =
      quotes.reflect.report.errorAndAbort(error)

    def nameFromType(tpe: TypeRepr, name: String = ""): String =
      tpe match
        case atpe: AndType =>
          nameFromType(atpe.right, decapitalize(name + atpe.left.typeSymbol.name) + "And")
        case otpe: OrType =>
          nameFromType(otpe.right, decapitalize(name + otpe.left.typeSymbol.name) + "Or")
        case n: NamedType =>
          decapitalize(name + n.name)

    def relTargetTypeFromTpe(tpe: TypeRepr): String =
      tpe.typeSymbol.tree match
        case ValDef(name, tree, _) =>
          tree.tpe match
            case applied: AppliedType =>
              applied.args match
                case head :: Nil =>
                  head.typeSymbol.fullName
                case _ => reportError(s"Can't extract target type from" + applied.show)
            case n: NamedType => tree.tpe.typeSymbol.fullName
            case other        => reportError(s"Can't extract target type from tpe: " + other.show)
        case other => reportError(s"Can't extract target type from non ValDef: " + other.show)

    def relsFromType[T: Type](collectionTypes: List[String]): Expr[List[RelModel]] =

      val tpe = TypeRepr.of[T]
      val symbol = tpe.typeSymbol

      val rels = symbol.declaredFields
        .map(_.typeRef)
        .map(tRef =>
          val rel = tRef.name
          val targetType = relTargetTypeFromTpe(tRef)
          val oneToMany = tRef.baseClasses.map(_.fullName).contains("scala.collection.Iterable")

          RelModel(rel, targetType, oneToMany)
        )
        .filter(relModel => collectionTypes.contains(relModel.targetType))
        .map(relModel =>
          val rel = Expr(relModel.rel)
          val targetType = Expr(relModel.targetType)
          val oneToMany = Expr(relModel.oneToMany)

          '{ RelModel(${ rel }, ${ targetType }, ${ oneToMany }) }
        )

      Expr.ofList(rels)

    def collectionFromType[T: Type](collectionTypes: List[String] = List.empty): Expr[CollectionModel] =

      val symbol = TypeRepr.of[T].typeSymbol

      val fullName = Expr(symbol.fullName)
      val name = Expr.summon[Resource.Addressable[T]] match
        case Some(addressable) => '{ ${ addressable }.resourceNid }
        case None              => Expr(decapitalize(symbol.name))

      val rels = relsFromType[T](collectionTypes)

      '{ CollectionModel($name, $fullName, $rels) }

    def internalSymbolsFromTpe(tpe: TypeRepr, acum: List[Symbol] = List.empty): List[Symbol] =
      tpe match
        case a: AndType   => internalSymbolsFromTpe(a.right, acum :+ a.left.typeSymbol)
        case o: OrType    => internalSymbolsFromTpe(o.right, acum :+ o.left.typeSymbol)
        case _: NamedType => acum :+ tpe.typeSymbol

    def symbolsFromTpe(tpe: TypeRepr): List[Symbol] =
      tpe match
        case a: AndType => internalSymbolsFromTpe(a.right, List(a.left.typeSymbol))
        case o: OrType  => internalSymbolsFromTpe(o.right, List(o.left.typeSymbol))
        case _: NamedType =>
          tpe.typeSymbol.tree match
            case TypeDef(name, tree) =>
              tree match
                case applied: Applied => internalSymbolsFromTpe(applied.tpe)
                case other            => reportError(s"Can't extract symbols from type def: " + other.show)

            case cDef: ClassDef =>
              if cDef.symbol.children.nonEmpty then cDef.symbol.children
              else if cDef.symbol.declaredFields.nonEmpty then
                cDef.symbol.declaredFields
                  .map(_.tree)
                  .map:
                    case v: ValDef => v.tpt.tpe.typeSymbol
              else reportError(s"Can't extract collections from class def: " + cDef.show)

            case other => reportError(s"Can't extract symbols from " + other.show)

    val tpe = TypeRepr.of[T]
    val name = Expr(nameFromType(tpe))

    val collectionSymbols = symbolsFromTpe(tpe)
    val collectionTypes = collectionSymbols.map(_.fullName)
    val collectionModels = collectionSymbols.map(_.typeRef.asType match
      case '[t] => collectionFromType[t](collectionTypes)
    )

    val collectionsExpr = Expr.ofList(collectionModels)

    '{
      val collections = $collectionsExpr
      val collectionsMap = collections.map(c => (c.collectionName, c)).toMap
      ResourceModel($name, collectionsMap)
    }
