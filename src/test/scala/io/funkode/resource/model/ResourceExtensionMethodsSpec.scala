/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZSink, ZStream}
import zio.test.*

import io.funkode.portfolio.model.Portfolio
import io.funkode.resource.model.StoreModelDerivationSpec.{suite, test}

trait ResourceExamples:
  sealed trait Library

  case class Catalog(id: String, books: List[Book], owner: User) extends Library
  case class User(id: String, name: String, age: Int) extends Library derives JsonCodec
  case class Book(isbn: String, title: String, author: Option[User]) extends Library

  object Library:

    given Resource.Typed[Catalog] with
      def resourceCollection: String = "catalog"
      def resourceId(catalog: Catalog): String = catalog.id
      def resourceWithId(catalog: Catalog)(newId: String): Catalog = catalog.copy(id = newId)

    given Resource.Typed[User] with
      def resourceCollection: String = "user"
      def resourceId(user: User): String = user.id
      def resourceWithId(user: User)(newId: String): User = user.copy(id = newId)

    given Resource.Typed[Book] with
      def resourceCollection: String = "book"
      def resourceId(book: Book): String = book.isbn
      def resourceWithId(book: Book)(newId: String): Book = book.copy(isbn = newId)

  val jsonResourceUrn = Urn.parse("urn:user:123")
  val jsonResourceBody = """
      |{
      |  "id": "123",
      |  "name": "Peter",
      |  "age": 23
      |}
      |""".stripMargin
  val jsonResource: Resource = Resource.fromString(jsonResourceUrn, jsonResourceBody)

  val personResource: Resource.Of[User] =
    Resource.fromTypedClass(User("123", "Peter", 23))

  val catalogUrn = Urn.parse("urn:catalog:mainCatalog")
  val authorUrn = Urn.parse("urn:author:miguel-cervantes")
  val bookUrn = Urn.parse("urn:book:9780744525021")

  val catalogJson: String =
    s"""
       |{
       |  "id": "mainCatalog",
       |  "books": [
       |    {
       |      "isbn": "9780744525021",
       |      "title": "El Quijote",
       |      "author": {
       |        "id": "cervantes",
       |        "name": "Cervantes",
       |        "age": 57
       |      }
       |    }, {
       |      "isbn": "8467033401",
       |      "title": "Lazarillo de Tormes"
       |    }
       |  ],
       |  "owner": {
       |    "id": "pete-har",
       |    "name": "Pete Har",
       |    "age": 27
       |  }
       |}
       |""".stripMargin

  val denormalizedCatalogResource = Resource.fromString(catalogUrn, catalogJson)

  val normalizedCatalogJson = """{ "id": "mainCatalog" }""".stripMargin
  val normalizedBook1Json = """{ "isbn": "9780744525021", "title": "El Quijote" }""".stripMargin
  val normalizedAuthorJson = """{ "id": "cervantes", "name": "Cervantes", "age": 57 }""".stripMargin
  val normalizedBook2Json = """{ "isbn": "8467033401", "title": "Lazarillo de Tormes" }""".stripMargin
  val normalizedOwnerJson = """{ "id": "pete-har", "name": "Pete Har", "age": 27 }""".stripMargin

  val expectedNormalizedJsons =
    Chunk(
      normalizedCatalogJson,
      normalizedBook1Json,
      normalizedAuthorJson,
      normalizedBook2Json,
      normalizedOwnerJson
    ).map(_.fromJson[Json].getOrElse(Json.Null))

object ResourceOfDerivationSpec extends ZIOSpecDefault with ResourceExamples:

  import Resource.fromRawResourceToTypedResource

  override def spec: Spec[TestEnvironment, Any] =
    suite("Resource should")(
      test("Create a domain resource from json with resource.of[R]") {
        val parsedResource = jsonResource.of[User]
        for
          parsedBody <- parsedResource.body
          personBody <- personResource.body
        yield assertTrue(parsedResource.id == personResource.id) && assertTrue(parsedBody == personBody)
      },
      test("Denormalize a resource (document) with resource.denormalize") {

        for normalizedResources <- denormalizedCatalogResource
            .normalizeWithModel[Library]
            .run(ZSink.collectAll)
        yield assertTrue(normalizedResources == expectedNormalizedJsons)
      }
    )
