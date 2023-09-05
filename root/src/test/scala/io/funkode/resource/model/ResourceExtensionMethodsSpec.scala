/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.test.*

trait ResourceExamples:
  sealed trait Library

  case class Catalog(id: String, books: List[Book], owner: User) extends Library
  case class User(id: String, name: String, age: Int) extends Library derives JsonCodec
  case class Book(isbn: String, title: String, author: Option[User]) extends Library

  object Library:

    given Resource.Addressable[Catalog] with
      def resourceNid: String = "catalog"
      def resourceNss(catalog: Catalog): String = catalog.id

    given Resource.Addressable[User] with
      def resourceNid: String = "user"
      def resourceNss(user: User): String = user.id

    given Resource.Addressable[Book] with
      def resourceNid: String = "book"
      def resourceNss(book: Book): String = book.isbn

  val jsonResourceUrn = Urn.parse("urn:user:123")
  val jsonResourceBody = """
      |{
      |  "id": "123",
      |  "name": "Peter",
      |  "age": 23
      |}
      |""".stripMargin
  val jsonResource: Resource = Resource.fromJsonString(jsonResourceUrn, jsonResourceBody)

  val userResource: Resource.Of[User] = User("123", "Peter", 23).asResource

  val catalogUrn = Urn.parse("urn:catalog:mainCatalog")
  val authorUrn = Urn.parse("urn:author:miguel-cervantes")
  val bookUrn = Urn.parse("urn:book:9780744525021")

object ResourceOfDerivationSpec extends ZIOSpecDefault with ResourceExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Resource should")(
      test("Create a typed resource from json") {
        val parsedResource = jsonResource.of[User]
        for
          parsedBody <- jsonResource.of[User].body
          userBody <- userResource.body
        yield assertTrue(parsedResource.urn == userResource.urn) && assertTrue(parsedBody == userBody)
      }
    )
