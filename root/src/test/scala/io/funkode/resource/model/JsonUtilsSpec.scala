/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import zio.Chunk
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZSink, ZStream}
import zio.test.*

trait JsonExamples:

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

  val denormalizedJsonStream = ZStream.fromIterable(catalogJson.getBytes)

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

object JsonUtilsSpec extends ZIOSpecDefault with JsonExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("JsonUtils should")(
      test("Denormalize json") {
        for normalizedJsons <- JsonUtils
            .normalizeJsonStream(denormalizedJsonStream)
            .run(ZSink.collectAll)
        yield assertTrue(normalizedJsons == expectedNormalizedJsons)
      }
    )
