/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.lemonlabs.uri.Urn
import zio.ZIO
import zio.json.JsonCodec
import zio.stream.ZStream
import zio.test.*

import io.funkode.portfolio.model.Portfolio
import io.funkode.resource.model.StoreModelDerivationSpec.{suite, test}

trait ResourceExamples:

  val jsonResourceUrn = Urn.parse("urn:user:peter")
  val jsonResourceBodyStream = ZStream.fromIterable("""
      |{
      |  "name": "Peter",
      |  "age": 23
      |}
      |""".stripMargin.getBytes)
  val jsonResource: Resource = Resource.apply(jsonResourceUrn, jsonResourceBodyStream)

  case class Person(name: String, age: Int) derives JsonCodec
  val personResource: Resource.Of[Person] = Resource.Of(jsonResourceUrn, ZIO.succeed(Person("Peter", 23)))

object ResourceOfDerivationSpec extends ZIOSpecDefault with ResourceExamples:

  import Resource.fromRawResourceToTypedResource

  override def spec: Spec[TestEnvironment, Any] =
    suite("Resource should")(test("Create a domain resource from json") {
      val parsedResource = jsonResource.of[Person]
      for
        parsedBody <- parsedResource.body
        personBody <- personResource.body
      yield assertTrue(parsedResource.id == personResource.id) && assertTrue(parsedBody == personBody)
    })
