/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package input
package adapter

import io.lemonlabs.uri.Urn
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

import io.funkode.resource.model.*
import io.funkode.resource.output.*

trait RestMockService:

  case class Customer(id: String, name: String) derives JsonCodec

  given Resource.Addressable[Customer] = new Resource.Addressable[Customer]:
    def resourceNid: String = "customers"
    def resourceNss(customer: Customer): String = customer.id

  val roger = Customer("123", "Roger")
  val customerJson =
    s"""
       |{
       |  "id": "123",
       |  "name": "Roger"
       |}
       |""".stripMargin

object RestResourceApiSpec extends ZIOSpecDefault with RestMockService:

  val app = RestResourceApi.app
    .provideSomeLayer(ResourceStore.inMemory)
    .provideSomeLayer(ResourceInputService.default)
    .runZIO

  extension (req: Request) def json: Request = req.addHeader("content-type", "application/json")

  override def spec: Spec[TestEnvironment, Any] =
    suite("Resource Rest API")(
      test("Get a Resource from HTTP GET request") {
        for
          resourceNotFound <- app(Request.get(URL(Root / "customers" / "123")).json).flip
          storedResource <- app(
            Request.put(Body.fromString(customerJson), URL(Root / "customers" / "123")).json
          )
          storedCustomer <- storedResource.of[Customer].body
          fetchedResource <- app(Request.get(URL(Root / "customers" / "123")).json)
          fetchedCustomer <- fetchedResource.of[Customer].body
        yield assertTrue(resourceNotFound == Some(ResourceError.NotFoundError(Urn("customers", "123")))) &&
          assertTrue(storedResource.urn == Urn("customers", "123")) &&
          assertTrue(storedCustomer == roger) &&
          assertTrue(fetchedResource.urn == Urn("customers", "123")) &&
          assertTrue(fetchedCustomer == roger)
      }
    ).provideShared(ResourceStore.inMemory)
