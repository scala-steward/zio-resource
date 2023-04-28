/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package output

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.test.*

import io.funkode.resource.model.*

object InMemoryStoreSpec extends ZIOSpecDefault:

  case class Car(vin: String, make: String, model: String) derives JsonCodec
  case class Owner(id: String, name: String) derives JsonCodec

  given Resource.Addressable[Car] = new Resource.Addressable[Car]:
    def resourceNid: String = "cars"
    def resourceNss(car: Car): String = car.vin

  given Resource.Addressable[Owner] = new Resource.Addressable[Owner]:
    def resourceNid: String = "owners"
    def resourceNss(r: Owner): String = r.id

  override def spec: Spec[TestEnvironment, Any] =
    suite("In memory resource store should")(
      test("Return not found error if resource doesn't exist") {
        for error <- ResourceStore.fetchOne(Urn("resource", "doesnotexist")).flip
        yield assertTrue(error == ResourceError.NotFoundError(Urn("resource", "doesnotexist")))
      },
      test("Store and fetch resource by urn") {
        for
          storedResource <- ResourceStore.save(Resource.fromAddressableClass(Car("123", "Honda", "Civic")))
          storedCar <- storedResource.of[Car].body
          fetchedResource <- ResourceStore.fetchOneAs[Car](Urn("cars", "123"))
          fetchedCar <- fetchedResource.body
        yield assertTrue(storedCar == fetchedCar)
      },
      test("Link and fetch rel resources") {
        for
          storedCar <- ResourceStore.save(Resource.fromAddressableClass(Car("53", "Mercedes", "GLE")))
          storedOwner <- ResourceStore.save(Resource.fromAddressableClass(Owner("abc", "Roger")))
          _ <- ResourceStore.link(storedOwner.urn, "owns", storedCar.urn)
          _ <- ResourceStore.link(storedCar.urn, "ownedBy", storedOwner.urn)
          ownedCars <- ResourceStore.fetchRelAs[Car](storedOwner.urn, "owns").mapZIO(_.body).runCollect
          carOwner <- ResourceStore.fetchOneRelAs[Owner](storedCar.urn, "ownedBy").flatMap(_.body)
        yield assertTrue(ownedCars == Chunk(Car("53", "Mercedes", "GLE"))) && assertTrue(
          carOwner == Owner("abc", "Roger")
        )
      }
    ).provideShared(ResourceStore.inMemory)
