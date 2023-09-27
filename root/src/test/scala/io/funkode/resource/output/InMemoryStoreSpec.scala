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
import ResourceStore.{ifNotFound, body}

trait CarsAndOwners:
  case class Car(vin: String, make: String, model: String) derives JsonCodec
  case class Owner(id: String, name: String) derives JsonCodec

  given Resource.Addressable[Car] = new Resource.Addressable[Car]:
    def resourceNid: String = "cars"

    def resourceNss(car: Car): String = car.vin

  given Resource.Addressable[Owner] = new Resource.Addressable[Owner]:
    def resourceNid: String = "owners"

    def resourceNss(r: Owner): String = r.id

  val carHonda = Car("123", "Honda", "Civic")
  val carMercedes = Car("1", "Mercedes", "GLE")
  val carFord = Car("2", "Ford", "Ecosport")

  val ownerRoger = Owner("abc", "Roger")

object InMemoryStoreSpec extends ZIOSpecDefault with CarsAndOwners:

  override def spec: Spec[TestEnvironment, Any] =
    suite("In memory resource store should")(
      test("Return not found error if resource doesn't exist") {
        for error <- ResourceStore.fetchOne(Urn("resource", "doesnotexist")).flip
        yield assertTrue(error == ResourceError.NotFoundError(Urn("resource", "doesnotexist")))
      },
      test("Store and fetch resource by urn") {
        for
          storedResource <- ResourceStore.save(Resource.fromAddressableClass(carHonda))
          storedCar <- storedResource.of[Car].body
          fetchedCar <- ResourceStore.fetchOneAs[Car](carHonda.urn).body
        yield assertTrue(storedCar == fetchedCar)
      },
      test("Link and fetch rel resources") {
        for
          storedCar1 <- ResourceStore.save(Resource.fromAddressableClass(carMercedes))
          storedCar2 <- ResourceStore.save(Resource.fromAddressableClass(carFord))
          storedOwner <- ResourceStore.save(Resource.fromAddressableClass(ownerRoger))
          _ <- ResourceStore.link(storedOwner.urn, "owns", storedCar1.urn)
          _ <- ResourceStore.link(storedCar1.urn, "ownedBy", storedOwner.urn)
          ownedCars1 <- ResourceStore.fetchOneRelAs[Car](storedOwner.urn, "owns").body
          carOwner <- ResourceStore.fetchOneRelAs[Owner](storedCar1.urn, "ownedBy").body
          _ <- ResourceStore.link(storedOwner.urn, "owns", storedCar2.urn)
          ownedCars2 <- ResourceStore.fetchRelAs[Car](storedOwner.urn, "owns").mapZIO(_.body).runCollect
          _ <- ResourceStore.delete(carMercedes.urn)
          ownedCars3 <- ResourceStore.fetchRelAs[Car](storedOwner.urn, "owns").mapZIO(_.body).runCollect
        yield assertTrue(ownedCars1 == carMercedes) &&
          assertTrue(carOwner == Owner("abc", "Roger")) &&
          assertTrue(ownedCars2 == Chunk(carMercedes, carFord)) &&
          assertTrue(ownedCars3 == Chunk(carFord))
      },
      test("Unlink resources") {
        for
          storedCar1 <- ResourceStore.save(Resource.fromAddressableClass(carMercedes))
          storedOwner <- ResourceStore.save(Resource.fromAddressableClass(ownerRoger))
          _ <- ResourceStore.link(storedOwner.urn, "owns", storedCar1.urn)
          ownedCars1 <- ResourceStore.fetchOneRelAs[Car](storedOwner.urn, "owns").body
          _ <- ResourceStore.unlink(storedOwner.urn, "owns", storedCar1.urn)
          errorAfterUnlink <- ResourceStore.fetchOneRelAs[Car](storedOwner.urn, "owns").flip
          fetchedCarAfterUnlink <- ResourceStore.fetchOneAs[Car](storedCar1.urn).body
        yield assertTrue(ownedCars1 == carMercedes) &&
          assertTrue(errorAfterUnlink == ResourceError.NotFoundError(storedOwner.urn)) &&
          assertTrue(fetchedCarAfterUnlink == carMercedes)

      },
      test("Call effect if not found") {
        for
          carIfNotFound <- ResourceStore
            .fetchOneAs[Car](Urn("cars", "that-not-exist"))
            .body
            .ifNotFound(e => ZIO.succeed(Car(e.urn.toString, "created", "when-not-found")))
          savedIfNotFound <- ResourceStore
            .fetchOneAs[Car](Urn("cars", "that-not-exist"))
            .saveIfNotFound(Car("saved", "when", "not-found"))
            .body
        yield assertTrue(carIfNotFound == Car("urn:cars:that-not-exist", "created", "when-not-found")) &&
          assertTrue(savedIfNotFound == Car("saved", "when", "not-found"))
      }
    ).provide(ResourceStore.inMemory)
